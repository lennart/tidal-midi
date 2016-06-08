module Sound.Tidal.MIDI.Stream (midiStream, midiBackend, midiState, midiSetters, midiDevices,store,cutShape) where

import           Control.Monad
-- generics
import qualified Data.Map as Map
import           Data.List (sortBy, find, partition)
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Data.Bits
import           Foreign.C
import           Control.Applicative

import           Numeric

-- Tidal specific
import           Sound.Tidal.Tempo (Tempo(Tempo), cps, clockedTick)
import           Sound.Tidal.Stream as S
import           Sound.Tidal.Utils
import           Sound.Tidal.Time
import           Sound.Tidal.Pattern (arc)

import           Data.Ratio ((%),Ratio)
import           Sound.Tidal.Transition (transition)

-- MIDI specific
import           Sound.Tidal.MIDI.Device
import           Sound.Tidal.MIDI.Control
import qualified Sound.PortMidi as PM


type ConnectionCount = Int
type TickedConnectionCount = Int
type OutputOnline = Bool
type OutputState = (TickedConnectionCount, ConnectionCount, [ParamMap], OutputOnline)
type MIDITime = (Tempo, Int, Double, Double)
type MIDIEvent = (MIDITime, MIDIMessage)
type MIDIChannel = CLong
type MIDIStatus = CLong
type MIDIDatum = CLong
type MIDIMessage = (MIDIChannel, MIDIStatus, MIDIDatum, MIDIDatum)
type TimedNote = (CLong, CLong, Ratio Integer)


data Output = Output {
  cshape :: ControllerShape,
  conn :: PM.PMStream,
  buffer :: MVar ([ParamMap], [MIDIEvent]),
  bufferstate :: MVar OutputState
  }

type MidiMap = Map.Map S.Param (Maybe Int)
type MidiDeviceMap = Map.Map String Output


toMidiValue :: ControllerShape -> S.Param -> Value -> Maybe Int
toMidiValue s p (VF x) = ($) <$> mscale <*> mrange <*> pure x
    where
      mrange = fmap range mcc
      mscale = fmap scalef mcc
      mcc = paramN s p
toMidiValue _ _ (VI x) = Just x
toMidiValue _ _ (VS _) = Nothing -- ignore strings for now, we might 'read' them later

toMidiMap :: ControllerShape -> S.ParamMap -> MidiMap
toMidiMap s m = Map.mapWithKey (toMidiValue s) (Map.mapMaybe id m)


cutShape :: S.Shape -> ParamMap -> Maybe ParamMap
cutShape s m = flip Map.intersection (S.defaultMap s) <$> S.applyShape' s m

stripDefaults :: Maybe ParamMap -> Maybe ParamMap
stripDefaults m = Map.filterWithKey (\k v -> v /= defaultValue k) <$> m

store :: Output -> Int -> Tempo -> Int -> Double -> Double -> MidiMap -> ParamMap -> IO ()
store s ch change tick on off ctrls note = storemidi s ch' note' (change, tick, on, offset) ctrls
    where
      (note', nudge) = computeTiming' change on off note
      ch' = fromIntegral ch
      cshape' = cshape s
      offset = Sound.Tidal.MIDI.Control.latency cshape' + nudge

mkStore cshape channel s = return $ \ shape change tick (on,off,m) -> do
                        let ctrls = cutShape shape m
                            props = cutShape midiShape m
                            ctrls' = stripDefaults ctrls
                            ctrls'' = toMidiMap cshape <$> ctrls'
                            store' = store s channel change tick on off <$> ctrls''
                        -- store ParamMap with _all_ possible params (notes might not send CCs but need to reset some)

                        fmap ($) (storeParams s channel <$> stripDefaults (applyShape' shape m)) <*> (($) <$> store' <*> props)


-- this will union the currently stored paramstate with the given one
storeParams :: Output -> Int -> ParamMap -> IO () -> IO ()
storeParams o ch m action = do
  modifyMVar_ (buffer o) $ \(states, events) -> do
    let (before,current:after) = splitAt (ch - 1) states
        state' = Map.union m current
        states' = before ++ [state'] ++ after
    return (states', events)
  action

computeTiming' :: Tempo -> Double -> Double -> ParamMap -> (TimedNote, Double)
computeTiming' tempo on off note = ((fromIntegral n, fromIntegral v, d), nudge)
  where
    ((n,v,d), nudge) = computeTiming tempo on (realToFrac (off - on) / S.ticksPerCycle) note


connected cshape channel name s = do
  let shape = toShape cshape
      defaultParams = S.defaultMap shape
      allctrls = toMidiMap cshape defaultParams
  putStrLn ("Successfully initialized Device '" ++ name ++ "'")
  changeState goOnline s
  now <- getCurrentTime
  storeevents s $ makectrls s (fromIntegral channel) (Tempo now 0 1 False 0,0,0,0) allctrls -- send all ctrls defaults to specific channel of device
  mkStore cshape channel s

failed di err = error (show err ++ ": " ++ show di)

notfound name = do
  putStrLn "List of Available Device Names"
  putStrLn =<< displayOutputDevices
  error ("Device '" ++ show name ++ "' not found")

readState f o = do
  s <- readMVar $ bufferstate o
  let fs = f s
      (ticked, conns, paramstate, online) = s
  return fs

isCycling (0, conns, _, True) = True
isCycling _ = False

displayState (ticked, conns, paramstate, online) = show ticked ++ "/" ++ show conns ++ "[" ++ show online ++ "]" ++ " active params: " ++ show paramstate

changeState f o = do
  bs <- takeMVar stateM
  let fs = f bs
      (ticked, conns, paramstate, online) = fs
  putMVar stateM fs
    where
      stateM = bufferstate o

changeState' f o = do
  bs <- takeMVar stateM
  let fs = f bs
  putMVar stateM fs
  return (fs, bs)
    where
      stateM = bufferstate o

resetParamStates newstates (ticked, conns, paramstates, online) = (ticked, conns, zipWith resetParamState newstates paramstates, online)

resetParamState newstate currentstate
  | Map.empty == newstate = currentstate -- updating with an empty state is a noop
  | otherwise = newstate

goOnline (ticked, conns, paramstate, online) = (ticked, conns, paramstate, True)
addConnection (ticked, conns, paramstate, online) = (ticked, conns + 1, paramstate, online)
tickConnections (ticked, conns, paramstate, online) = ((ticked + 1) `mod` conns, conns, paramstate, online)

useOutput outsM name lat cshape = do
  outs <- readMVar outsM -- blocks
  let outM = Map.lookup name outs -- maybe
  -- if we have a valid output by now, return
  case outM of
    Just o -> do
      putStrLn "Cached Device Output"
      changeState addConnection o -- blocks
      return $ Just o
    Nothing -> do
      -- otherwise open a new output and store the result in the mvar
      devidM <- (>>= maybe (failed name "Failed opening MIDI Output Device ID") return) (getIDForDeviceName name)
      econn <- outputDevice devidM lat cshape  -- either
      case econn of
        Left o -> do
          changeState addConnection o
          swapMVar outsM $ Map.insert name o outs
          return $ Just o
        Right _ -> return Nothing

makeConnection :: MVar MidiDeviceMap -> String -> Int -> ControllerShape -> IO (S.ToMessageFunc, Output)
makeConnection devicesM deviceName channel cshape = do
  moutput <- useOutput devicesM deviceName 1 cshape
  case moutput of
    Just o -> do
      s <- connected cshape channel deviceName o
      return (s, o)
    Nothing ->
      --failed o
      error "Failed"

showLate :: (CULong, Double, PM.PMEvent, CULong, UTCTime) -> String
showLate (o, t, e, m, n) =
  unwords ["late",
           show $ (\x -> [PM.status x, PM.data1 x, PM.data2 x]) $ PM.decodeMsg $ PM.message e,
           "midi now ", show m, " midi onset: ", show o,
           "onset (relative): ", show $ showFFloat (Just 3) (t - realToFrac (utcTimeToPOSIXSeconds n)) "",
           ", sched: ", show $ PM.timestamp e]

-- should only send out events if all connections have ticked
flushBackend :: Output -> S.Shape -> Tempo -> Int -> IO ()
flushBackend o shape change ticks = do
  changeState tickConnections o
  cycling <- readState isCycling o

  Control.Monad.when cycling (do
      -- gather last sent params, update state with new
      let bs = bufferstate o
          buf = buffer o
          lat = S.latency shape
      (states, events) <- takeMVar buf

      ((_,_,newstates,_), (_,_,oldstates,_)) <- changeState' (resetParamStates states) o

      -- find params that were removed
      let mapDefaults = Map.mapWithKey (\k v -> defaultValue k)
          diffs = map mapDefaults $ zipWith Map.difference oldstates newstates
      -- store additional "reset" events in buffer
      -- schedule time must be exactly before/ontime with the next regular event to be sent. otherwise we risk
      -- mixing order of ctrl messages, and resets get override
      -- FIXME: when schedulin, note late CC messages and DROP THEM, otherwise everything is screwed
      let offset = S.latency shape
          mididiffs = map (toMidiMap (cshape o)) diffs
          resetevents = concat $ zipWith (\x y -> makectrls o x (change,ticks,1,offset) y) [1..] mididiffs

      maybe (pure ()) putStrLn (case length resetevents of
                                   0 -> Nothing
                                   _ -> Just $ show resetevents)
      -- send out MIDI events
      (late, later) <- sendevents o shape change ticks events resetevents
      -- finally clear buffered ParamMap for next tick
      putMVar buf (replicate 16 Map.empty, later)
      let len = length late
      case len of
        0 ->
          return ()
        _ -> do
          putStrLn $ showLate $ head late
          putStrLn $ "and " ++ show (len - 1) ++ " more")



midiDevices :: IO (MVar MidiDeviceMap)
midiDevices = newMVar $ Map.fromList []

midiBackend d n c cs = do
  (s, o) <- makeConnection d n c cs
  return $ Backend s (flushBackend o)

midiStream d n c s = do
  backend <- midiBackend d n c s
  stream backend (toShape s)

midiState d n c s = do
  backend <- midiBackend d n c s
  S.state backend (toShape s)

midiSetters :: MVar MidiDeviceMap -> String -> Int -> ControllerShape -> IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
midiSetters d n c s getNow = do
  ds <- midiState d n c s
  return (setter ds, transition getNow ds)


toDescriptor midiTime now (o,m,t,e) = (o,t,e, midiTime, now)

calcOnsets (a@(tempo, tick, onset, offset), e) = (a, logicalOnset' tempo tick onset offset, e)

showEvent :: PM.PMEvent -> String
showEvent e = show t ++ " " ++ show msg
  where msg = PM.decodeMsg $ PM.message e
        t = PM.timestamp e

showRawEvent :: (CULong, MIDITime, Double, PM.PMEvent) -> String
showRawEvent (midionset, (tempo,tick,onset,offset), logicalOnset, e) = "(" ++ show onset ++ "," ++ show offset ++ ") / " ++ show logicalOnset ++ " " ++  showEvent e

sendevents :: Output -> S.Shape -> Tempo -> Int -> [MIDIEvent] -> [MIDIEvent] -> IO ([(CULong, Double, PM.PMEvent, CULong, UTCTime)], [MIDIEvent])
sendevents stream shape change ticks [] [] = return ([],[])
sendevents stream shape change ticks evts resets = do
  -- assumptions:
  -- all reset events have the same timestamp
  -- questions:      
  -- could there be any events in `evts` at all that need reset? or are these just in late from the last tick?
  let output = conn stream
  midiTime <- PM.time
  now <- getCurrentTime
  let offset = S.latency shape
      nextTick = logicalOnset' change (ticks+1) 0 offset
      onsets = map calcOnsets evts
      -- calculate temporary scheduling for resetevts
      resetevts = map calcOnsets resets
      -- split into events sent now and later (e.g. a noteOff that would otherwise cut off noteOn's in the next tick)
      (evts', later) = span ((< nextTick).(\(_,o,_) -> o)) $ sortBy (comparing (\(_,o,_) -> o)) onsets
      -- calculate MIDI time to schedule events, putting time into fn to create PM.PMEvents
      evts'' = map (\(t, o, e) -> let midionset = scheduleTime midiTime now o
                                  in (midionset, t, o, makeRawEvent e midionset)) evts'
      -- a list CC `names` that need to be reset
      resetccs = map (\(t, o, (ch, st, d1, d2)) -> d1) resetevts
      later' = map (\(t,o,e) -> (t,e)) later
      evts''' = map (\(_,_,_,e) -> e) evts''
      findCC match list = find (\(t, o, (ch, st, d1, d2)) -> st == 0xB0 && (d1 `elem` match)) $ reverse list


      -- 1. find the ccs that needs reset (search in `later` then in `evts`)
      (evtstosend, laterevts) = case findCC resetccs later of
        Nothing -> case findCC resetccs evts' of
          -- 1c. no events at all need to be reset
          --      1cI. use the default passed in midionset for resets
          --      1cII. append `resets` to `evts` FIXME: make sure we really do by timing
          --      1cIII. send `evts`
          Nothing -> (evts'' ++ map (\(t, o, e) -> let midionset = scheduleTime midiTime now o
                                                   in (midionset, t, o, makeRawEvent e midionset)) resetevts, later')
          -- 1b. only `evts` contain a CC to be reset
          --      1bI. set scheduletime for reset __after__ the latest CC that needs to be reset in `evts`
          --      1bII. add `resets` to `evts`
          --      1bIII. send `evts`                
          Just l@(lt, latestO, le) -> (before ++
                                      map (
                                          \(t, o, e) ->
                                          let midionset = scheduleTime midiTime now latestO
                                          in (midionset, t,o,makeRawEvent e midionset)
                                          ) resetevts ++ after, later')
            where
              (before, after) = partition (\(m,t,o,e) -> m > scheduleTime midiTime now o) evts''

        -- 1a. `later` contains a cc to be reset, (omit searching in evts)
        --      1aI. set scheduletime for reset __after__ the latest CC that needs to be reset in `later`
        --      1aII. add `resetevts` to `later`
        --      1aIII. send `evts`                
        Just (latestT, _, _) -> (evts'', later' ++ map (\(t, _, e) -> (latestT, e)) resetevts)
      evtstosend' = map (\(_,_,_,e) -> e) evtstosend
      -- filter events that are too late
      late = map (toDescriptor midiTime now) $ filter (\(_,_,t,_) -> t < realToFrac (utcTimeToPOSIXSeconds now)) evtstosend

  -- write events for this tick to stream
  
  err <- PM.writeEvents output evtstosend'
  putStrLn $ unlines $ zipWith (++) (replicate (length evtstosend) (show midiTime ++ " " ++ show (realToFrac (utcTimeToPOSIXSeconds now)))) (map showRawEvent evtstosend)
  case err of
   PM.NoError -> return (late, laterevts)  -- return events for logging in outer scope
   e -> do
     putStrLn "sending failed"
     return (late, laterevts)


scheduleTime :: CULong -> UTCTime -> Double -> CULong
scheduleTime mnow' now' logicalOnset = t
  where
    now = realToFrac $ utcTimeToPOSIXSeconds now'
    mnow = fromIntegral mnow' -- FIXME: miditime is not as precise as _real time_, therefore the computation below will be inaccurate if mnow might be...Idon't know but somehow this calculation is not based on correct values, whereas now should be trusted, logicalOnset is correct so only mnow can be wrong,right?
    t = floor $ mnow + (1000 * (logicalOnset - now)) -- 1 second are 1000 microseconds as is the unit of timestamps in PortMidi


-- Event creation

-- FIXME: throws if param cannot be found
makectrls :: Output -> CLong -> MIDITime -> MidiMap -> [MIDIEvent]
makectrls o ch t ctrls = concatMap (\(param, ctrl) -> makeCtrl ch (fromJust $ paramN shape param) (fromIntegral ctrl) t) ctrls'
  where
    shape = cshape o
    ctrls' = filter ((>=0) . snd) $ Map.toList $ Map.mapMaybe id ctrls

makenote :: CLong -> TimedNote -> MIDITime -> [MIDIEvent]
makenote ch (note,vel,dur) (tempo,tick,onset,offset) = noteon' ++ noteoff'
  where
    noteon' = noteOn ch midinote vel (tempo,tick,onset,offset)
    noteoff' = noteOff ch midinote (tempo,tick,onset,offset + fromRational dur)
    midinote = note + 60

makemidi :: Output -> CLong -> TimedNote -> MIDITime -> MidiMap -> [MIDIEvent]
makemidi o ch (128,vel,dur) t ctrls = makectrls o ch t ctrls -- HACK: to send only CC use (n + 60) == 128
makemidi o ch note t ctrls = makectrls o ch t ctrls ++ makenote ch note t

-- Event buffering
storemidi :: Output -> CLong -> TimedNote -> MIDITime -> MidiMap -> IO ()
storemidi o ch n t ctrls = do
  storeevents o $ makemidi o ch n t ctrls
  return ()


-- MIDI Event wrapping
makeEvent :: CLong -> CLong -> CLong -> CLong -> MIDITime -> MIDIEvent
makeEvent st n ch v t = (t, msg)
  where
    msg = (ch, st, n, v)

makeRawEvent :: MIDIMessage -> CULong -> PM.PMEvent
makeRawEvent (ch, st, n, v) = PM.PMEvent msg
  where msg = PM.encodeMsg $ PM.PMMsg (encodeChannel ch st) n v

storeevents :: Output -> [MIDIEvent] -> IO (Maybe a)
storeevents o evts = do
  let buf = buffer o
  (paramstate, cbuf) <- takeMVar buf
--  putStrLn $ unlines $ map show evts
  putMVar buf (paramstate, cbuf ++ evts)
  return Nothing


-- MIDI Utils
encodeChannel :: (Bits a, Num a) => a -> a -> a
encodeChannel ch cc = (-) ch 1 .|. cc


-- MIDI Messages
noteOn :: CLong -> CLong -> CLong -> MIDITime -> [MIDIEvent]
noteOn ch val vel t = [makeEvent 0x90 val ch vel t]

noteOff :: CLong -> CLong -> MIDITime -> [MIDIEvent]
noteOff ch val t = [makeEvent 0x80 val ch 60 t]

makeCtrl :: CLong -> ControlChange -> CLong -> MIDITime -> [MIDIEvent]
makeCtrl ch CC {midi=midi, range=range} n t = makeCC ch (fromIntegral midi) n t
makeCtrl ch NRPN {midi=midi, range=range} n t = makeNRPN ch (fromIntegral midi) n t

makeCC :: CLong -> CLong -> CLong -> MIDITime -> [MIDIEvent]
makeCC ch c n t = [makeEvent 0xB0 c ch n t]

makeNRPN :: CLong -> CLong -> CLong -> MIDITime -> [MIDIEvent]
makeNRPN ch c n t = [
  nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t,
  nrpn 0x62 ch (c .&. 0x7F) t,
  nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t,
  nrpn 0x26 ch (n .&. 0x7F) t
  ]
  where
    nrpn = makeEvent 0xB0


-- Port Midi Device Wrapper
outputDevice :: PM.DeviceID -> Int -> ControllerShape -> IO (Either Output PM.PMError)
outputDevice deviceID latency shape = do
  PM.initialize
  now <- getCurrentTime
  result <- PM.openOutput deviceID latency
  bs <- newMVar (0, 0, replicate 16 Map.empty, False)
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
        b <- newMVar (replicate 16 Map.empty, [])

        return (Left Output { cshape=shape, conn=dev, buffer=b, bufferstate=bs })
    Right err -> return (Right err)

