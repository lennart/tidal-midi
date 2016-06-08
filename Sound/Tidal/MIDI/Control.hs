module Sound.Tidal.MIDI.Control where

import qualified Sound.Tidal.Stream as S
import Sound.Tidal.Tempo(Tempo(Tempo, cps))
import qualified Data.Map.Strict as Map
import Data.Ratio
import Sound.Tidal.Params

type RangeMapFunc = (Int, Int) -> Double -> Int

data ControlChange =
  CC { param :: S.Param,
       midi :: Int,
       range :: (Int, Int),
       vdefault :: Double,
       scalef :: RangeMapFunc
     }
  | NRPN { param :: S.Param,
           midi :: Int,
           range :: (Int, Int),
           vdefault :: Double,
           scalef :: RangeMapFunc
         }
  | SysEx { param :: S.Param,
            midi :: Int,
            range :: (Int, Int),
            vdefault :: Double,
            scalef :: RangeMapFunc
          }

data ControllerShape = ControllerShape {
  controls :: [ControlChange],
  latency :: Double
  }

midiShape = S.Shape {
  S.params = [     
     dur_p,
     n_p,
     nudge_p,
     velocity_p,     
     unit_p
     ],
  S.latency = 0,
  S.cpsStamp = False
  }

computeTiming :: Tempo -> Double -> Ratio Integer -> S.ParamMap -> ((Int,Int,Ratio Integer), Double)
computeTiming tempo on dur note = ((n', v', d'), nudge')
  where
    note' = Map.mapMaybe (id) $ note
    unit = S.svalue $ note' Map.! unit_p    
    v' = mapRange (0, 127) $ S.fvalue $ note' Map.! velocity_p
    n' = S.ivalue $  note' Map.! n_p
    d' = case unit of
      "rate" -> realToFrac $ S.fvalue $ note' Map.! dur_p
      "cycle" -> (/) dur $ realToFrac $ cps tempo

    nudge' = S.fvalue $ note' Map.! nudge_p
    
toShape :: ControllerShape -> S.Shape
toShape cs = S.Shape {
  S.params = toParams cs,
  S.cpsStamp = False,
  S.latency = latency cs
  }

passThru :: (Int, Int) -> Double -> Int
passThru (_, _) = floor -- no sanitizing of range…

mapRange :: (Int, Int) -> Double -> Int
mapRange (low, high) = floor . (+ (fromIntegral low)) . (* ratio)
  where ratio = fromIntegral $ high - low

mCC :: S.Param -> Int -> ControlChange
mCC p m = CC {param=p, midi=m, range=(0, 127), vdefault=0, scalef=mapRange }

mNRPN :: S.Param -> Int -> ControlChange
mNRPN p m = NRPN {param=p, midi=m, range=(0, 127), vdefault=0, scalef=mapRange }

mrNRPN :: S.Param -> Int -> (Int, Int) -> Double -> ControlChange
mrNRPN p m r d = NRPN {param=p, midi=m, range=r, vdefault=d, scalef=mapRange }

toParams :: ControllerShape -> [S.Param]
toParams shape = map param (controls shape)

ctrlN :: Num b => ControllerShape -> S.Param -> Maybe b
ctrlN shape x = fmap fromIntegral $ fmap midi (paramN shape x)

paramN :: ControllerShape -> S.Param -> Maybe ControlChange
paramN shape x
  | x `elem` names = paramX $ matching p
  | otherwise = Nothing -- error $ "No such Controller param: " ++ show x
  where names = toParams shape
        paramX [] = Nothing
        paramX (h:_) = Just h
        matching = filter ((== x) . param)
        p = controls shape
