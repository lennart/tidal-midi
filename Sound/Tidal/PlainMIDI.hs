module Sound.Tidal.PlainMIDI where

import Sound.Tidal.Stream (makeI, makeF)

import Sound.Tidal.MIDI.Control

keys :: ControllerShape
keys = ControllerShape {
  params = [
    mCC "cc0" 0,
    mCC "cc1" 1, mCC "cc2" 2, mCC "cc3" 3, mCC "cc4" 4, mCC "cc5" 5, mCC "cc6" 6,
    mCC "cc7" 7, mCC "cc8" 8, mCC "cc9" 9, mCC "cc10" 10, mCC "cc11" 11,
    mCC "cc12" 12, mCC "cc13" 13, mCC "cc14" 14, mCC "cc15" 15, mCC "cc16" 16,
    mCC "cc17" 17, mCC "cc18" 18, mCC "cc19" 19, mCC "cc20" 20, mCC "cc21" 21,
    mCC "cc22" 22, mCC "cc23" 23, mCC "cc24" 24, mCC "cc25" 25, mCC "cc26" 26,
    mCC "cc27" 27, mCC "cc28" 28, mCC "cc29" 29, mCC "cc30" 30, mCC "cc31" 31,
    mCC "cc32" 32, mCC "cc33" 33, mCC "cc34" 34, mCC "cc35" 35, mCC "cc36" 36,
    mCC "cc37" 37, mCC "cc38" 38, mCC "cc39" 39, mCC "cc40" 40, mCC "cc41" 41,
    mCC "cc42" 42, mCC "cc43" 43, mCC "cc44" 44, mCC "cc45" 45, mCC "cc46" 46,
    mCC "cc47" 47, mCC "cc48" 48, mCC "cc49" 49, mCC "cc50" 50, mCC "cc51" 51,
    mCC "cc52" 52, mCC "cc53" 53, mCC "cc54" 54, mCC "cc55" 55, mCC "cc56" 56,
    mCC "cc57" 57, mCC "cc58" 58, mCC "cc59" 59, mCC "cc60" 60, mCC "cc61" 61,
    mCC "cc62" 62, mCC "cc63" 63, mCC "cc64" 64, mCC "cc65" 65, mCC "cc66" 66,
    mCC "cc67" 67, mCC "cc68" 68, mCC "cc69" 69, mCC "cc70" 70, mCC "cc71" 71,
    mCC "cc72" 72, mCC "cc73" 73, mCC "cc74" 74, mCC "cc75" 75, mCC "cc76" 76,
    mCC "cc77" 77, mCC "cc78" 78, mCC "cc79" 79, mCC "cc80" 80, mCC "cc81" 81,
    mCC "cc82" 82, mCC "cc83" 83, mCC "cc84" 84, mCC "cc85" 85, mCC "cc86" 86,
    mCC "cc87" 87, mCC "cc88" 88, mCC "cc89" 89, mCC "cc90" 90, mCC "cc91" 91,
    mCC "cc92" 92, mCC "cc93" 93, mCC "cc94" 94, mCC "cc95" 95, mCC "cc96" 96,
    mCC "cc97" 97, mCC "cc98" 98, mCC "cc99" 99, mCC "cc100" 100, mCC "cc101" 101,
    mCC "cc102" 102, mCC "cc103" 103, mCC "cc104" 104, mCC "cc105" 105,
    mCC "cc106" 106, mCC "cc107" 107, mCC "cc108" 108, mCC "cc109" 109,
    mCC "cc110" 110, mCC "cc111" 111, mCC "cc112" 112, mCC "cc113" 113,
    mCC "cc114" 114, mCC "cc115" 115, mCC "cc116" 116, mCC "cc117" 117,
    mCC "cc118" 118, mCC "cc119" 119, mCC "cc120" 120, mCC "cc121" 121,
    mCC "cc122" 122, mCC "cc123" 123, mCC "cc124" 124, mCC "cc125" 125,
    mCC "cc126" 126, mCC "cc127" 127
  ],
  duration = ("dur", 0.05),
  velocity = ("vel", 0.5),
  latency = 0.1
}

oscKeys = toOscShape keys

-- note on/off
note         = makeI oscKeys "note"
dur          = makeF oscKeys "dur"

-- CC values
cc0 = makeF oscKeys "cc0";
cc1 = makeF oscKeys "cc1"; cc2 = makeF oscKeys "cc2"; cc3 = makeF oscKeys "cc3"
cc4 = makeF oscKeys "cc4"; cc5 = makeF oscKeys "cc5"; cc6 = makeF oscKeys "cc6"
cc7 = makeF oscKeys "cc7"; cc8 = makeF oscKeys "cc8"; cc9 = makeF oscKeys "cc9"
cc10 = makeF oscKeys "cc10"; cc11 = makeF oscKeys "cc11"; cc12 = makeF oscKeys "cc12"
cc13 = makeF oscKeys "cc13"; cc14 = makeF oscKeys "cc14"; cc15 = makeF oscKeys "cc15"
cc16 = makeF oscKeys "cc16"; cc17 = makeF oscKeys "cc17"; cc18 = makeF oscKeys "cc18"
cc19 = makeF oscKeys "cc19"; cc20 = makeF oscKeys "cc20"; cc21 = makeF oscKeys "cc21"
cc22 = makeF oscKeys "cc22"; cc23 = makeF oscKeys "cc23"; cc24 = makeF oscKeys "cc24"
cc25 = makeF oscKeys "cc25"; cc26 = makeF oscKeys "cc26"; cc27 = makeF oscKeys "cc27"
cc28 = makeF oscKeys "cc28"; cc29 = makeF oscKeys "cc29"; cc30 = makeF oscKeys "cc30"
cc31 = makeF oscKeys "cc31"; cc32 = makeF oscKeys "cc32"; cc33 = makeF oscKeys "cc33"
cc34 = makeF oscKeys "cc34"; cc35 = makeF oscKeys "cc35"; cc36 = makeF oscKeys "cc36"
cc37 = makeF oscKeys "cc37"; cc38 = makeF oscKeys "cc38"; cc39 = makeF oscKeys "cc39"
cc40 = makeF oscKeys "cc40"; cc41 = makeF oscKeys "cc41"; cc42 = makeF oscKeys "cc42"
cc43 = makeF oscKeys "cc43"; cc44 = makeF oscKeys "cc44"; cc45 = makeF oscKeys "cc45"
cc46 = makeF oscKeys "cc46"; cc47 = makeF oscKeys "cc47"; cc48 = makeF oscKeys "cc48"
cc49 = makeF oscKeys "cc49"; cc50 = makeF oscKeys "cc50"; cc51 = makeF oscKeys "cc51"
cc52 = makeF oscKeys "cc52"; cc53 = makeF oscKeys "cc53"; cc54 = makeF oscKeys "cc54"
cc55 = makeF oscKeys "cc55"; cc56 = makeF oscKeys "cc56"; cc57 = makeF oscKeys "cc57"
cc58 = makeF oscKeys "cc58"; cc59 = makeF oscKeys "cc59"; cc60 = makeF oscKeys "cc60"
cc61 = makeF oscKeys "cc61"; cc62 = makeF oscKeys "cc62"; cc63 = makeF oscKeys "cc63"
cc64 = makeF oscKeys "cc64"; cc65 = makeF oscKeys "cc65"; cc66 = makeF oscKeys "cc66"
cc67 = makeF oscKeys "cc67"; cc68 = makeF oscKeys "cc68"; cc69 = makeF oscKeys "cc69"
cc70 = makeF oscKeys "cc70"; cc71 = makeF oscKeys "cc71"; cc72 = makeF oscKeys "cc72"
cc73 = makeF oscKeys "cc73"; cc74 = makeF oscKeys "cc74"; cc75 = makeF oscKeys "cc75"
cc76 = makeF oscKeys "cc76"; cc77 = makeF oscKeys "cc77"; cc78 = makeF oscKeys "cc78"
cc79 = makeF oscKeys "cc79"; cc80 = makeF oscKeys "cc80"; cc81 = makeF oscKeys "cc81"
cc82 = makeF oscKeys "cc82"; cc83 = makeF oscKeys "cc83"; cc84 = makeF oscKeys "cc84"
cc85 = makeF oscKeys "cc85"; cc86 = makeF oscKeys "cc86"; cc87 = makeF oscKeys "cc87"
cc88 = makeF oscKeys "cc88"; cc89 = makeF oscKeys "cc89"; cc90 = makeF oscKeys "cc90"
cc91 = makeF oscKeys "cc91"; cc92 = makeF oscKeys "cc92"; cc93 = makeF oscKeys "cc93"
cc94 = makeF oscKeys "cc94"; cc95 = makeF oscKeys "cc95"; cc96 = makeF oscKeys "cc96"
cc97 = makeF oscKeys "cc97"; cc98 = makeF oscKeys "cc98"; cc99 = makeF oscKeys "cc99"
cc100 = makeF oscKeys "cc100"; cc101 = makeF oscKeys "cc101"; cc102 = makeF oscKeys "cc102"
cc103 = makeF oscKeys "cc103"; cc104 = makeF oscKeys "cc104"; cc105 = makeF oscKeys "cc105"
cc106 = makeF oscKeys "cc106"; cc107 = makeF oscKeys "cc107"; cc108 = makeF oscKeys "cc108"
cc109 = makeF oscKeys "cc109"; cc110 = makeF oscKeys "cc110"; cc111 = makeF oscKeys "cc111"
cc112 = makeF oscKeys "cc112"; cc113 = makeF oscKeys "cc113"; cc114 = makeF oscKeys "cc114"
cc115 = makeF oscKeys "cc115"; cc116 = makeF oscKeys "cc116"; cc117 = makeF oscKeys "cc117"
cc118 = makeF oscKeys "cc118"; cc119 = makeF oscKeys "cc119"; cc120 = makeF oscKeys "cc120"
cc121 = makeF oscKeys "cc121"; cc122 = makeF oscKeys "cc122"; cc123 = makeF oscKeys "cc123"
cc124 = makeF oscKeys "cc124"; cc125 = makeF oscKeys "cc125"; cc126 = makeF oscKeys "cc126"
cc127 = makeF oscKeys "cc127"

-- nicer names for common controllers
modwheel      = cc1
balance       = cc10
expression    = cc1
sustainpedal  = cc64
