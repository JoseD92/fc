{-# OPTIONS_GHC -w #-}
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

action_0 (34) = happyShift action_8
action_0 (35) = happyShift action_9
action_0 (42) = happyShift action_10
action_0 (43) = happyShift action_11
action_0 (44) = happyShift action_12
action_0 (4) = happyGoto action_13
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (10) = happyGoto action_6
action_0 (11) = happyGoto action_7
action_0 _ = happyFail

action_1 (34) = happyShift action_8
action_1 (35) = happyShift action_9
action_1 (42) = happyShift action_10
action_1 (43) = happyShift action_11
action_1 (44) = happyShift action_12
action_1 (4) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (11) = happyGoto action_7
action_1 _ = happyFail

action_2 (34) = happyShift action_8
action_2 (35) = happyShift action_9
action_2 (42) = happyShift action_10
action_2 (43) = happyShift action_11
action_2 (44) = happyShift action_12
action_2 (7) = happyGoto action_3
action_2 (8) = happyGoto action_14
action_2 (9) = happyGoto action_15
action_2 (10) = happyGoto action_16
action_2 (11) = happyGoto action_17
action_2 _ = happyFail

action_3 (31) = happyShift action_23
action_3 (63) = happyShift action_24
action_3 (12) = happyGoto action_22
action_3 _ = happyFail

action_4 _ = happyReduce_8

action_5 _ = happyReduce_4

action_6 _ = happyReduce_6

action_7 _ = happyReduce_2

action_8 _ = happyReduce_13

action_9 (65) = happyShift action_21
action_9 _ = happyFail

action_10 (31) = happyShift action_20
action_10 _ = happyFail

action_11 (31) = happyShift action_19
action_11 _ = happyFail

action_12 (34) = happyShift action_18
action_12 _ = happyFail

action_13 (34) = happyShift action_8
action_13 (35) = happyShift action_9
action_13 (42) = happyShift action_10
action_13 (43) = happyShift action_11
action_13 (44) = happyShift action_12
action_13 (78) = happyAccept
action_13 (7) = happyGoto action_3
action_13 (8) = happyGoto action_14
action_13 (9) = happyGoto action_15
action_13 (10) = happyGoto action_16
action_13 (11) = happyGoto action_17
action_13 _ = happyFail

action_14 _ = happyReduce_7

action_15 _ = happyReduce_3

action_16 _ = happyReduce_5

action_17 _ = happyReduce_1

action_18 _ = happyReduce_14

action_19 (67) = happyShift action_33
action_19 (16) = happyGoto action_34
action_19 _ = happyReduce_15

action_20 (67) = happyShift action_33
action_20 (16) = happyGoto action_32
action_20 _ = happyReduce_16

action_21 (34) = happyShift action_8
action_21 (35) = happyShift action_9
action_21 (42) = happyShift action_30
action_21 (43) = happyShift action_31
action_21 (44) = happyShift action_12
action_21 (7) = happyGoto action_29
action_21 _ = happyFail

action_22 (65) = happyShift action_28
action_22 (13) = happyGoto action_27
action_22 _ = happyFail

action_23 (69) = happyShift action_25
action_23 (75) = happyShift action_26
action_23 _ = happyReduce_26

action_24 _ = happyReduce_19

action_25 _ = happyReduce_20

action_26 (31) = happyShift action_51
action_26 (32) = happyShift action_52
action_26 (33) = happyShift action_53
action_26 (36) = happyShift action_54
action_26 (50) = happyShift action_55
action_26 (62) = happyShift action_56
action_26 (63) = happyShift action_57
action_26 (65) = happyShift action_58
action_26 (23) = happyGoto action_47
action_26 (28) = happyGoto action_48
action_26 (29) = happyGoto action_49
action_26 (30) = happyGoto action_50
action_26 _ = happyFail

action_27 (34) = happyShift action_8
action_27 (35) = happyShift action_9
action_27 (37) = happyShift action_46
action_27 (42) = happyShift action_30
action_27 (43) = happyShift action_31
action_27 (44) = happyShift action_12
action_27 (7) = happyGoto action_43
action_27 (26) = happyGoto action_44
action_27 (27) = happyGoto action_45
action_27 _ = happyFail

action_28 (66) = happyShift action_42
action_28 _ = happyReduce_27

action_29 (63) = happyShift action_24
action_29 (66) = happyShift action_41
action_29 _ = happyFail

action_30 (31) = happyShift action_40
action_30 _ = happyFail

action_31 (31) = happyShift action_39
action_31 _ = happyFail

action_32 (34) = happyShift action_8
action_32 (35) = happyShift action_9
action_32 (42) = happyShift action_30
action_32 (43) = happyShift action_31
action_32 (44) = happyShift action_12
action_32 (5) = happyGoto action_38
action_32 (7) = happyGoto action_36
action_32 (8) = happyGoto action_37
action_32 _ = happyFail

action_33 _ = happyReduce_30

action_34 (34) = happyShift action_8
action_34 (35) = happyShift action_9
action_34 (42) = happyShift action_30
action_34 (43) = happyShift action_31
action_34 (44) = happyShift action_12
action_34 (5) = happyGoto action_35
action_34 (7) = happyGoto action_36
action_34 (8) = happyGoto action_37
action_34 _ = happyFail

action_35 (34) = happyShift action_8
action_35 (35) = happyShift action_9
action_35 (42) = happyShift action_30
action_35 (43) = happyShift action_31
action_35 (44) = happyShift action_12
action_35 (68) = happyShift action_92
action_35 (7) = happyGoto action_36
action_35 (8) = happyGoto action_90
action_35 (17) = happyGoto action_94
action_35 _ = happyFail

action_36 (31) = happyShift action_93
action_36 (63) = happyShift action_24
action_36 _ = happyFail

action_37 _ = happyReduce_10

action_38 (34) = happyShift action_8
action_38 (35) = happyShift action_9
action_38 (42) = happyShift action_30
action_38 (43) = happyShift action_31
action_38 (44) = happyShift action_12
action_38 (68) = happyShift action_92
action_38 (7) = happyGoto action_36
action_38 (8) = happyGoto action_90
action_38 (17) = happyGoto action_91
action_38 _ = happyFail

action_39 _ = happyReduce_15

action_40 _ = happyReduce_16

action_41 (65) = happyShift action_89
action_41 _ = happyFail

action_42 (67) = happyShift action_33
action_42 (15) = happyGoto action_87
action_42 (16) = happyGoto action_88
action_42 _ = happyFail

action_43 (31) = happyShift action_86
action_43 (63) = happyShift action_24
action_43 _ = happyFail

action_44 _ = happyReduce_62

action_45 (66) = happyShift action_84
action_45 (77) = happyShift action_85
action_45 _ = happyFail

action_46 (34) = happyShift action_8
action_46 (35) = happyShift action_9
action_46 (42) = happyShift action_30
action_46 (43) = happyShift action_31
action_46 (44) = happyShift action_12
action_46 (7) = happyGoto action_83
action_46 _ = happyFail

action_47 _ = happyReduce_65

action_48 _ = happyReduce_71

action_49 _ = happyReduce_93

action_50 (51) = happyShift action_64
action_50 (52) = happyShift action_65
action_50 (53) = happyShift action_66
action_50 (54) = happyShift action_67
action_50 (55) = happyShift action_68
action_50 (56) = happyShift action_69
action_50 (57) = happyShift action_70
action_50 (58) = happyShift action_71
action_50 (59) = happyShift action_72
action_50 (61) = happyShift action_73
action_50 (62) = happyShift action_74
action_50 (63) = happyShift action_75
action_50 (64) = happyShift action_76
action_50 (70) = happyShift action_77
action_50 (71) = happyShift action_78
action_50 (72) = happyShift action_79
action_50 (73) = happyShift action_80
action_50 (74) = happyShift action_81
action_50 (76) = happyShift action_82
action_50 _ = happyFail

action_51 (65) = happyShift action_63
action_51 _ = happyReduce_70

action_52 _ = happyReduce_66

action_53 _ = happyReduce_67

action_54 _ = happyReduce_68

action_55 _ = happyReduce_69

action_56 (31) = happyShift action_51
action_56 (32) = happyShift action_52
action_56 (33) = happyShift action_53
action_56 (36) = happyShift action_54
action_56 (50) = happyShift action_55
action_56 (62) = happyShift action_56
action_56 (63) = happyShift action_57
action_56 (65) = happyShift action_58
action_56 (23) = happyGoto action_47
action_56 (28) = happyGoto action_48
action_56 (29) = happyGoto action_49
action_56 (30) = happyGoto action_62
action_56 _ = happyFail

action_57 (31) = happyShift action_51
action_57 (32) = happyShift action_52
action_57 (33) = happyShift action_53
action_57 (36) = happyShift action_54
action_57 (50) = happyShift action_55
action_57 (62) = happyShift action_56
action_57 (63) = happyShift action_57
action_57 (65) = happyShift action_58
action_57 (23) = happyGoto action_47
action_57 (28) = happyGoto action_48
action_57 (29) = happyGoto action_49
action_57 (30) = happyGoto action_61
action_57 _ = happyFail

action_58 (31) = happyShift action_51
action_58 (32) = happyShift action_52
action_58 (33) = happyShift action_53
action_58 (34) = happyShift action_8
action_58 (35) = happyShift action_9
action_58 (36) = happyShift action_54
action_58 (42) = happyShift action_30
action_58 (43) = happyShift action_31
action_58 (44) = happyShift action_12
action_58 (50) = happyShift action_55
action_58 (62) = happyShift action_56
action_58 (63) = happyShift action_57
action_58 (65) = happyShift action_58
action_58 (7) = happyGoto action_59
action_58 (23) = happyGoto action_47
action_58 (28) = happyGoto action_48
action_58 (29) = happyGoto action_49
action_58 (30) = happyGoto action_60
action_58 _ = happyFail

action_59 (63) = happyShift action_24
action_59 (66) = happyShift action_145
action_59 _ = happyFail

action_60 (51) = happyShift action_64
action_60 (52) = happyShift action_65
action_60 (53) = happyShift action_66
action_60 (54) = happyShift action_67
action_60 (55) = happyShift action_68
action_60 (56) = happyShift action_69
action_60 (57) = happyShift action_70
action_60 (58) = happyShift action_71
action_60 (59) = happyShift action_72
action_60 (61) = happyShift action_73
action_60 (62) = happyShift action_74
action_60 (63) = happyShift action_75
action_60 (64) = happyShift action_76
action_60 (66) = happyShift action_144
action_60 (70) = happyShift action_77
action_60 (71) = happyShift action_78
action_60 (72) = happyShift action_79
action_60 (73) = happyShift action_80
action_60 (74) = happyShift action_81
action_60 _ = happyFail

action_61 (53) = happyShift action_66
action_61 _ = happyReduce_92

action_62 (53) = happyShift action_66
action_62 _ = happyReduce_91

action_63 (31) = happyShift action_51
action_63 (32) = happyShift action_52
action_63 (33) = happyShift action_53
action_63 (36) = happyShift action_54
action_63 (50) = happyShift action_55
action_63 (62) = happyShift action_56
action_63 (63) = happyShift action_57
action_63 (65) = happyShift action_58
action_63 (66) = happyShift action_143
action_63 (23) = happyGoto action_47
action_63 (24) = happyGoto action_141
action_63 (28) = happyGoto action_48
action_63 (29) = happyGoto action_49
action_63 (30) = happyGoto action_142
action_63 _ = happyFail

action_64 (31) = happyShift action_51
action_64 (32) = happyShift action_52
action_64 (33) = happyShift action_53
action_64 (36) = happyShift action_54
action_64 (50) = happyShift action_55
action_64 (62) = happyShift action_56
action_64 (63) = happyShift action_57
action_64 (65) = happyShift action_58
action_64 (23) = happyGoto action_47
action_64 (28) = happyGoto action_48
action_64 (29) = happyGoto action_49
action_64 (30) = happyGoto action_140
action_64 _ = happyFail

action_65 (31) = happyShift action_51
action_65 (32) = happyShift action_52
action_65 (33) = happyShift action_53
action_65 (36) = happyShift action_54
action_65 (50) = happyShift action_55
action_65 (62) = happyShift action_56
action_65 (63) = happyShift action_57
action_65 (65) = happyShift action_58
action_65 (23) = happyGoto action_47
action_65 (28) = happyGoto action_48
action_65 (29) = happyGoto action_49
action_65 (30) = happyGoto action_139
action_65 _ = happyFail

action_66 (31) = happyShift action_138
action_66 _ = happyFail

action_67 (31) = happyShift action_51
action_67 (32) = happyShift action_52
action_67 (33) = happyShift action_53
action_67 (36) = happyShift action_54
action_67 (50) = happyShift action_55
action_67 (62) = happyShift action_56
action_67 (63) = happyShift action_57
action_67 (65) = happyShift action_58
action_67 (23) = happyGoto action_47
action_67 (28) = happyGoto action_48
action_67 (29) = happyGoto action_49
action_67 (30) = happyGoto action_137
action_67 _ = happyFail

action_68 (31) = happyShift action_51
action_68 (32) = happyShift action_52
action_68 (33) = happyShift action_53
action_68 (36) = happyShift action_54
action_68 (50) = happyShift action_55
action_68 (62) = happyShift action_56
action_68 (63) = happyShift action_57
action_68 (65) = happyShift action_58
action_68 (23) = happyGoto action_47
action_68 (28) = happyGoto action_48
action_68 (29) = happyGoto action_49
action_68 (30) = happyGoto action_136
action_68 _ = happyFail

action_69 (31) = happyShift action_51
action_69 (32) = happyShift action_52
action_69 (33) = happyShift action_53
action_69 (36) = happyShift action_54
action_69 (50) = happyShift action_55
action_69 (62) = happyShift action_56
action_69 (63) = happyShift action_57
action_69 (65) = happyShift action_58
action_69 (23) = happyGoto action_47
action_69 (28) = happyGoto action_48
action_69 (29) = happyGoto action_49
action_69 (30) = happyGoto action_135
action_69 _ = happyFail

action_70 (31) = happyShift action_51
action_70 (32) = happyShift action_52
action_70 (33) = happyShift action_53
action_70 (36) = happyShift action_54
action_70 (50) = happyShift action_55
action_70 (62) = happyShift action_56
action_70 (63) = happyShift action_57
action_70 (65) = happyShift action_58
action_70 (23) = happyGoto action_47
action_70 (28) = happyGoto action_48
action_70 (29) = happyGoto action_49
action_70 (30) = happyGoto action_134
action_70 _ = happyFail

action_71 (31) = happyShift action_51
action_71 (32) = happyShift action_52
action_71 (33) = happyShift action_53
action_71 (36) = happyShift action_54
action_71 (50) = happyShift action_55
action_71 (62) = happyShift action_56
action_71 (63) = happyShift action_57
action_71 (65) = happyShift action_58
action_71 (23) = happyGoto action_47
action_71 (28) = happyGoto action_48
action_71 (29) = happyGoto action_49
action_71 (30) = happyGoto action_133
action_71 _ = happyFail

action_72 (31) = happyShift action_51
action_72 (32) = happyShift action_52
action_72 (33) = happyShift action_53
action_72 (36) = happyShift action_54
action_72 (50) = happyShift action_55
action_72 (62) = happyShift action_56
action_72 (63) = happyShift action_57
action_72 (65) = happyShift action_58
action_72 (23) = happyGoto action_47
action_72 (28) = happyGoto action_48
action_72 (29) = happyGoto action_49
action_72 (30) = happyGoto action_132
action_72 _ = happyFail

action_73 (31) = happyShift action_51
action_73 (32) = happyShift action_52
action_73 (33) = happyShift action_53
action_73 (36) = happyShift action_54
action_73 (50) = happyShift action_55
action_73 (62) = happyShift action_56
action_73 (63) = happyShift action_57
action_73 (65) = happyShift action_58
action_73 (23) = happyGoto action_47
action_73 (28) = happyGoto action_48
action_73 (29) = happyGoto action_49
action_73 (30) = happyGoto action_131
action_73 _ = happyFail

action_74 (31) = happyShift action_51
action_74 (32) = happyShift action_52
action_74 (33) = happyShift action_53
action_74 (36) = happyShift action_54
action_74 (50) = happyShift action_55
action_74 (62) = happyShift action_56
action_74 (63) = happyShift action_57
action_74 (65) = happyShift action_58
action_74 (23) = happyGoto action_47
action_74 (28) = happyGoto action_48
action_74 (29) = happyGoto action_49
action_74 (30) = happyGoto action_130
action_74 _ = happyFail

action_75 (31) = happyShift action_51
action_75 (32) = happyShift action_52
action_75 (33) = happyShift action_53
action_75 (36) = happyShift action_54
action_75 (50) = happyShift action_55
action_75 (62) = happyShift action_56
action_75 (63) = happyShift action_57
action_75 (65) = happyShift action_58
action_75 (23) = happyGoto action_47
action_75 (28) = happyGoto action_48
action_75 (29) = happyGoto action_49
action_75 (30) = happyGoto action_129
action_75 _ = happyFail

action_76 (31) = happyShift action_51
action_76 (32) = happyShift action_52
action_76 (33) = happyShift action_53
action_76 (36) = happyShift action_54
action_76 (50) = happyShift action_55
action_76 (62) = happyShift action_56
action_76 (63) = happyShift action_57
action_76 (65) = happyShift action_58
action_76 (23) = happyGoto action_47
action_76 (28) = happyGoto action_48
action_76 (29) = happyGoto action_49
action_76 (30) = happyGoto action_128
action_76 _ = happyFail

action_77 (31) = happyShift action_51
action_77 (32) = happyShift action_52
action_77 (33) = happyShift action_53
action_77 (36) = happyShift action_54
action_77 (50) = happyShift action_55
action_77 (62) = happyShift action_56
action_77 (63) = happyShift action_57
action_77 (65) = happyShift action_58
action_77 (23) = happyGoto action_47
action_77 (28) = happyGoto action_48
action_77 (29) = happyGoto action_49
action_77 (30) = happyGoto action_127
action_77 _ = happyFail

action_78 (31) = happyShift action_51
action_78 (32) = happyShift action_52
action_78 (33) = happyShift action_53
action_78 (36) = happyShift action_54
action_78 (50) = happyShift action_55
action_78 (62) = happyShift action_56
action_78 (63) = happyShift action_57
action_78 (65) = happyShift action_58
action_78 (23) = happyGoto action_47
action_78 (28) = happyGoto action_48
action_78 (29) = happyGoto action_49
action_78 (30) = happyGoto action_126
action_78 _ = happyFail

action_79 (31) = happyShift action_51
action_79 (32) = happyShift action_52
action_79 (33) = happyShift action_53
action_79 (36) = happyShift action_54
action_79 (50) = happyShift action_55
action_79 (62) = happyShift action_56
action_79 (63) = happyShift action_57
action_79 (65) = happyShift action_58
action_79 (23) = happyGoto action_47
action_79 (28) = happyGoto action_48
action_79 (29) = happyGoto action_49
action_79 (30) = happyGoto action_125
action_79 _ = happyFail

action_80 (31) = happyShift action_51
action_80 (32) = happyShift action_52
action_80 (33) = happyShift action_53
action_80 (36) = happyShift action_54
action_80 (50) = happyShift action_55
action_80 (62) = happyShift action_56
action_80 (63) = happyShift action_57
action_80 (65) = happyShift action_58
action_80 (23) = happyGoto action_47
action_80 (28) = happyGoto action_48
action_80 (29) = happyGoto action_49
action_80 (30) = happyGoto action_124
action_80 _ = happyFail

action_81 (31) = happyShift action_51
action_81 (32) = happyShift action_52
action_81 (33) = happyShift action_53
action_81 (36) = happyShift action_54
action_81 (50) = happyShift action_55
action_81 (62) = happyShift action_56
action_81 (63) = happyShift action_57
action_81 (65) = happyShift action_58
action_81 (23) = happyGoto action_47
action_81 (28) = happyGoto action_48
action_81 (29) = happyGoto action_49
action_81 (30) = happyGoto action_123
action_81 _ = happyFail

action_82 (69) = happyShift action_122
action_82 _ = happyFail

action_83 (31) = happyShift action_121
action_83 (63) = happyShift action_24
action_83 _ = happyFail

action_84 (67) = happyShift action_33
action_84 (15) = happyGoto action_120
action_84 (16) = happyGoto action_88
action_84 _ = happyFail

action_85 (34) = happyShift action_8
action_85 (35) = happyShift action_9
action_85 (37) = happyShift action_46
action_85 (42) = happyShift action_30
action_85 (43) = happyShift action_31
action_85 (44) = happyShift action_12
action_85 (7) = happyGoto action_43
action_85 (26) = happyGoto action_119
action_85 _ = happyFail

action_86 _ = happyReduce_60

action_87 _ = happyReduce_25

action_88 (31) = happyShift action_109
action_88 (32) = happyShift action_52
action_88 (33) = happyShift action_53
action_88 (34) = happyShift action_8
action_88 (35) = happyShift action_9
action_88 (36) = happyShift action_54
action_88 (38) = happyShift action_110
action_88 (39) = happyShift action_111
action_88 (40) = happyShift action_112
action_88 (42) = happyShift action_30
action_88 (43) = happyShift action_31
action_88 (44) = happyShift action_12
action_88 (45) = happyShift action_113
action_88 (46) = happyShift action_114
action_88 (47) = happyShift action_115
action_88 (48) = happyShift action_116
action_88 (49) = happyShift action_117
action_88 (50) = happyShift action_55
action_88 (62) = happyShift action_56
action_88 (63) = happyShift action_57
action_88 (65) = happyShift action_58
action_88 (67) = happyShift action_33
action_88 (69) = happyShift action_118
action_88 (7) = happyGoto action_36
action_88 (8) = happyGoto action_100
action_88 (15) = happyGoto action_101
action_88 (16) = happyGoto action_88
action_88 (18) = happyGoto action_102
action_88 (19) = happyGoto action_103
action_88 (20) = happyGoto action_104
action_88 (21) = happyGoto action_105
action_88 (22) = happyGoto action_106
action_88 (23) = happyGoto action_47
action_88 (25) = happyGoto action_107
action_88 (28) = happyGoto action_48
action_88 (29) = happyGoto action_49
action_88 (30) = happyGoto action_108
action_88 _ = happyFail

action_89 (34) = happyShift action_8
action_89 (35) = happyShift action_9
action_89 (42) = happyShift action_30
action_89 (43) = happyShift action_31
action_89 (44) = happyShift action_12
action_89 (66) = happyShift action_99
action_89 (6) = happyGoto action_97
action_89 (7) = happyGoto action_98
action_89 _ = happyFail

action_90 _ = happyReduce_9

action_91 (69) = happyShift action_96
action_91 _ = happyFail

action_92 _ = happyReduce_31

action_93 (69) = happyShift action_25
action_93 (75) = happyShift action_26
action_93 _ = happyFail

action_94 (69) = happyShift action_95
action_94 _ = happyFail

action_95 _ = happyReduce_22

action_96 _ = happyReduce_23

action_97 (66) = happyShift action_166
action_97 (77) = happyShift action_167
action_97 _ = happyFail

action_98 (63) = happyShift action_24
action_98 _ = happyReduce_12

action_99 _ = happyReduce_18

action_100 _ = happyReduce_58

action_101 _ = happyReduce_32

action_102 _ = happyReduce_56

action_103 _ = happyReduce_33

action_104 _ = happyReduce_34

action_105 _ = happyReduce_35

action_106 (69) = happyShift action_165
action_106 _ = happyFail

action_107 (31) = happyShift action_109
action_107 (32) = happyShift action_52
action_107 (33) = happyShift action_53
action_107 (34) = happyShift action_8
action_107 (35) = happyShift action_9
action_107 (36) = happyShift action_54
action_107 (38) = happyShift action_110
action_107 (39) = happyShift action_111
action_107 (40) = happyShift action_112
action_107 (42) = happyShift action_30
action_107 (43) = happyShift action_31
action_107 (44) = happyShift action_12
action_107 (45) = happyShift action_113
action_107 (46) = happyShift action_114
action_107 (47) = happyShift action_115
action_107 (48) = happyShift action_116
action_107 (49) = happyShift action_117
action_107 (50) = happyShift action_55
action_107 (62) = happyShift action_56
action_107 (63) = happyShift action_57
action_107 (65) = happyShift action_58
action_107 (67) = happyShift action_33
action_107 (68) = happyShift action_92
action_107 (69) = happyShift action_118
action_107 (7) = happyGoto action_36
action_107 (8) = happyGoto action_162
action_107 (15) = happyGoto action_101
action_107 (16) = happyGoto action_88
action_107 (17) = happyGoto action_163
action_107 (18) = happyGoto action_164
action_107 (19) = happyGoto action_103
action_107 (20) = happyGoto action_104
action_107 (21) = happyGoto action_105
action_107 (22) = happyGoto action_106
action_107 (23) = happyGoto action_47
action_107 (28) = happyGoto action_48
action_107 (29) = happyGoto action_49
action_107 (30) = happyGoto action_108
action_107 _ = happyFail

action_108 (51) = happyShift action_64
action_108 (52) = happyShift action_65
action_108 (53) = happyShift action_66
action_108 (54) = happyShift action_67
action_108 (55) = happyShift action_68
action_108 (56) = happyShift action_69
action_108 (57) = happyShift action_70
action_108 (58) = happyShift action_71
action_108 (59) = happyShift action_72
action_108 (61) = happyShift action_73
action_108 (62) = happyShift action_74
action_108 (63) = happyShift action_75
action_108 (64) = happyShift action_76
action_108 (69) = happyShift action_161
action_108 (70) = happyShift action_77
action_108 (71) = happyShift action_78
action_108 (72) = happyShift action_79
action_108 (73) = happyShift action_80
action_108 (74) = happyShift action_81
action_108 _ = happyFail

action_109 (60) = happyShift action_159
action_109 (65) = happyShift action_63
action_109 (75) = happyShift action_160
action_109 _ = happyReduce_70

action_110 (65) = happyShift action_158
action_110 _ = happyFail

action_111 (65) = happyShift action_157
action_111 _ = happyFail

action_112 (65) = happyShift action_156
action_112 _ = happyFail

action_113 (69) = happyShift action_155
action_113 _ = happyFail

action_114 (69) = happyShift action_154
action_114 _ = happyFail

action_115 (31) = happyShift action_51
action_115 (32) = happyShift action_52
action_115 (33) = happyShift action_53
action_115 (36) = happyShift action_54
action_115 (50) = happyShift action_55
action_115 (62) = happyShift action_56
action_115 (63) = happyShift action_57
action_115 (65) = happyShift action_58
action_115 (69) = happyShift action_153
action_115 (23) = happyGoto action_47
action_115 (28) = happyGoto action_48
action_115 (29) = happyGoto action_49
action_115 (30) = happyGoto action_152
action_115 _ = happyFail

action_116 (31) = happyShift action_151
action_116 _ = happyFail

action_117 (31) = happyShift action_51
action_117 (32) = happyShift action_52
action_117 (33) = happyShift action_53
action_117 (36) = happyShift action_54
action_117 (50) = happyShift action_55
action_117 (62) = happyShift action_56
action_117 (63) = happyShift action_57
action_117 (65) = happyShift action_58
action_117 (23) = happyGoto action_47
action_117 (28) = happyGoto action_48
action_117 (29) = happyGoto action_49
action_117 (30) = happyGoto action_150
action_117 _ = happyFail

action_118 _ = happyReduce_44

action_119 _ = happyReduce_61

action_120 (14) = happyGoto action_149
action_120 _ = happyReduce_28

action_121 _ = happyReduce_59

action_122 _ = happyReduce_21

action_123 (51) = happyShift action_64
action_123 (52) = happyShift action_65
action_123 (53) = happyShift action_66
action_123 (54) = happyShift action_67
action_123 (55) = happyShift action_68
action_123 (56) = happyShift action_69
action_123 (57) = happyShift action_70
action_123 (61) = happyShift action_73
action_123 (62) = happyShift action_74
action_123 (63) = happyShift action_75
action_123 (64) = happyShift action_76
action_123 (70) = happyShift action_77
action_123 (71) = happyShift action_78
action_123 (72) = happyShift action_79
action_123 (73) = happyShift action_80
action_123 _ = happyReduce_86

action_124 (51) = happyShift action_64
action_124 (52) = happyShift action_65
action_124 (53) = happyShift action_66
action_124 (54) = happyShift action_67
action_124 (55) = happyShift action_68
action_124 (56) = happyShift action_69
action_124 (57) = happyShift action_70
action_124 (61) = happyShift action_73
action_124 (62) = happyShift action_74
action_124 (63) = happyShift action_75
action_124 (64) = happyShift action_76
action_124 (70) = happyShift action_77
action_124 (71) = happyShift action_78
action_124 (72) = happyShift action_79
action_124 _ = happyReduce_84

action_125 (51) = happyShift action_64
action_125 (52) = happyShift action_65
action_125 (53) = happyShift action_66
action_125 (55) = happyFail
action_125 (56) = happyFail
action_125 (61) = happyShift action_73
action_125 (62) = happyShift action_74
action_125 (63) = happyShift action_75
action_125 (64) = happyShift action_76
action_125 (70) = happyShift action_77
action_125 (71) = happyFail
action_125 (72) = happyFail
action_125 _ = happyReduce_80

action_126 (51) = happyShift action_64
action_126 (52) = happyShift action_65
action_126 (53) = happyShift action_66
action_126 (55) = happyFail
action_126 (56) = happyFail
action_126 (61) = happyShift action_73
action_126 (62) = happyShift action_74
action_126 (63) = happyShift action_75
action_126 (64) = happyShift action_76
action_126 (70) = happyShift action_77
action_126 (71) = happyFail
action_126 (72) = happyFail
action_126 _ = happyReduce_78

action_127 (53) = happyShift action_66
action_127 _ = happyReduce_85

action_128 (53) = happyShift action_66
action_128 _ = happyReduce_75

action_129 (53) = happyShift action_66
action_129 _ = happyReduce_74

action_130 (53) = happyShift action_66
action_130 (63) = happyShift action_75
action_130 (64) = happyShift action_76
action_130 (70) = happyShift action_77
action_130 _ = happyReduce_73

action_131 (53) = happyShift action_66
action_131 (63) = happyShift action_75
action_131 (64) = happyShift action_76
action_131 (70) = happyShift action_77
action_131 _ = happyReduce_72

action_132 (51) = happyShift action_64
action_132 (52) = happyShift action_65
action_132 (53) = happyShift action_66
action_132 (54) = happyShift action_67
action_132 (55) = happyShift action_68
action_132 (56) = happyShift action_69
action_132 (57) = happyShift action_70
action_132 (61) = happyShift action_73
action_132 (62) = happyShift action_74
action_132 (63) = happyShift action_75
action_132 (64) = happyShift action_76
action_132 (70) = happyShift action_77
action_132 (71) = happyShift action_78
action_132 (72) = happyShift action_79
action_132 (73) = happyShift action_80
action_132 (74) = happyShift action_81
action_132 _ = happyReduce_87

action_133 (51) = happyShift action_64
action_133 (52) = happyShift action_65
action_133 (53) = happyShift action_66
action_133 (54) = happyShift action_67
action_133 (55) = happyShift action_68
action_133 (56) = happyShift action_69
action_133 (57) = happyShift action_70
action_133 (59) = happyShift action_72
action_133 (61) = happyShift action_73
action_133 (62) = happyShift action_74
action_133 (63) = happyShift action_75
action_133 (64) = happyShift action_76
action_133 (70) = happyShift action_77
action_133 (71) = happyShift action_78
action_133 (72) = happyShift action_79
action_133 (73) = happyShift action_80
action_133 (74) = happyShift action_81
action_133 _ = happyReduce_88

action_134 (51) = happyShift action_64
action_134 (52) = happyShift action_65
action_134 (53) = happyShift action_66
action_134 (54) = happyFail
action_134 (55) = happyShift action_68
action_134 (56) = happyShift action_69
action_134 (57) = happyFail
action_134 (61) = happyShift action_73
action_134 (62) = happyShift action_74
action_134 (63) = happyShift action_75
action_134 (64) = happyShift action_76
action_134 (70) = happyShift action_77
action_134 (71) = happyShift action_78
action_134 (72) = happyShift action_79
action_134 _ = happyReduce_83

action_135 (51) = happyShift action_64
action_135 (52) = happyShift action_65
action_135 (53) = happyShift action_66
action_135 (55) = happyFail
action_135 (56) = happyFail
action_135 (61) = happyShift action_73
action_135 (62) = happyShift action_74
action_135 (63) = happyShift action_75
action_135 (64) = happyShift action_76
action_135 (70) = happyShift action_77
action_135 (71) = happyFail
action_135 (72) = happyFail
action_135 _ = happyReduce_79

action_136 (51) = happyShift action_64
action_136 (52) = happyShift action_65
action_136 (53) = happyShift action_66
action_136 (55) = happyFail
action_136 (56) = happyFail
action_136 (61) = happyShift action_73
action_136 (62) = happyShift action_74
action_136 (63) = happyShift action_75
action_136 (64) = happyShift action_76
action_136 (70) = happyShift action_77
action_136 (71) = happyFail
action_136 (72) = happyFail
action_136 _ = happyReduce_81

action_137 (51) = happyShift action_64
action_137 (52) = happyShift action_65
action_137 (53) = happyShift action_66
action_137 (54) = happyFail
action_137 (55) = happyShift action_68
action_137 (56) = happyShift action_69
action_137 (57) = happyFail
action_137 (61) = happyShift action_73
action_137 (62) = happyShift action_74
action_137 (63) = happyShift action_75
action_137 (64) = happyShift action_76
action_137 (70) = happyShift action_77
action_137 (71) = happyShift action_78
action_137 (72) = happyShift action_79
action_137 _ = happyReduce_82

action_138 _ = happyReduce_89

action_139 (53) = happyShift action_66
action_139 (61) = happyShift action_73
action_139 (62) = happyShift action_74
action_139 (63) = happyShift action_75
action_139 (64) = happyShift action_76
action_139 (70) = happyShift action_77
action_139 _ = happyReduce_77

action_140 (53) = happyShift action_66
action_140 (61) = happyShift action_73
action_140 (62) = happyShift action_74
action_140 (63) = happyShift action_75
action_140 (64) = happyShift action_76
action_140 (70) = happyShift action_77
action_140 _ = happyReduce_76

action_141 (66) = happyShift action_147
action_141 (77) = happyShift action_148
action_141 _ = happyFail

action_142 (51) = happyShift action_64
action_142 (52) = happyShift action_65
action_142 (53) = happyShift action_66
action_142 (54) = happyShift action_67
action_142 (55) = happyShift action_68
action_142 (56) = happyShift action_69
action_142 (57) = happyShift action_70
action_142 (58) = happyShift action_71
action_142 (59) = happyShift action_72
action_142 (61) = happyShift action_73
action_142 (62) = happyShift action_74
action_142 (63) = happyShift action_75
action_142 (64) = happyShift action_76
action_142 (70) = happyShift action_77
action_142 (71) = happyShift action_78
action_142 (72) = happyShift action_79
action_142 (73) = happyShift action_80
action_142 (74) = happyShift action_81
action_142 _ = happyReduce_54

action_143 _ = happyReduce_52

action_144 _ = happyReduce_90

action_145 (65) = happyShift action_146
action_145 _ = happyFail

action_146 (34) = happyShift action_8
action_146 (35) = happyShift action_9
action_146 (37) = happyShift action_46
action_146 (42) = happyShift action_30
action_146 (43) = happyShift action_31
action_146 (44) = happyShift action_12
action_146 (66) = happyShift action_179
action_146 (7) = happyGoto action_43
action_146 (26) = happyGoto action_44
action_146 (27) = happyGoto action_178
action_146 _ = happyFail

action_147 _ = happyReduce_51

action_148 (31) = happyShift action_51
action_148 (32) = happyShift action_52
action_148 (33) = happyShift action_53
action_148 (36) = happyShift action_54
action_148 (50) = happyShift action_55
action_148 (62) = happyShift action_56
action_148 (63) = happyShift action_57
action_148 (65) = happyShift action_58
action_148 (23) = happyGoto action_47
action_148 (28) = happyGoto action_48
action_148 (29) = happyGoto action_49
action_148 (30) = happyGoto action_177
action_148 _ = happyFail

action_149 _ = happyReduce_24

action_150 (51) = happyShift action_64
action_150 (52) = happyShift action_65
action_150 (53) = happyShift action_66
action_150 (54) = happyShift action_67
action_150 (55) = happyShift action_68
action_150 (56) = happyShift action_69
action_150 (57) = happyShift action_70
action_150 (58) = happyShift action_71
action_150 (59) = happyShift action_72
action_150 (61) = happyShift action_73
action_150 (62) = happyShift action_74
action_150 (63) = happyShift action_75
action_150 (64) = happyShift action_76
action_150 (69) = happyShift action_176
action_150 (70) = happyShift action_77
action_150 (71) = happyShift action_78
action_150 (72) = happyShift action_79
action_150 (73) = happyShift action_80
action_150 (74) = happyShift action_81
action_150 _ = happyFail

action_151 (69) = happyShift action_175
action_151 _ = happyFail

action_152 (51) = happyShift action_64
action_152 (52) = happyShift action_65
action_152 (53) = happyShift action_66
action_152 (54) = happyShift action_67
action_152 (55) = happyShift action_68
action_152 (56) = happyShift action_69
action_152 (57) = happyShift action_70
action_152 (58) = happyShift action_71
action_152 (59) = happyShift action_72
action_152 (61) = happyShift action_73
action_152 (62) = happyShift action_74
action_152 (63) = happyShift action_75
action_152 (64) = happyShift action_76
action_152 (69) = happyShift action_174
action_152 (70) = happyShift action_77
action_152 (71) = happyShift action_78
action_152 (72) = happyShift action_79
action_152 (73) = happyShift action_80
action_152 (74) = happyShift action_81
action_152 _ = happyFail

action_153 _ = happyReduce_40

action_154 _ = happyReduce_38

action_155 _ = happyReduce_39

action_156 (31) = happyShift action_109
action_156 (32) = happyShift action_52
action_156 (33) = happyShift action_53
action_156 (36) = happyShift action_54
action_156 (38) = happyShift action_110
action_156 (39) = happyShift action_111
action_156 (40) = happyShift action_112
action_156 (45) = happyShift action_113
action_156 (46) = happyShift action_114
action_156 (47) = happyShift action_115
action_156 (48) = happyShift action_116
action_156 (49) = happyShift action_117
action_156 (50) = happyShift action_55
action_156 (62) = happyShift action_56
action_156 (63) = happyShift action_57
action_156 (65) = happyShift action_58
action_156 (67) = happyShift action_33
action_156 (69) = happyShift action_118
action_156 (15) = happyGoto action_101
action_156 (16) = happyGoto action_88
action_156 (18) = happyGoto action_173
action_156 (19) = happyGoto action_103
action_156 (20) = happyGoto action_104
action_156 (21) = happyGoto action_105
action_156 (22) = happyGoto action_106
action_156 (23) = happyGoto action_47
action_156 (28) = happyGoto action_48
action_156 (29) = happyGoto action_49
action_156 (30) = happyGoto action_108
action_156 _ = happyFail

action_157 (31) = happyShift action_51
action_157 (32) = happyShift action_52
action_157 (33) = happyShift action_53
action_157 (36) = happyShift action_54
action_157 (50) = happyShift action_55
action_157 (62) = happyShift action_56
action_157 (63) = happyShift action_57
action_157 (65) = happyShift action_58
action_157 (23) = happyGoto action_47
action_157 (28) = happyGoto action_48
action_157 (29) = happyGoto action_49
action_157 (30) = happyGoto action_172
action_157 _ = happyFail

action_158 (31) = happyShift action_51
action_158 (32) = happyShift action_52
action_158 (33) = happyShift action_53
action_158 (36) = happyShift action_54
action_158 (50) = happyShift action_55
action_158 (62) = happyShift action_56
action_158 (63) = happyShift action_57
action_158 (65) = happyShift action_58
action_158 (23) = happyGoto action_47
action_158 (28) = happyGoto action_48
action_158 (29) = happyGoto action_49
action_158 (30) = happyGoto action_171
action_158 _ = happyFail

action_159 (31) = happyShift action_51
action_159 (32) = happyShift action_52
action_159 (33) = happyShift action_53
action_159 (36) = happyShift action_54
action_159 (50) = happyShift action_55
action_159 (62) = happyShift action_56
action_159 (63) = happyShift action_57
action_159 (65) = happyShift action_58
action_159 (23) = happyGoto action_47
action_159 (28) = happyGoto action_48
action_159 (29) = happyGoto action_49
action_159 (30) = happyGoto action_170
action_159 _ = happyFail

action_160 (31) = happyShift action_51
action_160 (32) = happyShift action_52
action_160 (33) = happyShift action_53
action_160 (36) = happyShift action_54
action_160 (50) = happyShift action_55
action_160 (62) = happyShift action_56
action_160 (63) = happyShift action_57
action_160 (65) = happyShift action_58
action_160 (23) = happyGoto action_47
action_160 (28) = happyGoto action_48
action_160 (29) = happyGoto action_49
action_160 (30) = happyGoto action_169
action_160 _ = happyFail

action_161 _ = happyReduce_37

action_162 _ = happyReduce_57

action_163 _ = happyReduce_29

action_164 _ = happyReduce_55

action_165 _ = happyReduce_36

action_166 _ = happyReduce_17

action_167 (34) = happyShift action_8
action_167 (35) = happyShift action_9
action_167 (42) = happyShift action_30
action_167 (43) = happyShift action_31
action_167 (44) = happyShift action_12
action_167 (7) = happyGoto action_168
action_167 _ = happyFail

action_168 (63) = happyShift action_24
action_168 _ = happyReduce_11

action_169 (51) = happyShift action_64
action_169 (52) = happyShift action_65
action_169 (53) = happyShift action_66
action_169 (54) = happyShift action_67
action_169 (55) = happyShift action_68
action_169 (56) = happyShift action_69
action_169 (57) = happyShift action_70
action_169 (58) = happyShift action_71
action_169 (59) = happyShift action_72
action_169 (61) = happyShift action_73
action_169 (62) = happyShift action_74
action_169 (63) = happyShift action_75
action_169 (64) = happyShift action_76
action_169 (70) = happyShift action_77
action_169 (71) = happyShift action_78
action_169 (72) = happyShift action_79
action_169 (73) = happyShift action_80
action_169 (74) = happyShift action_81
action_169 (76) = happyShift action_185
action_169 _ = happyFail

action_170 (51) = happyShift action_64
action_170 (52) = happyShift action_65
action_170 (53) = happyShift action_66
action_170 (54) = happyShift action_67
action_170 (55) = happyShift action_68
action_170 (56) = happyShift action_69
action_170 (57) = happyShift action_70
action_170 (58) = happyShift action_71
action_170 (59) = happyShift action_72
action_170 (61) = happyShift action_73
action_170 (62) = happyShift action_74
action_170 (63) = happyShift action_75
action_170 (64) = happyShift action_76
action_170 (70) = happyShift action_77
action_170 (71) = happyShift action_78
action_170 (72) = happyShift action_79
action_170 (73) = happyShift action_80
action_170 (74) = happyShift action_81
action_170 _ = happyReduce_49

action_171 (51) = happyShift action_64
action_171 (52) = happyShift action_65
action_171 (53) = happyShift action_66
action_171 (54) = happyShift action_67
action_171 (55) = happyShift action_68
action_171 (56) = happyShift action_69
action_171 (57) = happyShift action_70
action_171 (58) = happyShift action_71
action_171 (59) = happyShift action_72
action_171 (61) = happyShift action_73
action_171 (62) = happyShift action_74
action_171 (63) = happyShift action_75
action_171 (64) = happyShift action_76
action_171 (66) = happyShift action_184
action_171 (70) = happyShift action_77
action_171 (71) = happyShift action_78
action_171 (72) = happyShift action_79
action_171 (73) = happyShift action_80
action_171 (74) = happyShift action_81
action_171 _ = happyFail

action_172 (51) = happyShift action_64
action_172 (52) = happyShift action_65
action_172 (53) = happyShift action_66
action_172 (54) = happyShift action_67
action_172 (55) = happyShift action_68
action_172 (56) = happyShift action_69
action_172 (57) = happyShift action_70
action_172 (58) = happyShift action_71
action_172 (59) = happyShift action_72
action_172 (61) = happyShift action_73
action_172 (62) = happyShift action_74
action_172 (63) = happyShift action_75
action_172 (64) = happyShift action_76
action_172 (66) = happyShift action_183
action_172 (70) = happyShift action_77
action_172 (71) = happyShift action_78
action_172 (72) = happyShift action_79
action_172 (73) = happyShift action_80
action_172 (74) = happyShift action_81
action_172 _ = happyFail

action_173 (69) = happyShift action_182
action_173 _ = happyFail

action_174 _ = happyReduce_41

action_175 _ = happyReduce_42

action_176 _ = happyReduce_43

action_177 (51) = happyShift action_64
action_177 (52) = happyShift action_65
action_177 (53) = happyShift action_66
action_177 (54) = happyShift action_67
action_177 (55) = happyShift action_68
action_177 (56) = happyShift action_69
action_177 (57) = happyShift action_70
action_177 (58) = happyShift action_71
action_177 (59) = happyShift action_72
action_177 (61) = happyShift action_73
action_177 (62) = happyShift action_74
action_177 (63) = happyShift action_75
action_177 (64) = happyShift action_76
action_177 (70) = happyShift action_77
action_177 (71) = happyShift action_78
action_177 (72) = happyShift action_79
action_177 (73) = happyShift action_80
action_177 (74) = happyShift action_81
action_177 _ = happyReduce_53

action_178 (66) = happyShift action_181
action_178 (77) = happyShift action_85
action_178 _ = happyFail

action_179 (67) = happyShift action_180
action_179 _ = happyFail

action_180 (31) = happyShift action_109
action_180 (32) = happyShift action_52
action_180 (33) = happyShift action_53
action_180 (34) = happyShift action_8
action_180 (35) = happyShift action_9
action_180 (36) = happyShift action_54
action_180 (38) = happyShift action_110
action_180 (39) = happyShift action_111
action_180 (40) = happyShift action_112
action_180 (42) = happyShift action_30
action_180 (43) = happyShift action_31
action_180 (44) = happyShift action_12
action_180 (45) = happyShift action_113
action_180 (46) = happyShift action_114
action_180 (47) = happyShift action_115
action_180 (48) = happyShift action_116
action_180 (49) = happyShift action_117
action_180 (50) = happyShift action_55
action_180 (62) = happyShift action_56
action_180 (63) = happyShift action_57
action_180 (65) = happyShift action_58
action_180 (67) = happyShift action_33
action_180 (69) = happyShift action_118
action_180 (7) = happyGoto action_36
action_180 (8) = happyGoto action_100
action_180 (15) = happyGoto action_101
action_180 (16) = happyGoto action_88
action_180 (18) = happyGoto action_102
action_180 (19) = happyGoto action_103
action_180 (20) = happyGoto action_104
action_180 (21) = happyGoto action_105
action_180 (22) = happyGoto action_106
action_180 (23) = happyGoto action_47
action_180 (25) = happyGoto action_191
action_180 (28) = happyGoto action_48
action_180 (29) = happyGoto action_49
action_180 (30) = happyGoto action_108
action_180 _ = happyFail

action_181 (67) = happyShift action_190
action_181 _ = happyFail

action_182 (31) = happyShift action_51
action_182 (32) = happyShift action_52
action_182 (33) = happyShift action_53
action_182 (36) = happyShift action_54
action_182 (50) = happyShift action_55
action_182 (62) = happyShift action_56
action_182 (63) = happyShift action_57
action_182 (65) = happyShift action_58
action_182 (23) = happyGoto action_47
action_182 (28) = happyGoto action_48
action_182 (29) = happyGoto action_49
action_182 (30) = happyGoto action_189
action_182 _ = happyFail

action_183 (31) = happyShift action_109
action_183 (32) = happyShift action_52
action_183 (33) = happyShift action_53
action_183 (36) = happyShift action_54
action_183 (38) = happyShift action_110
action_183 (39) = happyShift action_111
action_183 (40) = happyShift action_112
action_183 (45) = happyShift action_113
action_183 (46) = happyShift action_114
action_183 (47) = happyShift action_115
action_183 (48) = happyShift action_116
action_183 (49) = happyShift action_117
action_183 (50) = happyShift action_55
action_183 (62) = happyShift action_56
action_183 (63) = happyShift action_57
action_183 (65) = happyShift action_58
action_183 (67) = happyShift action_33
action_183 (69) = happyShift action_118
action_183 (15) = happyGoto action_101
action_183 (16) = happyGoto action_88
action_183 (18) = happyGoto action_188
action_183 (19) = happyGoto action_103
action_183 (20) = happyGoto action_104
action_183 (21) = happyGoto action_105
action_183 (22) = happyGoto action_106
action_183 (23) = happyGoto action_47
action_183 (28) = happyGoto action_48
action_183 (29) = happyGoto action_49
action_183 (30) = happyGoto action_108
action_183 _ = happyFail

action_184 (67) = happyShift action_33
action_184 (15) = happyGoto action_187
action_184 (16) = happyGoto action_88
action_184 _ = happyFail

action_185 (60) = happyShift action_186
action_185 _ = happyFail

action_186 (31) = happyShift action_51
action_186 (32) = happyShift action_52
action_186 (33) = happyShift action_53
action_186 (36) = happyShift action_54
action_186 (50) = happyShift action_55
action_186 (62) = happyShift action_56
action_186 (63) = happyShift action_57
action_186 (65) = happyShift action_58
action_186 (23) = happyGoto action_47
action_186 (28) = happyGoto action_48
action_186 (29) = happyGoto action_49
action_186 (30) = happyGoto action_196
action_186 _ = happyFail

action_187 (41) = happyShift action_195
action_187 _ = happyReduce_47

action_188 _ = happyReduce_46

action_189 (51) = happyShift action_64
action_189 (52) = happyShift action_65
action_189 (53) = happyShift action_66
action_189 (54) = happyShift action_67
action_189 (55) = happyShift action_68
action_189 (56) = happyShift action_69
action_189 (57) = happyShift action_70
action_189 (58) = happyShift action_71
action_189 (59) = happyShift action_72
action_189 (61) = happyShift action_73
action_189 (62) = happyShift action_74
action_189 (63) = happyShift action_75
action_189 (64) = happyShift action_76
action_189 (69) = happyShift action_194
action_189 (70) = happyShift action_77
action_189 (71) = happyShift action_78
action_189 (72) = happyShift action_79
action_189 (73) = happyShift action_80
action_189 (74) = happyShift action_81
action_189 _ = happyFail

action_190 (31) = happyShift action_109
action_190 (32) = happyShift action_52
action_190 (33) = happyShift action_53
action_190 (34) = happyShift action_8
action_190 (35) = happyShift action_9
action_190 (36) = happyShift action_54
action_190 (38) = happyShift action_110
action_190 (39) = happyShift action_111
action_190 (40) = happyShift action_112
action_190 (42) = happyShift action_30
action_190 (43) = happyShift action_31
action_190 (44) = happyShift action_12
action_190 (45) = happyShift action_113
action_190 (46) = happyShift action_114
action_190 (47) = happyShift action_115
action_190 (48) = happyShift action_116
action_190 (49) = happyShift action_117
action_190 (50) = happyShift action_55
action_190 (62) = happyShift action_56
action_190 (63) = happyShift action_57
action_190 (65) = happyShift action_58
action_190 (67) = happyShift action_33
action_190 (69) = happyShift action_118
action_190 (7) = happyGoto action_36
action_190 (8) = happyGoto action_100
action_190 (15) = happyGoto action_101
action_190 (16) = happyGoto action_88
action_190 (18) = happyGoto action_102
action_190 (19) = happyGoto action_103
action_190 (20) = happyGoto action_104
action_190 (21) = happyGoto action_105
action_190 (22) = happyGoto action_106
action_190 (23) = happyGoto action_47
action_190 (25) = happyGoto action_193
action_190 (28) = happyGoto action_48
action_190 (29) = happyGoto action_49
action_190 (30) = happyGoto action_108
action_190 _ = happyFail

action_191 (31) = happyShift action_109
action_191 (32) = happyShift action_52
action_191 (33) = happyShift action_53
action_191 (34) = happyShift action_8
action_191 (35) = happyShift action_9
action_191 (36) = happyShift action_54
action_191 (38) = happyShift action_110
action_191 (39) = happyShift action_111
action_191 (40) = happyShift action_112
action_191 (42) = happyShift action_30
action_191 (43) = happyShift action_31
action_191 (44) = happyShift action_12
action_191 (45) = happyShift action_113
action_191 (46) = happyShift action_114
action_191 (47) = happyShift action_115
action_191 (48) = happyShift action_116
action_191 (49) = happyShift action_117
action_191 (50) = happyShift action_55
action_191 (62) = happyShift action_56
action_191 (63) = happyShift action_57
action_191 (65) = happyShift action_58
action_191 (67) = happyShift action_33
action_191 (68) = happyShift action_192
action_191 (69) = happyShift action_118
action_191 (7) = happyGoto action_36
action_191 (8) = happyGoto action_162
action_191 (15) = happyGoto action_101
action_191 (16) = happyGoto action_88
action_191 (18) = happyGoto action_164
action_191 (19) = happyGoto action_103
action_191 (20) = happyGoto action_104
action_191 (21) = happyGoto action_105
action_191 (22) = happyGoto action_106
action_191 (23) = happyGoto action_47
action_191 (28) = happyGoto action_48
action_191 (29) = happyGoto action_49
action_191 (30) = happyGoto action_108
action_191 _ = happyFail

action_192 _ = happyReduce_64

action_193 (31) = happyShift action_109
action_193 (32) = happyShift action_52
action_193 (33) = happyShift action_53
action_193 (34) = happyShift action_8
action_193 (35) = happyShift action_9
action_193 (36) = happyShift action_54
action_193 (38) = happyShift action_110
action_193 (39) = happyShift action_111
action_193 (40) = happyShift action_112
action_193 (42) = happyShift action_30
action_193 (43) = happyShift action_31
action_193 (44) = happyShift action_12
action_193 (45) = happyShift action_113
action_193 (46) = happyShift action_114
action_193 (47) = happyShift action_115
action_193 (48) = happyShift action_116
action_193 (49) = happyShift action_117
action_193 (50) = happyShift action_55
action_193 (62) = happyShift action_56
action_193 (63) = happyShift action_57
action_193 (65) = happyShift action_58
action_193 (67) = happyShift action_33
action_193 (68) = happyShift action_199
action_193 (69) = happyShift action_118
action_193 (7) = happyGoto action_36
action_193 (8) = happyGoto action_162
action_193 (15) = happyGoto action_101
action_193 (16) = happyGoto action_88
action_193 (18) = happyGoto action_164
action_193 (19) = happyGoto action_103
action_193 (20) = happyGoto action_104
action_193 (21) = happyGoto action_105
action_193 (22) = happyGoto action_106
action_193 (23) = happyGoto action_47
action_193 (28) = happyGoto action_48
action_193 (29) = happyGoto action_49
action_193 (30) = happyGoto action_108
action_193 _ = happyFail

action_194 (31) = happyShift action_109
action_194 (32) = happyShift action_52
action_194 (33) = happyShift action_53
action_194 (36) = happyShift action_54
action_194 (38) = happyShift action_110
action_194 (39) = happyShift action_111
action_194 (40) = happyShift action_112
action_194 (45) = happyShift action_113
action_194 (46) = happyShift action_114
action_194 (47) = happyShift action_115
action_194 (48) = happyShift action_116
action_194 (49) = happyShift action_117
action_194 (50) = happyShift action_55
action_194 (62) = happyShift action_56
action_194 (63) = happyShift action_57
action_194 (65) = happyShift action_58
action_194 (67) = happyShift action_33
action_194 (69) = happyShift action_118
action_194 (15) = happyGoto action_101
action_194 (16) = happyGoto action_88
action_194 (18) = happyGoto action_198
action_194 (19) = happyGoto action_103
action_194 (20) = happyGoto action_104
action_194 (21) = happyGoto action_105
action_194 (22) = happyGoto action_106
action_194 (23) = happyGoto action_47
action_194 (28) = happyGoto action_48
action_194 (29) = happyGoto action_49
action_194 (30) = happyGoto action_108
action_194 _ = happyFail

action_195 (31) = happyShift action_109
action_195 (32) = happyShift action_52
action_195 (33) = happyShift action_53
action_195 (36) = happyShift action_54
action_195 (38) = happyShift action_110
action_195 (39) = happyShift action_111
action_195 (40) = happyShift action_112
action_195 (45) = happyShift action_113
action_195 (46) = happyShift action_114
action_195 (47) = happyShift action_115
action_195 (48) = happyShift action_116
action_195 (49) = happyShift action_117
action_195 (50) = happyShift action_55
action_195 (62) = happyShift action_56
action_195 (63) = happyShift action_57
action_195 (65) = happyShift action_58
action_195 (67) = happyShift action_33
action_195 (69) = happyShift action_118
action_195 (15) = happyGoto action_101
action_195 (16) = happyGoto action_88
action_195 (18) = happyGoto action_197
action_195 (19) = happyGoto action_103
action_195 (20) = happyGoto action_104
action_195 (21) = happyGoto action_105
action_195 (22) = happyGoto action_106
action_195 (23) = happyGoto action_47
action_195 (28) = happyGoto action_48
action_195 (29) = happyGoto action_49
action_195 (30) = happyGoto action_108
action_195 _ = happyFail

action_196 (51) = happyShift action_64
action_196 (52) = happyShift action_65
action_196 (53) = happyShift action_66
action_196 (54) = happyShift action_67
action_196 (55) = happyShift action_68
action_196 (56) = happyShift action_69
action_196 (57) = happyShift action_70
action_196 (58) = happyShift action_71
action_196 (59) = happyShift action_72
action_196 (61) = happyShift action_73
action_196 (62) = happyShift action_74
action_196 (63) = happyShift action_75
action_196 (64) = happyShift action_76
action_196 (70) = happyShift action_77
action_196 (71) = happyShift action_78
action_196 (72) = happyShift action_79
action_196 (73) = happyShift action_80
action_196 (74) = happyShift action_81
action_196 _ = happyReduce_50

action_197 _ = happyReduce_48

action_198 (66) = happyShift action_200
action_198 _ = happyFail

action_199 _ = happyReduce_63

action_200 (31) = happyShift action_109
action_200 (32) = happyShift action_52
action_200 (33) = happyShift action_53
action_200 (36) = happyShift action_54
action_200 (38) = happyShift action_110
action_200 (39) = happyShift action_111
action_200 (40) = happyShift action_112
action_200 (45) = happyShift action_113
action_200 (46) = happyShift action_114
action_200 (47) = happyShift action_115
action_200 (48) = happyShift action_116
action_200 (49) = happyShift action_117
action_200 (50) = happyShift action_55
action_200 (62) = happyShift action_56
action_200 (63) = happyShift action_57
action_200 (65) = happyShift action_58
action_200 (67) = happyShift action_33
action_200 (69) = happyShift action_118
action_200 (15) = happyGoto action_101
action_200 (16) = happyGoto action_88
action_200 (18) = happyGoto action_201
action_200 (19) = happyGoto action_103
action_200 (20) = happyGoto action_104
action_200 (21) = happyGoto action_105
action_200 (22) = happyGoto action_106
action_200 (23) = happyGoto action_47
action_200 (28) = happyGoto action_48
action_200 (29) = happyGoto action_49
action_200 (30) = happyGoto action_108
action_200 _ = happyFail

action_201 _ = happyReduce_45

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (
	)

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (
	)

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn4
		 (
	)

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn4
		 (
	)

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn5
		 (
	)

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (
	)

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_3:happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyTerminal (Type happy_var_2))
	_
	 =  HappyAbsSyn7
		 ({-TUnsigned happy_var_2-}
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyMonadReduce 2 7 happyReduction_15
happyReduction_15 ((HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_16 = happyMonadReduce 2 7 happyReduction_16
happyReduction_16 ((HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_17 = happyReduce 7 7 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 7 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 ({-TRef happy_var_1-}
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyMonadReduce 3 8 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_2 TInt)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_21 = happyMonadReduce 6 8 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_2 TInt)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_22 = happyMonadReduce 6 9 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_2 (TStruct $ (\(s,_,_)->s) happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_23 = happyMonadReduce 6 10 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_2 (TUnion $ (\(s,_,_)->s) happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn10 r))

happyReduce_24 = happyReduce 7 11 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 11 happyReduction_25
happyReduction_25 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (
	) `HappyStk` happyRest

happyReduce_26 = happyMonadReduce 1 12 happyReduction_26
happyReduction_26 ((HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_1 FunGlob)
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_27 = happyMonadReduce 1 13 happyReduction_27
happyReduction_27 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify (\(t,s,s2)->(T.enterScope t,s,s2)))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_28 = happyMonadReduce 0 14 happyReduction_28
happyReduction_28 (happyRest) tk
	 = happyThen (( modify (\(t,s,s2)->(T.exitScope t,s,s2)))
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 _
	_
	_
	 =  HappyAbsSyn15
		 (
	)

happyReduce_30 = happyMonadReduce 1 16 happyReduction_30
happyReduction_30 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify (\(t,s,s2)->(T.enterScope t,s,s2)))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_31 = happyMonadReduce 1 17 happyReduction_31
happyReduction_31 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify (\(t,s,s2)->(T.exitScope t,s,s2)))
	) (\r -> happyReturn (HappyAbsSyn17 r))

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (
	)

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (
	)

happyReduce_36 = happySpecReduce_2  18 happyReduction_36
happyReduction_36 _
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_37 = happySpecReduce_2  18 happyReduction_37
happyReduction_37 _
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_38 = happySpecReduce_2  18 happyReduction_38
happyReduction_38 _
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_39 = happySpecReduce_2  18 happyReduction_39
happyReduction_39 _
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_40 = happySpecReduce_2  18 happyReduction_40
happyReduction_40 _
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_41 = happySpecReduce_3  18 happyReduction_41
happyReduction_41 _
	_
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_42 = happyMonadReduce 3 18 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn18 r))

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 _
	_
	_
	 =  HappyAbsSyn18
		 (
	)

happyReduce_44 = happySpecReduce_1  18 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn18
		 (
	)

happyReduce_45 = happyReduce 9 19 happyReduction_45
happyReduction_45 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 5 20 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 5 21 happyReduction_47
happyReduction_47 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 7 21 happyReduction_48
happyReduction_48 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (
	) `HappyStk` happyRest

happyReduce_49 = happyMonadReduce 3 22 happyReduction_49
happyReduction_49 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_50 = happyMonadReduce 6 22 happyReduction_50
happyReduction_50 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_51 = happyMonadReduce 4 23 happyReduction_51
happyReduction_51 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_52 = happyMonadReduce 3 23 happyReduction_52
happyReduction_52 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn24
		 (
	)

happyReduce_54 = happySpecReduce_1  24 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn24
		 (
	)

happyReduce_55 = happySpecReduce_2  25 happyReduction_55
happyReduction_55 _
	_
	 =  HappyAbsSyn25
		 (
	)

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn25
		 (
	)

happyReduce_57 = happySpecReduce_2  25 happyReduction_57
happyReduction_57 _
	_
	 =  HappyAbsSyn25
		 (
	)

happyReduce_58 = happySpecReduce_1  25 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn25
		 (
	)

happyReduce_59 = happyMonadReduce 3 26 happyReduction_59
happyReduction_59 ((HappyTerminal (Var happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_3 TInt)
	) (\r -> happyReturn (HappyAbsSyn26 r))

happyReduce_60 = happyMonadReduce 2 26 happyReduction_60
happyReduction_60 ((HappyTerminal (Var happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ modifTabla happy_var_2 TInt)
	) (\r -> happyReturn (HappyAbsSyn26 r))

happyReduce_61 = happySpecReduce_3  27 happyReduction_61
happyReduction_61 _
	_
	_
	 =  HappyAbsSyn27
		 (
	)

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn27
		 (
	)

happyReduce_63 = happyReduce 9 28 happyReduction_63
happyReduction_63 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 8 28 happyReduction_64
happyReduction_64 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_69 = happyMonadReduce 1 29 happyReduction_69
happyReduction_69 ((HappyTerminal (Str happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ (\ins (t,s,s2)->if(not $ elem ins s2)then (t,s,ins:s2) else (t,s,s2)) $ (\(s,_,_)->s) happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn29 r))

happyReduce_70 = happyMonadReduce 1 29 happyReduction_70
happyReduction_70 ((HappyTerminal (Var happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( modify $ checkExist happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn29 r))

happyReduce_71 = happySpecReduce_1  29 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn29
		 (
	)

happyReduce_72 = happySpecReduce_3  30 happyReduction_72
happyReduction_72 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_73 = happySpecReduce_3  30 happyReduction_73
happyReduction_73 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_74 = happySpecReduce_3  30 happyReduction_74
happyReduction_74 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_75 = happySpecReduce_3  30 happyReduction_75
happyReduction_75 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_76 = happySpecReduce_3  30 happyReduction_76
happyReduction_76 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_77 = happySpecReduce_3  30 happyReduction_77
happyReduction_77 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_78 = happySpecReduce_3  30 happyReduction_78
happyReduction_78 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_79 = happySpecReduce_3  30 happyReduction_79
happyReduction_79 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_80 = happySpecReduce_3  30 happyReduction_80
happyReduction_80 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_81 = happySpecReduce_3  30 happyReduction_81
happyReduction_81 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_82 = happySpecReduce_3  30 happyReduction_82
happyReduction_82 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_83 = happySpecReduce_3  30 happyReduction_83
happyReduction_83 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_84 = happySpecReduce_3  30 happyReduction_84
happyReduction_84 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_85 = happySpecReduce_3  30 happyReduction_85
happyReduction_85 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_86 = happySpecReduce_3  30 happyReduction_86
happyReduction_86 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_87 = happySpecReduce_3  30 happyReduction_87
happyReduction_87 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_88 = happySpecReduce_3  30 happyReduction_88
happyReduction_88 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_89 = happySpecReduce_3  30 happyReduction_89
happyReduction_89 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_90 = happySpecReduce_3  30 happyReduction_90
happyReduction_90 _
	_
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_91 = happySpecReduce_2  30 happyReduction_91
happyReduction_91 _
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_92 = happySpecReduce_2  30 happyReduction_92
happyReduction_92 _
	_
	 =  HappyAbsSyn30
		 (
	)

happyReduce_93 = happySpecReduce_1  30 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn30
		 (
	)

happyNewToken action sts stk [] =
	action 78 78 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Var happy_dollar_dollar -> cont 31;
	Int happy_dollar_dollar -> cont 32;
	Float happy_dollar_dollar -> cont 33;
	Type happy_dollar_dollar -> cont 34;
	Function happy_dollar_dollar -> cont 35;
	Bool happy_dollar_dollar -> cont 36;
	Ref happy_dollar_dollar -> cont 37;
	If happy_dollar_dollar -> cont 38;
	While happy_dollar_dollar -> cont 39;
	For happy_dollar_dollar -> cont 40;
	Else happy_dollar_dollar -> cont 41;
	Union happy_dollar_dollar -> cont 42;
	Struct happy_dollar_dollar -> cont 43;
	Unsigned happy_dollar_dollar -> cont 44;
	Continue happy_dollar_dollar -> cont 45;
	Break happy_dollar_dollar -> cont 46;
	Return happy_dollar_dollar -> cont 47;
	Read happy_dollar_dollar -> cont 48;
	Write happy_dollar_dollar -> cont 49;
	Str happy_dollar_dollar -> cont 50;
	ShiftL happy_dollar_dollar -> cont 51;
	ShiftR happy_dollar_dollar -> cont 52;
	Punto happy_dollar_dollar -> cont 53;
	Igual happy_dollar_dollar -> cont 54;
	MayorIgual happy_dollar_dollar -> cont 55;
	MenorIgual happy_dollar_dollar -> cont 56;
	Diferente happy_dollar_dollar -> cont 57;
	BoolOr happy_dollar_dollar -> cont 58;
	BoolAnd happy_dollar_dollar -> cont 59;
	Asignacion happy_dollar_dollar -> cont 60;
	Mas happy_dollar_dollar -> cont 61;
	Menos happy_dollar_dollar -> cont 62;
	Por happy_dollar_dollar -> cont 63;
	Entre happy_dollar_dollar -> cont 64;
	OpenParen happy_dollar_dollar -> cont 65;
	CloseParen happy_dollar_dollar -> cont 66;
	OpenCurly happy_dollar_dollar -> cont 67;
	CloseCurly happy_dollar_dollar -> cont 68;
	PuntoComa happy_dollar_dollar -> cont 69;
	Modulo happy_dollar_dollar -> cont 70;
	Menor happy_dollar_dollar -> cont 71;
	Mayor happy_dollar_dollar -> cont 72;
	BitAnd happy_dollar_dollar -> cont 73;
	BitOr happy_dollar_dollar -> cont 74;
	OpenBrackets happy_dollar_dollar -> cont 75;
	CloseBrackets happy_dollar_dollar -> cont 76;
	Coma happy_dollar_dollar -> cont 77;
	_ -> happyError' (tk:tks)
	}

happyError_ 78 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => State (T.Tabla String TypeData,[String],[String]) a -> (a -> State (T.Tabla String TypeData,[String],[String]) b) -> State (T.Tabla String TypeData,[String],[String]) b
happyThen = ((>>=))
happyReturn :: () => a -> State (T.Tabla String TypeData,[String],[String]) a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> State (T.Tabla String TypeData,[String],[String]) a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> State (T.Tabla String TypeData,[String],[String]) a
happyError' = parseError

parsefc tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError s = error ("Parse error in " ++ (lineCol $ head s))

data TypeData = FunGlob | FunLoc [TypeData] [TypeData] | TInt | TFloat| TBool | 
  TVoid | TChar | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq)

modifTabla (s,l,c) tipo (tabla,errores,strings) = if ((T.localLookup s tabla) == Nothing)
  then
    if ((T.lookup s tabla) /= Just FunGlob)
      then
        (T.insert s tipo tabla ,errores,strings)
      else
        (tabla,("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c)):errores,strings)
  else 
    (tabla,("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c)):errores,strings)

checkExist (s,l,c) (tabla,errores,strings) = if ((T.lookup s tabla) == Nothing)
  then
    (tabla,("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c)):errores,strings)
  else 
    (tabla,errores,strings)

{-
{ Plus $1 $3 }
{ Minus $1 $3 }
{ Times $1 $3 }
{ Div $1 $3 }
{ $2 }
{ Negate $2 }
 { (\(n,_,_)->Val n) $1 }

data Exp = Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Brack Exp
         | Val Int
         deriving Show
-}
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
