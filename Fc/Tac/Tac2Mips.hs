module Fc.Tac.Tac2Mips (
basics
) where

import      qualified     Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Fc.Tac.Tac
import Data.Foldable (toList)
import Data.Maybe (maybe)
import qualified Fc.Tabla as T
import qualified Fc.Tac.Tac as T (BiOperators(GT))
import Debug.Trace

data MyData = MyData {
regmap :: Map.Map Registro Int,
funName :: String,
maxframe :: Int,
regtam :: Int
--tabla :: T.Tabla String (TypeData,Offset)
}

basics :: TacIns -> String
basics (TacIns s) = concat $ map concat resul
  where
    l = toList s
    chunked = funChunks l
    help (l,m) = map (toCode m) l
    resul = map help chunked

funChunks :: [Tac] -> [([Tac],MyData)]
funChunks [] = []
funChunks l = (l2,midata{maxframe=frame2,regtam=reg}):funChunks resto
  where
    ((l2,midata),resto) = nextBlock l Map.empty [] 0
    tam = minimum $ concatMap getFrame l2
    frame = abs $ ( if tam >0 then 0 else tam )
    frame2 = if frame==0 then 4 else frame
    reg = (maximum $ Map.elems $ regmap midata)+4

nextBlock :: [Tac] -> Map.Map Registro Int -> [Tac] -> Int -> (([Tac],MyData),[Tac])
nextBlock (e@(Epilogo _ s):l) m l2 _ = (((reverse $ e:l2),MyData m s 0 0),l)
nextBlock (ins:l) m l2 i = nextBlock l m2 (ins:l2) i2
  where
    regs = getRegs ins
    (m2,i2) = foldr help (m,i) regs
    help r (m,i) = if Map.member r m then (m,i) else (Map.insert r i m,i+4)

getRegs :: Tac -> [Registro]
getRegs (IntBiOp _ _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (UIntBiOp _ _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (FloatBiOp _ _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (BoolBiOp _ _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (UnaryOp _ _ v1 v2) = concatMap value2reg [v1,v2]
getRegs (Copy _ v1 v2) = concatMap value2reg [v1,v2]
getRegs (Gotoz _ _ v1) = concatMap value2reg [v1]
getRegs (Gotonz _ _ v1) = concatMap value2reg [v1]
getRegs (Parametro _ v1) = concatMap value2reg [v1]
getRegs (Call _ _ _ v1) = concatMap value2reg [v1]
getRegs (Copy1 _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (Copy2 _ v1 v2 v3) = concatMap value2reg [v1,v2,v3]
getRegs (Copy3 _ v1 v2) = concatMap value2reg [v1,v2]
getRegs (Copy4 _ v1 v2) = concatMap value2reg [v1,v2]
getRegs (Copy5 _ v1 v2) = concatMap value2reg [v1,v2]
getRegs (ReturnW _ v1) = concatMap value2reg [v1]
getRegs (TacParam _ v1) = concatMap value2reg [v1]
getRegs _ = []

value2reg (Val r) = [r]
value2reg _ = []

getFrame :: Tac -> [Int]
getFrame (IntBiOp _ _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (UIntBiOp _ _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (FloatBiOp _ _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (BoolBiOp _ _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (UnaryOp _ _ v1 v2) = concatMap value2frame [v1,v2]
getFrame (Copy _ v1 v2) = concatMap value2frame [v1,v2]
getFrame (Gotoz _ _ v1) = concatMap value2frame [v1]
getFrame (Gotonz _ _ v1) = concatMap value2frame [v1]
getFrame (Parametro _ v1) = concatMap value2frame [v1]
getFrame (Call _ _ _ v1) = concatMap value2frame [v1]
getFrame (Copy1 _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (Copy2 _ v1 v2 v3) = concatMap value2frame [v1,v2,v3]
getFrame (Copy3 _ v1 v2) = concatMap value2frame [v1,v2]
getFrame (Copy4 _ v1 v2) = concatMap value2frame [v1,v2]
getFrame (Copy5 _ v1 v2) = concatMap value2frame [v1,v2]
getFrame (ReturnW _ v1) = concatMap value2frame [v1]
getFrame (TacParam _ v1) = concatMap value2frame [v1]
getFrame _ = []

value2frame (FramePtr i) = [i]
value2frame _ = []

codeforv reg (Val r) m = "addiu "++reg++",$a3,"++place++"\nlw "++reg++",("++reg++")\n"
  where
    place = maybe "error" show $ Map.lookup r m
--codeforv reg (BasePtr Int) m =
--codeforv reg (FramePtr i) m = "addiu "++reg++",$fp,"++show i++"\nlw "++reg++",("++reg++")\n"
codeforv reg (FramePtr i) m = "addiu "++reg++",$fp,"++show i++"\n"
codeforv reg (BasicInt i) m = "li "++reg++","++show i++"\n"
--codeforv reg (BasicFloat f) m =
codeforv reg (BasicBool b) m = "li "++reg++","++if b then "1\n" else "0\n"
codeforv _ _ _ = "error\n"



toCode :: MyData -> Tac -> String
toCode m (IntBiOp eti oper v1 v2 (Val resul)) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    codeforv "$t1" v2 (regmap m)++instruc oper++"addiu $t1,$a3,"++place++"\nsw $t0,($t1)\n"
  where
    instruc Mas = "add $t0,$t0,$t1\n"
    instruc Menos = "sub $t0,$t0,$t1\n"
    instruc Por = "mul $t0,$t0,$t1\n"
    instruc Mod = "div $t0,$t1\nmfhi $t0\n"
    instruc Entre = "div $t0,$t1\nmflo $t0\n"
    instruc T.GT = "sgt $t0,$t0,$t1\n"
    instruc Lt = "slt $t0,$t0,$t1\n"
    instruc Eq = "seq $t0,$t0,$t1\n"
    instruc EGT = "sge $t0,$t0,$t1\n"
    instruc ELT = "sle $t0,$t0,$t1\n"
    instruc NEq = "sne $t0,$t0,$t1\n"
    instruc _ = "error\n"
    place = maybe "error" show $ Map.lookup resul (regmap m)
toCode m (BoolBiOp eti oper v1 v2 (Val resul)) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    codeforv "$t1" v2 (regmap m)++instruc oper++"addiu $t1,$a3,"++place++"\nsw $t0,($t1)\n"
  where
    instruc T.GT = "sgt $t0,$t0,$t1\n"
    instruc Lt = "slt $t0,$t0,$t1\n"
    instruc Eq = "seq $t0,$t0,$t1\n"
    instruc EGT = "sge $t0,$t0,$t1\n"
    instruc ELT = "sle $t0,$t0,$t1\n"
    instruc NEq = "sne $t0,$t0,$t1\n"
    instruc And = "and $t0,$t0,$t1\n"
    instruc Or = "or $t0,$t0,$t1\n"
    instruc _ = "error\n"
    place = maybe "error" show $ Map.lookup resul (regmap m)


toCode m (Goto eti eti2) = "fc"++show eti ++":\tj fc"++show eti2++"\n"
toCode m (Gotoz eti eti2 v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    "beqz $t0, fc"++show eti2++"\n"
toCode m (Gotonz eti eti2 v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    "bnez $t0, fc"++show eti2++"\n"


toCode m (Copy eti (Val resul) v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    "addiu $t1,$a3,"++place++"\nsw $t0,($t1)\n"
  where
    place = maybe "error" show $ Map.lookup resul (regmap m)
toCode m (Copy4 eti (Val resul) v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    "lw $t0,($t0)\naddiu $t1,$a3,"++place++"\nsw $t0,($t1)\n"
  where
    place = maybe "error" show $ Map.lookup resul (regmap m)
toCode m (Copy5 eti v2 v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
    codeforv "$t1" v2 (regmap m)++"sw $t0,($t1)\n"

toCode m (Return eti) = "fc"++show eti ++":\tj __"++funName m++"\n"
toCode m (ReturnW eti v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
  "sw $t0,8($fp)\nj __"++funName m++"\n"

toCode m (TacParam eti v1) = "fc"++show eti ++":\t"++codeforv "$t0" v1 (regmap m)++
  "sw $t0, ($sp)\nsubiu $sp, $sp, 4\n"

toCode m (Call eti s tam (Val resul)) = "fc"++show eti ++":\t"++
  "sw $a3, -4($fp)\njal "++s++"\naddiu $sp, $fp, "++show (8+(tam*4))++
  "\nlw $t0, 8($fp)\nlw $fp, ($fp)\nlw $a3, -4($fp)"++
  "\naddiu $t1,$a3,"++place++"\nsw $t0,($t1)\n"
  where
    place = maybe "error" show $ Map.lookup resul (regmap m)

toCode m (Prologo eti s) = "fc"++show eti ++":\n"++ s ++":\n"++
  "sw $0, ($sp)\nsw $ra, -4($sp)\nsw $fp, -8($sp)\nsubiu $fp, $sp, 8\nsubiu $sp, $sp, 12\n"
  ++"subiu $sp, $sp, "++show ((maxframe m)+(regtam m))++"\naddiu $a3,$sp,4\n"

toCode m (Epilogo eti s) = "fc"++show eti ++":\n__"++ s ++
  ":\nlw $ra, 4($fp)\njr $ra\n"

toCode m (Special eti "fcLlamadaIntWrite") = "fc"++show eti ++":\taddiu $sp,$sp,4\n"++
  "lw $a0,($sp)\nli $v0, 1\nsyscall\n"

toCode m (Special eti "fcLlamadaIntRead") = "fc"++show eti ++":\tli $v0, 5\n"++
  "syscall\naddiu $sp,$sp,4\nlw $t0,($sp)\nsw $v0,($t0)\n"

toCode _ _ = ""
--toCode t = "=============\n" ++ show t ++ "\n=============\n"













--
