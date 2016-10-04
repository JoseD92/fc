{-# LANGUAGE DeriveGeneric #-}
module Fc.Datas (TypeData(..),operAcc,operAccMono,tam,Expre(..),Instruc(..),insToStr)
where

import qualified Fc.Tabla as T(Tabla(info,hijos))
import qualified Data.Map.Strict as Map
import Data.Maybe

data TypeData = FunGlob TypeData [TypeData] | FunLoc TypeData [TypeData] | TInt | TFloat| TBool | TAny | TError |
  TVoid | TChar | TArray Int TypeData | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Eq)

operAcc :: TypeData -> String -> TypeData -> TypeData
operAcc TError _ _ = TError
operAcc _ _ TError = TError
operAcc TAny _ x = x
operAcc x _ TAny = x
operAcc x _ y = if (x==y) then x else TError
operAcc _ _ _ = TError

operAccMono :: String -> TypeData -> TypeData
operAccMono _ TError = TError
operAccMono _ TAny = TAny
operAccMono _ x = x
operAccMono _ _ = TError

tam :: TypeData -> Map.Map TypeData Int -> Int
tam TInt _ = 4
tam TFloat _ = 4
tam (TRef _) _ = 4
tam TBool _ = 1
tam TChar _ = 1
tam (TArray i x) m = i*(tam x m)
tam (TUnsigned x) m = tam x m
tam s@(TStruct _) tamTable = fromJust $ Map.lookup s tamTable
tam s@(TUnion _) tamTable = fromJust $ Map.lookup s tamTable
tam _ _ = 0

instance Show TypeData where
  show (FunGlob _ _) = "Function"
  show (FunLoc _ _) = "Function"
  show (TInt) = "Int"
  show (TFloat) = "Float"
  show (TBool) = "Bool"
  show (TAny) = "Any"
  show (TError) = "Error"
  show (TVoid) = "Void"
  show (TChar) = "Char"
  show (TArray i x) = "Array("++(show i)++") of "++(show x)
  show (TUnion s) = "Union "++s
  show (TStruct s) = "Struct "++s
  show (TUnsigned x) = "Unsigned "++(show x)
  show (TRef x) = "Pointer to "++(show x)


----------------------tipos del ast

data Expre = ExpreBin String Expre Expre Int Int
  | ExpreMono String Expre Int Int
  | ExpreDeRef Expre Int Int
  | ExpreField String Expre Int Int
  | ExpreArr String [Expre] Int Int
  | ExpreLLamada String [Expre] Int Int
  | ExpreBasicInt Int Int Int
  | ExpreBasicFloat Float Int Int
  | ExpreBasicBool Bool Int Int
  | ExpreBasicStr String Int Int
  | ExpreNull

expreToStr i (ExpreBin s e1 e2 _ _) = init.unlines $ ((replicate i ' ')++s):k
  where k = ((expreToStr (i+2)) e1):((expreToStr (i+2)) e2):[]
expreToStr i (ExpreMono s e1 _ _) = init.unlines $ ((replicate i ' ')++s):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreField s e1 _ _) = init.unlines $ ((replicate i ' ')++"Field: "++s):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreDeRef e1 _ _) = init.unlines $ ((replicate i ' ')++"Deref"):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreArr s e1 _ _) = init.unlines $ ((replicate i ' ')++"Var: "++s):k
  where k = map (expreToStr (i+2)) e1
expreToStr i (ExpreLLamada s e1 _ _) = init.unlines $ ((replicate i ' ')++"Llamada: "++s):k
  where k = map (expreToStr (i+2)) e1
expreToStr i (ExpreBasicInt i1 _ _) = init.unlines $ ((replicate i ' ')++"Int: "++(show i1)):[]
expreToStr i (ExpreBasicFloat i1 _ _) = init.unlines $ ((replicate i ' ')++"Float: "++(show i1)):[]
expreToStr i (ExpreBasicBool i1 _ _) = init.unlines $ ((replicate i ' ')++"Bool: "++(show i1)):[]
expreToStr i (ExpreBasicStr s _ _) = init.unlines $ ((replicate i ' ')++"String: "++s):[]

data Instruc = InstrucBlock [Instruc]
  | InstrucFor Instruc Expre Instruc Instruc Int Int
  | InstrucWhile Expre Instruc Int Int
  | InstrucIf Expre Instruc Int Int
  | InstrucIfElse Expre Instruc Instruc Int Int
  | InstrucAsing Expre Expre Int Int
  | InstrucExpre Expre Int Int
  | InstrucBreak Int Int
  | InstrucCon Int Int
  | InstrucRet Int Int
  | InstrucRetW Expre Int Int
  | InstrucFun Instruc
  | InstrucNull

insToStr i (InstrucBlock l) = init.unlines $ ((replicate i ' ')++"Bloque"):k
  where k = map (insToStr (i+2)) l
insToStr i (InstrucFor i1 e i2 i3 _ _) = init.unlines $ ((replicate i ' ')++"For"):k
  where k = ((insToStr (i+2)) i1):((expreToStr (i+2)) e):((insToStr (i+2)) i2):((insToStr (i+2)) i3):[]
insToStr i (InstrucWhile e i1 _ _) = init.unlines $ ((replicate i ' ')++"While"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):[]
insToStr i (InstrucIf e i1 _ _) = init.unlines $ ((replicate i ' ')++"If"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):[]
insToStr i (InstrucIfElse e i1 i2 _ _) = init.unlines $ ((replicate i ' ')++"If"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):((insToStr (i+2)) i2):[]
insToStr i (InstrucAsing e1 e2 _ _) = init.unlines $ ((replicate i ' ')++"Asignacion"):k
  where k = ((expreToStr (i+2)) e1):((expreToStr (i+2)) e2):[]
insToStr i (InstrucExpre e _ _) = init.unlines $ ((replicate i ' ')++"Expre"):k
  where k = ((expreToStr (i+2)) e):[]
insToStr i (InstrucBreak  _ _) = init.unlines $ ((replicate i ' ')++"Break"):[]
insToStr i (InstrucCon  _ _) = init.unlines $ ((replicate i ' ')++"Continue"):[]
insToStr i (InstrucRet  _ _) = init.unlines $ ((replicate i ' ')++"Return"):[]
insToStr i (InstrucRetW e _ _) = init.unlines $ ((replicate i ' ')++"Return"):k
  where k = ((expreToStr (i+2)) e):[]
insToStr i (InstrucFun i1) = init.unlines $ ((replicate i ' ')++"Funcion"):k
  where k = ((insToStr (i+2)) i1):[]
insToStr i InstrucNull = ""
