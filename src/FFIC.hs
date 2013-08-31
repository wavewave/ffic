{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module FFIC where 

import Control.Applicative 
import Control.Monad.Identity
import Control.Monad.State
import Data.List (intercalate)

data Primitive = PrimChar 
               | PrimInt 
               | PrimLong 
               | PrimUChar 
               | PrimULong 
               | PrimLongLong 
               | PrimULongLong 
               | PrimDouble 
               | PrimLongDouble 
               | PrimBool 
               | PrimVoid 
                deriving Show 

data Simple c = SPrim Primitive
              | SOpaq c 

deriving instance (Show c) => Show (Simple c)

-- | type representing full derived structure
data Composite c = CPtr (Composite c) 
                 | CRef (Composite c) 
                 | CSimple (Simple c) 

deriving instance (Show c) => Show (Composite c)

-- | type for simplifying class, class reference and class pointer
data Projected c = PPtr (Projected c) 
                 | PSimple (Simple c)

deriving instance (Show c) => Show (Projected c)


data Function t = Function { funcName :: String
                             , funcArgs :: [Var t] 
                             , funcRet  :: t }
  


data ConvPrim c = GetPrim Primitive 
                | Opaqueify c
                | AddPtr 
                | ElimPtr   
                | ChangeRefToPtr deriving Show 

-- deriving instance (Show c) => Show (ConversionPrim c)

type Conversion c = [ConvPrim c] -> [ConvPrim c]  

-- | project composite type to projected type
project :: (Functor m, Monad m) => Composite c -> StateT (Conversion c) m (Projected c) 
project (CPtr (CSimple (SOpaq x))) = modify (. (Opaqueify x :)) *> pure (PSimple (SOpaq x))
project (CPtr x) = modify ( . (AddPtr :)) >> PPtr <$> project x
project (CRef x) = modify ( . (ChangeRefToPtr :)) *> project (CPtr x) 
project (CSimple (SOpaq x)) = modify ( . (Opaqueify x :)) *> pure (PSimple (SOpaq x))
project (CSimple (SPrim x)) = modify ( . (GetPrim x :)) *> pure (PSimple (SPrim x))

-- | type with both before and after conversion  
data CompProjPair c = CPPair { cp_before :: Composite c
                             , cp_after :: Projected c 
                             , cp_conv :: Conversion c } 

mkCPPair :: Composite c -> CompProjPair c 
mkCPPair x = let (t,c) = runIdentity (runStateT (project x) id)
             in CPPair x t c



-- | 
mkCTypeFromPrimitive :: Primitive -> String 
mkCTypeFromPrimitive PrimChar       = "char"
mkCTypeFromPrimitive PrimInt        = "int"
mkCTypeFromPrimitive PrimLong       = "long"
mkCTypeFromPrimitive PrimUChar      = "unsigned char"
mkCTypeFromPrimitive PrimULong      = "unsigned long"
mkCTypeFromPrimitive PrimLongLong   = "long long"
mkCTypeFromPrimitive PrimULongLong  = "unsigned long long"
mkCTypeFromPrimitive PrimDouble     = "double"
mkCTypeFromPrimitive PrimLongDouble = "long double"
mkCTypeFromPrimitive PrimBool       = "bool"
mkCTypeFromPrimitive PrimVoid       = "void"



-- | 
mkCTypeFromProjected :: Projected String -> String
mkCTypeFromProjected (PSimple (SPrim p)) = mkCTypeFromPrimitive p 
mkCTypeFromProjected (PSimple (SOpaq x)) = x ++ "_p" 
mkCTypeFromProjected (PPtr x) = "(" ++  mkCTypeFromProjected x ++ "*)"


-- | 
data Var t = Var { varType :: t, varName :: String }

-- | 
class Nameable a where 
  name :: a -> String


-- |
class CTypeable a where
  mkCType :: a -> String 

-- | 
class CallArguable a where 
  mkCallArg :: a -> String 
   

instance CTypeable (Projected String) where
  mkCType = mkCTypeFromProjected 

instance CTypeable Primitive where
  mkCType = mkCTypeFromPrimitive 

instance CallArguable (Var (CompProjPair String)) where 
  mkCallArg v = let t = varType v 
                    n = varName v
                in mkConvStr n t 
               

instance Nameable String where 
  name = show 

mkConvStr :: String -> CompProjPair String -> String 
mkConvStr varname p = let r = (cp_conv p) []  
                      in execState (mapM_ (modify . flip mkConvStrFromConvPrim) r) varname  



mkConvStrFromConvPrim :: String -> ConvPrim String -> String 
mkConvStrFromConvPrim s AddPtr = "(*" ++ s ++ ")"
mkConvStrFromConvPrim s ElimPtr = undefined 
mkConvStrFromConvPrim s ChangeRefToPtr = undefined 
mkConvStrFromConvPrim s (Opaqueify c) = "reinterpret_cast<" ++ c ++ "*> " ++ s
mkConvStrFromConvPrim _ _ = undefined


-- | construct a declaration statement from variable type and name pair
mkVarDecl :: (CTypeable t) => Var t -> String
mkVarDecl v = (mkCType . varType) v ++ " " ++ (varName v)

-- | construct argument string 
mkArgs :: (CTypeable t) => [Var t] -> String 
mkArgs = intercalate ", " . map mkVarDecl 

-- | function declaration
mkFuncDecl :: (CTypeable t) => Function t -> String 
mkFuncDecl f = let retstr = (mkCType . funcRet) f 
                   argstr = (mkArgs . funcArgs) f 
               in retstr ++ " " ++ (funcName f) ++ "(" ++ argstr ++ ")" 


-- | opaqueification string 
mkOpaqueTypedef :: String -> [String] 
mkOpaqueTypedef name = let tag = name ++ "_tag" 
                           optyp = name ++ "_t"
                           opptr = name ++ "_p" 
                           opcptr = "const_" ++ name ++ "_p"
                       in [ "typedef struct " ++ tag ++ " " ++ optyp 
                          , "typedef " ++ optyp++"* " ++ opptr
                          , "typedef " ++ optyp++" const* " ++ opcptr ]

 
