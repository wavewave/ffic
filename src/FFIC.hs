{-# LANGUAGE StandaloneDeriving #-}

module FFIC where 

import Control.Applicative 
import Control.Monad.State

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



 

