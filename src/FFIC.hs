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

data Composite c = CPtr (Composite c) 
                 | CRef (Composite c) 
                 | CSimple (Simple c) 

deriving instance (Show c) => Show (Composite c)

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

makeCTypeFromCPPType :: (Functor m, Monad m) => Composite c -> StateT (Conversion c) m (Projected c) 
makeCTypeFromCPPType (CPtr (CSimple (SOpaq x))) = modify ( . (Opaqueify x :)) 
                                                         *> pure (PSimple (SOpaq x))
makeCTypeFromCPPType (CPtr x) = modify ( . (AddPtr :)) >> PPtr <$> makeCTypeFromCPPType x
makeCTypeFromCPPType (CRef x) = modify ( . (ChangeRefToPtr :)) *>makeCTypeFromCPPType (CPtr x) 
makeCTypeFromCPPType (CSimple (SOpaq x)) = modify ( . (Opaqueify x :)) 
                                                   *> pure (PSimple (SOpaq x))
makeCTypeFromCPPType (CSimple (SPrim x)) = modify ( . (GetPrim x :)) 
                                                      *> pure (PSimple (SPrim x))



 

