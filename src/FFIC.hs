{-# LANGUAGE StandaloneDeriving #-}

module FFIC where 

import Control.Applicative 
import Control.Monad.State

data NonComposite = CPTChar | CPTInt | CPTLong | CPTUChar | CPTULong | CPTLongLong | CPTULongLong 
                  | CPTDouble | CPTLongDouble 
                  | CPTBool | CPTVoid 
                    deriving Show 

data CPPType c = Ptr (CPPType c) 
               | Ref (CPPType c) 
               | PrimType (PrimitiveTypes c) 

deriving instance (Show c) => Show (CPPType c)


data PrimitiveTypes c = CPTNonComposite NonComposite
                      | CPTComposite c 

deriving instance (Show c) => Show (PrimitiveTypes c)


data CType c = Ptr_C (CType c) 
             | Prim_C (PrimCTypes c)

deriving instance (Show c) => Show (CType c)


data PrimCTypes c = CTypNonComposite NonComposite 
                  | CTypOpaque c  

deriving instance (Show c) => Show (PrimCTypes c) 

data ConversionPrim = Id | Opaqueify | AddPtr | ElimPtr | ChangeRefToPtr deriving Show 

-- deriving instance (Show c) => Show (ConversionPrim c)

type Conversion = [ConversionPrim] -> [ConversionPrim]  

makeCTypeFromCPPType :: (Functor m, Monad m) => CPPType c -> StateT Conversion m (CType c) 
makeCTypeFromCPPType (Ptr (PrimType (CPTComposite x))) = modify ( . (Opaqueify :)) 
                                                         *> pure (Prim_C (CTypOpaque x))
makeCTypeFromCPPType (Ptr x) = modify ( . (AddPtr :)) >> Ptr_C <$> makeCTypeFromCPPType x
makeCTypeFromCPPType (Ref x) = modify ( . (ChangeRefToPtr :)) *>makeCTypeFromCPPType (Ptr x) 
makeCTypeFromCPPType (PrimType (CPTComposite x)) = modify ( . (Opaqueify :)) 
                                                   *> pure (Prim_C (CTypOpaque x))
makeCTypeFromCPPType (PrimType (CPTNonComposite x)) = return (Prim_C (CTypNonComposite x))



 

