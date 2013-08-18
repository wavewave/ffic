import Control.Monad.Identity
import Control.Monad.State
import FFIC


main = do 
  putStrLn "Testing"

  let typ1 = Ptr (Ref (PrimType (CPTComposite "test"))) :: CPPType String 

      (a,s) = runIdentity ( runStateT (makeCTypeFromCPPType typ1) id )

  print a 
  print (s [])
