import Control.Monad.Identity
import Control.Monad.State
import FFIC


main = do 
  putStrLn "Testing"

  let typ1 = CPtr (CRef (CSimple (SOpaq "test"))) :: Composite String 
      typ2 = CPtr (CSimple (SPrim PrimChar)) :: Composite String
      (a,s) = runIdentity ( runStateT (makeCTypeFromCPPType typ1) id )
      (a',s') = runIdentity ( runStateT (makeCTypeFromCPPType typ2) id )
  print a 
  print (s [])

  print a' 
  print (s' []) 
