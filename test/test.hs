import Control.Monad.Identity
import Control.Monad.State
import FFIC


main = do 
  putStrLn "Testing"

  let typ1 = CPtr (CRef (CSimple (SOpaq "test"))) :: Composite String 
      typ2 = CPtr (CSimple (SPrim PrimChar)) :: Composite String
      (a,s) = runIdentity ( runStateT (project typ1) id )
      (a',s') = runIdentity ( runStateT (project typ2) id )
  print a 
  print (s [])
  putStrLn (mkCTypeFromProjected a)


  print a' 
  print (s' []) 
  putStrLn (mkCTypeFromProjected a')
