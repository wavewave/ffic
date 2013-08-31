import Control.Monad.Identity
import Control.Monad.State
import FFIC


main = do 
  putStrLn "Testing"

  let typ1 = CPtr (CPtr (CSimple (SOpaq "A"))) :: Composite String 
      typ2 = CPtr (CSimple (SPrim PrimChar)) :: Composite String
      typ3 = (CSimple (SPrim PrimInt)) :: Composite String 

      -- v1 = Var typ1 "a" 
      -- v2 = Var typ2 "b" 

      (a,s) = runIdentity ( runStateT (project typ1) id )
      (a',s') = runIdentity ( runStateT (project typ2) id )
      (a'',s'') = runIdentity (runStateT (project typ3) id) 

      v1 = Var a "a" 
      v2 = Var a' "b"

      f = Function "test" [v1,v2] a''
  
      t1 = mkCPPair typ1
      t2 = mkCPPair typ2 
      vt1 = Var t1 "x"
      vt2 = Var t2 "y"

  print a 
  print (s [])
  putStrLn (mkCTypeFromProjected a)


  print a' 
  print (s' []) 
  putStrLn (mkCTypeFromProjected a')

  putStrLn (mkArgs [v1,v2])
  putStrLn (mkFuncDecl f)
  
  mapM_ putStrLn (mkOpaqueTypedef "testtest")

  putStrLn (mkCallArg vt1) 
