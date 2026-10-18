import Control.Exception
main = do
  r <- try (evaluate (1 `div` (0 :: Int)))
  case r of
    Left (e :: SomeException) -> putStrLn ("caught: " ++ show e)
    Right v -> print v
  catch (error "oops" >> return ()) (\(e :: SomeException) -> putStrLn ("handled: " ++ displayException e))
  catch (ioError (userError "bad")) (\e -> putStrLn ("io: " ++ show (e :: SomeException)))
  let f (n :: Int) = n * 2
  print (f 21)
