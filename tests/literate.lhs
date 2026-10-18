A literate Haskell program: only the lines that start with '>' are code.

> import LitHelper (squares)
>
> main :: IO ()
> main = do
>   print (squares 5)
>   putStrLn (greet "world")

Prose between code blocks is ignored, so it can say anything:
main = this is not code.

> greet :: String -> String
> greet name = "hello, " ++ name
