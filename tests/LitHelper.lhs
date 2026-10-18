\documentclass{article}
\begin{document}
A module in LaTeX style: the code is between begin and end markers.

\begin{code}
module LitHelper (squares) where

squares :: Int -> [Int]
squares n = [x * x | x <- [1 .. n]]
\end{code}

\end{document}
