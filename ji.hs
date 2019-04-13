module Main where
import J
import Control.Monad
main = do
    line <- getLine
    putStrLn(unwords$result1(mf line))
    getLine