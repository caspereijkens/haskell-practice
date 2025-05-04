module Main (main) where

import Lib (calculate)

main :: IO ()
main = do
  putStrLn "Haskell Calculator"
  putStrLn "Enter operation (+, -, *, /) followed by two numbers:"

  input <- getLine
  case words input of
    [op, x, y] -> 
      case (reads x, reads y) of
        ([(x', "")], [(y', "")]) -> 
          putStrLn $ "Result: " ++ show (calculate op x' y')
        _ -> putStrLn "Error: Please enter valid numbers"
    
    _ -> putStrLn $ "Invalid input. Expected format: operator number number\n" ++
                    "Example: + 5 3"
