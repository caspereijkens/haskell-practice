module Lib 
  ( calculate
  , add
  , subtract
  , multiply
  , divide
  ) where

-- Basic operations
add :: Double -> Double -> Double
add x y = x + y

sub :: Double -> Double -> Double
sub x y = x - y

multiply :: Double -> Double -> Double
multiply x y = x * y

divide :: Double -> Double -> Double
divide _ 0 = error "Division by zero"
divide x y = x / y

-- Calculator function
calculate :: String -> Double -> Double -> Double
calculate op x y = 
  case op of
    "+" -> add x y
    "-" -> sub x y
    "*" -> multiply x y
    "/" -> divide x y
    ""  -> error "Empty operator"  -- Handles empty string case
    _   -> error $ "Unknown operator: " ++ op  -- Handles all other strings
