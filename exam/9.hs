import System.Environment
import System.IO
import Data.List.Split
import Data.Typeable(typeOf)

-- pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
--              asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh

chooseFunc :: String -> (Float -> Float)
chooseFunc name
    | name == "exp" = exp
    | name == "log" = log
    | name == "sqrt" = sqrt
    | name == "sin" = sin
    | name == "cos" = cos
    | name == "tan" = tan
    | name == "asin" = asin
    | name == "acos" = acos
    | name == "atan" = atan
    | name == "sinh" = sinh
    | name == "cosh" = cosh
    | name == "tanh" = tanh
    | name == "asinh" = asinh
    | name == "acosh" = acosh
    | name == "atanh" = atanh

calcTable :: (Float -> Float) -> Float -> Float -> Float -> [Float]
calcTable func a b h
    | a < b = (func a) : (calcTable func (a+h) b h)
    | otherwise = []

funPrint :: (Float -> Float) -> Float -> Float -> Float -> IO()
funPrint func a b h = do
    print (calcTable func a b h)


main = do
    args <- getArgs
    funPrint (chooseFunc (args !! 0)) (read (args !! 1) :: Float) (read (args !! 2) :: Float) (read (args !! 3) :: Float)
