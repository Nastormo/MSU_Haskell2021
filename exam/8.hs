import System.Environment
import Numeric



main = do
    x <- getArgs
    print (x)
    print (readFloat x)