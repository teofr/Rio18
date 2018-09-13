import Data.Char

ejercicio :: FilePath -> IO ()
ejercicio f = readFile	f  >>= putStrLn . ( map toUpper )