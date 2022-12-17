module Write_result where
import System.IO
import Data_define
import Basic_function
import TysPrim (primTyCons)


write_mirgroup :: Handle -> [MosGroup] -> IO ()
write_mirgroup handle mirgrouplist
        | mirgrouplist /= [] = do        
                hPutStrLn  handle ( mir_to_string  ( head mirgrouplist ) )
                write_mirgroup handle ( tail mirgrouplist )
        | otherwise = do 
                hPutStrLn handle ""


write_result :: FilePath -> [MosGroup] -> IO ()
write_result filename mirgrouplist = do
    handle <- openFile filename WriteMode
    hPutStrLn handle "{"
    hPutStrLn handle ""
    write_mirgroup handle mirgrouplist
    hPutStrLn handle "}"
    hClose handle

