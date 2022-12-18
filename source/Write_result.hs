---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Write_result.hs
--Note: write result to file
--Designers: He Chenlong
--Writers: He Chenlong
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
module Write_result where
import System.IO
import Data_define
import Basic_function


write_mirgroup :: Handle -> [MosGroup] -> IO ()
write_mirgroup handle mirgrouplist
        | mirgrouplist /= [] = do        
                hPutStrLn  handle ( mir_to_string  ( head mirgrouplist ) )
                write_mirgroup handle ( tail mirgrouplist )
        | otherwise = do 
                hPutStrLn handle ""


write_result :: Handle -> [MosGroup] -> IO ()
write_result handle mirgrouplist = do
    hPutStrLn handle "{"
    hPutStrLn handle ""
    write_mirgroup handle mirgrouplist
    hPutStrLn handle "}"

write_info :: Handle -> String -> IO ()
write_info handle info = do
    hPutStrLn handle info

write_all :: FilePath -> [MosGroup] -> [MosGroup] -> [MosGroup] -> IO ()
write_all filepath mirlist diflist symlist = do
    handle <- openFile filepath WriteMode
    write_info handle "Current Mirror List :"
    write_result handle mirlist
    write_info handle "Differental Pair List :"
    write_result handle diflist
    write_info handle "Symmetry Pair List :"
    write_result handle symlist
    hClose handle

