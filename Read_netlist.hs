---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Read_netlist.hs
--Note: Read Netlist From InputFile & Generate [Device]
--Designers: He Chenlong
--Writers: He Chenlong
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
module Read_netlist where

import System.IO
import Data_define
import Basic_function

read_device :: FilePath -> IO [String]
read_device filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let linelist = lines contents
    return linelist


set_device :: String -> Device
set_device line = ( let wordlist = words line in Device { 
                                 inst_name        = wordlist !! 0
                               , inst_type        = string_to_insttype (wordlist !! 5)
                               , source_pin       = ( SOURCE , wordlist !! 3 )
                               , drain_pin        = ( DRAIN  , wordlist !! 1 )
                               , gate_pin         = ( GATE   , wordlist !! 2 )
                               , bulk_pin         = ( BULK   , wordlist !! 4 )
                               , inst_len         = wordlist !! 6
                               , inst_wid         = wordlist !! 7
                               , inst_multi       = wordlist !! 8
                       } )

set_devices :: [String] -> [Device]
set_devices [] = []
set_devices (n : ns) = set_device n : set_devices ns
