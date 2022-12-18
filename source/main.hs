---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: main.hs
--Note: main of HMirr
--Designers: He Chenlong, WangChuyu
--Writers: He Chenlong, Wang Chuyu
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
module Main where
import Basic_function
import Data_define
import Read_netlist
import Write_result
import Current_mirror
import Differential_pair
import Symmetry_pair
import System.IO
import System.Environment
main = do
    -- get args
    inputfilepath <- getEnv "HMirrInputPath"
    outputfilepath <- getEnv "HMirrOutputPath"
    -- read information from input file
    linelist <- read_device inputfilepath
    -- main process
    let devices = set_devices linelist
    let mirlist = current_mirror_extract devices
    let diflist = differential_pair_extract devices mirlist
    let symlist = symmetry_pair_extract devices mirlist
    -- write result to output file
    write_all outputfilepath mirlist diflist symlist
    return mirlist
