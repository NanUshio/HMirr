module Main where

import Basic_function
import Data_define
import Read_netlist
import Write_result
import Current_mirror
import Differential_pair
main = do
    --putStrLn "Please enter Netlist File Path : "
    --filepath <- getLine
    --linelist <- read_device filepath
    linelist <- read_device "./netlist"
    let devices = set_devices linelist
    --show_devices devices
    let mirlist = current_mirror_extract devices
    let diflist = differential_pair_extract devices
    write_result "./current_mirror.txt" mirlist
    write_result "./differential_pair.txt" diflist
    return mirlist