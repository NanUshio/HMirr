---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Symmetry_pair.hs
--Note: Extract symmetry mos pairs from Netlist [Device]
--Designers: Wang Chuyu
--Writers: Wang Chuyu
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

module Symmetry_pair where
import Data_define
import Basic_function

find_sym_nets :: (Device, Device) -> [(String, String)]
find_sym_nets (inst1, inst2)
    = filter string_eq [(snd (source_pin inst1), snd (source_pin inst2)),
                 (snd (drain_pin inst1), snd (drain_pin inst2)),
                 (snd (gate_pin inst1), snd (gate_pin inst2)),
                 (snd (bulk_pin inst1), snd (bulk_pin inst2))]

find_device_from_net :: String -> [Device] -> [[Device]]
find_device_from_net "gnd" netlists = [[]]
find_device_from_net "vdd" netlists = [[]]
find_device_from_net "GND" netlists = [[]]
find_device_from_net "VDD" netlists = [[]]
find_device_from_net net netlists = 
    [[x | x <- netlists, string_eq (snd(source_pin x), net) ],
     [x | x <- netlists, string_eq (snd(drain_pin x), net) ],
     [x | x <- netlists, string_eq (snd(gate_pin x), net) ],
     [x | x <- netlists, string_eq (snd(bulk_pin x), net) ]]

net_pair_to_device_pair :: [Device] -> [(String, String)] -> [([[Device]], [[Device]])]
net_pair_to_device_pair netlists [] = []
net_pair_to_device_pair netlists ((net1, net2):netPairs) = 
    (find_device_from_net net1 netlists, find_device_from_net net2 netlists) : net_pair_to_device_pair netlists netPairs


find_sym_pair :: ([[Device]], [[Device]]) -> [(Device, Device)]
find_sym_pair ([], []) = []
find_sym_pair ((x:xs), []) = []
find_sym_pair ([], (y:ys)) = []
find_sym_pair ((x:xs), (y:ys)) = 
    [(inst1, inst2) | inst1 <- x, inst2 <- y] ++ find_sym_pair (xs, ys)

device_list_filter :: (Device, Device) -> [(Device, Device)] -> Bool
device_list_filter devicePair checkedList = 
    (mos_match devicePair) && (device_pair_in_list devicePair checkedList)

symmetry_pair_list :: [Device] -> [(Device, Device)] -> [(Device, Device)] -> [(Device, Device)]
symmetry_pair_list netlists [] checkedList = []
symmetry_pair_list netlists differentialList checkedList = do
    let netPairList = concat (map find_sym_nets differentialList)
    let rawDeviceList = concat (map find_sym_pair (net_pair_to_device_pair netlists netPairList))
    let nextDeviceList = [x | x <- rawDeviceList, device_list_filter x checkedList]
    let updatedCheckedList = checkedList ++ nextDeviceList
    nextDeviceList ++ symmetry_pair_list netlists nextDeviceList updatedCheckedList