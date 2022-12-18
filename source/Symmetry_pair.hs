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
import Differential_pair
import GHC.Read (list)

find_sym_nets :: (Device, Device) -> [(String, String)]
find_sym_nets (inst1, inst2)
    = filter (not . string_eq) [(snd (source_pin inst1), snd (source_pin inst2)),
                 (snd (drain_pin inst1), snd (drain_pin inst2)),
                 (snd (gate_pin inst1), snd (gate_pin inst2)),
                 (snd (bulk_pin inst1), snd (bulk_pin inst2))]

find_device_from_net :: String -> [Device] -> [[Device]]
find_device_from_net "gnd" netlists = []
find_device_from_net "vdd" netlists = []
find_device_from_net "GND" netlists = []
find_device_from_net "VDD" netlists = []
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
    [(inst1, inst2) | inst1 <- x, inst2 <- y, not (string_eq ((inst_name inst1, inst_name inst2)))]
    ++ find_sym_pair (xs, ys)

device_list_filter :: (Device, Device) -> [(Device, Device)] -> Bool
device_list_filter devicePair checkedList = 
    (mos_match devicePair) && 
    (not (device_pair_in_list devicePair checkedList)) &&
    (mos_connect_eq devicePair)

symmetry_pair_list :: [Device] -> [(Device, Device)] -> [(Device, Device)] -> [(Device, Device)]
symmetry_pair_list netlists [] checkedList = []
symmetry_pair_list netlists differentialList checkedList = do
    let netPairList = concat (map find_sym_nets differentialList)
    let rawDeviceList = concat (map find_sym_pair (net_pair_to_device_pair netlists netPairList))
    let nextDeviceList = device_pair_self_filter [x | x <- rawDeviceList, device_list_filter x checkedList] []
    let updatedCheckedList = checkedList ++ nextDeviceList
    nextDeviceList ++ symmetry_pair_list netlists nextDeviceList updatedCheckedList

symmetry_pair_extract :: [Device] -> [MosGroup] -> [MosGroup]
symmetry_pair_extract netlist mirlist = do
    let difList = differential_pair_list netlist mirlist
    map gen_mosgroup (symmetry_pair_list netlist difList difList)

print_debug_string :: [(String, String)] -> IO ()
print_debug_string [] = print ""
print_debug_string ((net1, net2):list) = do
    print(net1 ++ ", " ++ net2)
    print_debug_string list

print_debug_device :: [(Device, Device)] -> IO ()
print_debug_device [] = print ""
print_debug_device ((inst1, inst2):list) = do
    print(inst_name inst1 ++ ", " ++ inst_name inst2)
    print_debug_device list

symmetry_pair_debug :: [Device] -> [(Device, Device)] -> [(Device, Device)] -> IO()
symmetry_pair_debug netlists differentialList checkedList = do
    let netPairList = concat (map find_sym_nets differentialList)
    let rawDeviceList = concat (map find_sym_pair (net_pair_to_device_pair netlists netPairList))
    --print_debug_device rawDeviceList
    let nextDeviceList = device_pair_self_filter [x | x <- rawDeviceList, device_list_filter x checkedList] []
    let updatedCheckedList = checkedList ++ nextDeviceList
    --symmetry_pair_debug netlists nextDeviceList updatedCheckedList
    print_debug_device nextDeviceList
    print_debug_device updatedCheckedList