---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Differential_pair.hs
--Note: Extract differential mos pairs from Netlist [Device]
--Designers: Wang Chuyu
--Writers: Wang Chuyu
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

module Differential_pair where
import Data_define
import Basic_function

device_in_mosgroup_list :: Device -> [MosGroup] -> Bool
device_in_mosgroup_list inst [] = False
device_in_mosgroup_list inst (group:groupList) = 
    string_eq (inst_name inst, diode_name group) ||
    string_eq (inst_name inst, mir_name group) ||
    device_in_mosgroup_list inst groupList

is_differential_pair :: Device -> Device -> [MosGroup] -> Bool
is_differential_pair inst1 inst2 mirlist =
    (not (device_in_mosgroup_list inst1 mirlist)) &&
    (not (device_in_mosgroup_list inst2 mirlist)) &&
    (inst_type inst1 == inst_type inst2) &&
    (snd (source_pin inst1) == snd (source_pin inst2)) &&
    (snd (bulk_pin inst1) == snd (bulk_pin inst2)) &&
    (inst_len inst1 == inst_len inst2) &&
    (inst_wid inst1 == inst_wid inst2) &&
    (inst_multi inst1 == inst_multi inst2) &&
    (snd (drain_pin inst1) /= snd (gate_pin inst1)) && 
    (snd (drain_pin inst2) /= snd (gate_pin inst2)) &&
    (snd (drain_pin inst1) /= snd (source_pin inst1)) &&
    (snd (drain_pin inst2) /= snd (source_pin inst2)) &&
    (snd (drain_pin inst1) /= snd (drain_pin inst2)) &&
    (snd (gate_pin inst1) /= snd (gate_pin inst2))

differential_pair_list :: [Device] -> [MosGroup] -> [(Device, Device)]
differential_pair_list [] mirlist = []
differential_pair_list (inst:instlist) mirlist=
    [(inst, anotherinst) | anotherinst <- instlist, is_differential_pair inst anotherinst mirlist] ++ differential_pair_list instlist mirlist

differential_pair_extract :: [Device] -> [MosGroup] -> [MosGroup]
differential_pair_extract devices mirlist = 
    map gen_mosgroup (differential_pair_list devices mirlist)