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

is_differential_pair :: Device -> Device -> Bool
is_differential_pair inst1 inst2 =
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

differential_pair_list :: [Device] -> [(Device, Device)]
differential_pair_list [] = []
differential_pair_list (inst:instlist) 
    = [(inst, anotherinst) | anotherinst <- instlist, is_differential_pair inst anotherinst] ++ differential_pair_list instlist

differential_pair_extract :: [Device] -> [MosGroup]
differential_pair_extract devices
    = map gen_mosgroup (differential_pair_list devices)