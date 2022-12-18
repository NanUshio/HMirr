---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Current_mirror.hs
--Note: Extract current mirror mos pairs from Netlist [Device]
--Designers: Wang Chuyu
--Writers: Wang Chuyu
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

module Current_mirror where
import Data_define
import Basic_function

is_diode :: Device -> Bool
is_diode inst
        | (inst_type inst == PMOS && snd (drain_pin inst) == snd (gate_pin inst)) = True
        | (inst_type inst == NMOS && snd (drain_pin inst) == snd (gate_pin inst)) = True
        | otherwise = False

diode_list :: [Device] -> [Device]
diode_list devices = filter is_diode devices

is_current_mirror :: Device -> Device -> Bool
is_current_mirror diode inst = (inst_type diode == inst_type inst) &&
                               (snd (gate_pin diode) == snd (gate_pin inst)) &&
                               (snd (source_pin diode) == snd (source_pin inst)) &&
                               (snd (drain_pin inst) /= snd (gate_pin inst))

current_mirror_list :: [Device] -> [Device] -> [(Device, Device)]
current_mirror_list [] devices = []
current_mirror_list (diode:diodelist) devices
    = [(diode, inst) | inst <- devices, is_current_mirror diode inst] ++ 
      current_mirror_list diodelist devices

current_mirror_extract :: [Device] -> [MosGroup]
current_mirror_extract devices
    = map gen_mosgroup (current_mirror_list(diode_list devices) devices) 