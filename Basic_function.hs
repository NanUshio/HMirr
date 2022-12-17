---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Basic_function.hs
--Note: define basic function
--Designers: He Chenlong
--Writers: He Chenlong, Wang Chuyu
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
module Basic_function where
import Data_define

string_to_insttype :: String -> InstType
string_to_insttype s 
    | s == "RES"  = RES
    | s == "PMOS" || s == "pch" = PMOS
    | s == "NMOS" || s == "nch" = NMOS
    | s == "CAP"  = CAPTOR
    | otherwise   = OTHER

show_device :: Device -> IO ()
show_device device = do
    print (inst_name       device)
    print (inst_type       device)
    print (source_pin      device)
    print (drain_pin       device)
    print (gate_pin        device)
    print (bulk_pin        device)
    print (inst_len        device)
    print (inst_wid        device)
    print (inst_multi      device)


show_devices :: [Device] -> IO b
show_devices devices = do
    show_device ( head devices )
    show_devices ( tail devices )

gen_mosgroup :: (Device, Device) -> MosGroup
gen_mosgroup (diode, inst) 
    = MosGroup { diode_name = inst_name diode , mir_name =  inst_name inst}

mir_to_string :: MosGroup -> String
mir_to_string mirgroup = "    {" ++ (diode_name mirgroup) ++ "  ,  " ++ (mir_name mirgroup) ++ "}    "

string_eq :: (String, String) -> Bool
string_eq ([], []) = True
string_eq ((c:cs), []) = False
string_eq ([], (c:cs)) = False
string_eq ((c1:cs1), (c2:cs2)) = (c1 == c2) && string_eq (cs1, cs2)

mos_match :: (Device, Device) -> Bool
mos_match (inst1, inst2) = 
    (inst_type inst1 == inst_type inst2) &&
    (inst_len inst1 == inst_len inst2) &&
    (inst_wid inst1 == inst_wid inst2) &&
    (inst_multi inst1 == inst_multi inst2)

device_pair_in_list :: (Device, Device) -> [(Device, Device)] -> Bool
device_pair_in_list (inst1, inst2) [] = False
device_pair_in_list (inst1, inst2) ((inst3, inst4):list) = 
    (inst_name inst1 == inst_name inst3) && (inst_name inst2 == inst_name inst4) ||
    (inst_name inst1 == inst_name inst4) && (inst_name inst2 == inst_name inst3) ||
    device_pair_in_list (inst1, inst2) list