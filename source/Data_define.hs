---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
--Project: HMirr
--Authors: Wang Chuyu, He Chenlong
--Date: 2022.12
--
--File: Data_define.hs
--Note: define data struct
--Designers: He Chenlong
--Writers: He Chenlong
---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

module Data_define where


data InstType = RES | PMOS | NMOS | CAPTOR | OTHER deriving (Eq , Show)

data PinType = SOURCE | DRAIN | GATE | BULK deriving (Eq , Show)

data MosType = DIFF | DIODE | CAP | DUMMY deriving (Eq , Show)

data Device = Device { inst_name  :: String
                       , inst_type  :: InstType
                       , source_pin :: ( PinType , String )
                       , drain_pin  :: ( PinType , String )
                       , gate_pin   :: ( PinType , String )
                       , bulk_pin   :: ( PinType , String )
                       , inst_len   :: String
                       , inst_wid   :: String
                       , inst_multi :: String
                       }

data MosGroup = MosGroup { diode_name :: String 
                         , mir_name   :: String
                         } deriving (Eq , Show )