*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTSWAG_EXT_SYS..................................*
DATA:  BEGIN OF STATUS_ZTSWAG_EXT_SYS                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTSWAG_EXT_SYS                .
CONTROLS: TCTRL_ZTSWAG_EXT_SYS
            TYPE TABLEVIEW USING SCREEN '9000'.
*...processing: ZTSWAG_IF.......................................*
DATA:  BEGIN OF STATUS_ZTSWAG_IF                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTSWAG_IF                     .
CONTROLS: TCTRL_ZTSWAG_IF
            TYPE TABLEVIEW USING SCREEN '9010'.
*.........table declarations:.................................*
TABLES: *ZTSWAG_EXT_SYS                .
TABLES: *ZTSWAG_IF                     .
TABLES: ZTSWAG_EXT_SYS                 .
TABLES: ZTSWAG_IF                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
