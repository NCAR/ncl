      COMMON  /GWIAST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,ASMSZS ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,ASCHXP ,
     +                  ASCHSP  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,
     +                  MSASF(13)
        INTEGER         MSPLIX  ,MSLTYP ,MSPLCI
        REAL            ASLWSC
        INTEGER         MSPMIX  ,MSMTYP ,MSPMCI
        REAL            ASMSZS
        INTEGER         MSTXIX  ,MSTXP  ,MSTXAL ,MSTXFO
        INTEGER         MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV
        REAL            ASCHXP  ,ASCHSP
        INTEGER         MSFAIX  ,MSPASZ ,MSPARF ,MSFAIS ,MSFASI
        INTEGER         MSFACI  ,MSASF
        INTEGER         MSAEQV(45)
        REAL            ASAEQV(45)
        EQUIVALENCE     (MSPLIX, MSAEQV, ASAEQV)
