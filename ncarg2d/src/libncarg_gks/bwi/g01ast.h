      COMMON  /G01AST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
     +                  MSPMIX  ,MSMTYP ,ASMSZS ,MSPMCI ,
     +                  MSTXIX  ,MSTXP  ,MSTXAL(2)      ,MSCHH  ,
     +                  MSCHOV(4)       ,MSTXFO ,MSTXPR ,ASCHXP ,
     +                  ASCHSP  ,MSTXCI ,
     +                  MSFAIX  ,MSPASZ(4)      ,MSPARF(2)      ,
     +                  MSFAIS  ,MSFASI ,MSFACI ,
     +                  MSASF(13)
        REAL            ASLWSC  ,ASMSZS ,ASCHXP ,ASCHSP ,ASAEQV(45)
        INTEGER         MSPLIX  ,MSLTYP ,MSPLCI ,MSPMIX ,MSMTYP ,
     +                  MSPMCI  ,MSTXIX ,MSTXP  ,MSTXAL ,MSTXFO ,
     +                  MSTXPR  ,MSTXCI ,MSCHH  ,MSCHOV ,MSFAIX ,
     +                  MSPASZ  ,MSPARF ,MSFAIS ,MSFASI ,MSFACI ,
     +                  MSASF   ,MSAEQV(45)
        EQUIVALENCE     (MSPLIX, MSAEQV, ASAEQV)
