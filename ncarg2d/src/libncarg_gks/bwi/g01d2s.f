C
C	$Id: g01d2s.f,v 1.1.1.1 1992-04-17 22:33:57 ncargd Exp $
C
        SUBROUTINE G01D2S
C
C       COPY "DEFAULT" ATTRIBUTE CONTEXT TO "SET" CONTEXT.
C
C         (THERE IS NOW SUFFICIENT INFORMATION IN /G01ADC/
C          TO PUT A DO LOOP STRUCTURE IN TO EFFECT THE COPY,
C          USING THE EQUIVALENCING ARRAYS AND THE SIGN OF
C          IP2AEA).
C
      COMMON  /G01AST/  MSPLIX  ,MSLTYP ,ASLWSC ,MSPLCI ,
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
      COMMON  /G01ADF/  MDPLIX  ,MDLTYP ,ADLWSC ,MDPLCI ,
     +                  MDPMIX  ,MDMTYP ,ADMSZS ,MDPMCI ,
     +                  MDTXIX  ,MDTXP  ,MDTXAL(2)      ,MDCHH  ,
     +                  MDCHOV(4)       ,MDTXFO ,MDTXPR ,ADCHXP ,
     +                  ADCHSP  ,MDTXCI ,
     +                  MDFAIX  ,MDPASZ(4)       ,MDPARF(2)     ,
     +                  MDFAIS  ,MDFASI  ,MDFACI  ,
     +                  MDASF(13)
        INTEGER         MDPLIX  ,MDLTYP ,MDPLCI
        REAL            ADLWSC
        INTEGER         MDPMIX  ,MDMTYP ,MDPMCI
        REAL            ADMSZS
        INTEGER         MDTXIX  ,MDTXP  ,MDTXAL ,MDTXFO
        INTEGER         MDTXPR  ,MDTXCI ,MDCHH  ,MDCHOV
        REAL            ADCHXP  ,ADCHSP
        INTEGER         MDFAIX  ,MDPASZ ,MDPARF ,MDFAIS ,MDFASI
        INTEGER         MDFACI  ,MDASF
C
C
        INTEGER  I
C
C
C       POLYLINE ATTRIBUTES.
        MSPLIX = MDPLIX
        MSLTYP = MDLTYP
        ASLWSC = ADLWSC
        MSPLCI = MDPLCI
C
C       POLYMARKER ATTRIBUTES.
        MSPMIX = MDPMIX
        MSMTYP = MDMTYP
        ASMSZS = ADMSZS
        MSPMCI = MDPMCI
C
C       TEXT ATTRIBUTES.
        MSTXIX    = MDTXIX
        MSTXP     = MDTXP
        MSTXAL(1) = MDTXAL(1)
        MSTXAL(2) = MDTXAL(2)
        MSCHH     = MDCHH
        DO 10 I=1,4
           MSCHOV(I) = MDCHOV(I)
10      CONTINUE
        MSTXFO = MDTXFO
        MSTXPR = MDTXPR
        ASCHXP = ADCHXP
        ASCHSP = ADCHSP
        MSTXCI = MDTXCI
C
C       FILL AREA ATTRIBUTES.
        MSFAIX    = MDFAIX
        MSPASZ(1) = MDPASZ(1)
        MSPASZ(2) = MDPASZ(2)
        MSPARF(1) = MDPARF(1)
        MSPARF(2) = MDPARF(2)
        MSFAIS    = MDFAIS
        MSFASI    = MDFASI
        MSFACI    = MDFACI
C
C       ASPECT SOURCE FLAGS.
        DO 20 I=1,13
          MSASF(I) = MDASF(I)
20      CONTINUE
C
C
C
        RETURN
C
        END
