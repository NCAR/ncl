C
C	$Id: g01d2r.f,v 1.1.1.1 1992-04-17 22:33:57 ncargd Exp $
C
        SUBROUTINE G01D2R
C
C       COPY "DEFAULT" ATTRIBUTE CONTEXT TO "REQUESTED" CONTEXT.
C
C         (THERE IS NOW SUFFICIENT INFORMATION IN /G01ADC/
C          TO PUT A DO LOOP STRUCTURE IN TO EFFECT THE COPY,
C          USING THE EQUIVALENCING ARRAYS AND THE SIGN OF
C          IP2AEA).
C
      COMMON  /G01ARQ/  MRPLIX  ,MRLTYP ,ARLWSC ,MRPLCI ,
     +                  MRPMIX  ,MRMTYP ,ARMSZS ,MRPMCI ,
     +                  MRTXIX  ,MRTXP  ,MRTXAL(2)      ,MRCHH  ,
     +                  MRCHOV(4)       ,MRTXFO ,MRTXPR ,ARCHXP ,
     +                  ARCHSP  ,MRTXCI ,
     +                  MRFAIX  ,MRPASZ(4)      ,MRPARF(2)      ,
     +                  MRFAIS  ,MRFASI ,MRFACI ,
     +                  MRASF(13)
        INTEGER         MRPLIX  ,MRLTYP ,MRPLCI
        REAL            ARLWSC
        INTEGER         MRPMIX  ,MRMTYP ,MRPMCI
        REAL            ARMSZS
        INTEGER         MRTXIX  ,MRTXP  ,MRTXAL ,MRTXFO
        INTEGER         MRTXPR  ,MRTXCI ,MRCHH  ,MRCHOV
        REAL            ARCHXP  ,ARCHSP
        INTEGER         MRFAIX  ,MRPASZ ,MRPARF ,MRFAIS ,MRFASI
        INTEGER         MRFACI  ,MRASF
        INTEGER         MRAEQV(45)
        REAL            ARAEQV(45)
        EQUIVALENCE     (MRPLIX, MRAEQV, ARAEQV)
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
C       POLYLINE ATTRIBUTES.
        MRPLIX = MDPLIX
        MRLTYP = MDLTYP
        ARLWSC = ADLWSC
        MRPLCI = MDPLCI
C
C       POLYMARKER ATTRIBUTES.
        MRPMIX = MDPMIX
        MRMTYP = MDMTYP
        ARMSZS = ADMSZS
        MRPMCI = MDPMCI
C
C       TEXT ATTRIBUTES.
        MRTXIX    = MDTXIX
        MRTXP     = MDTXP
        MRTXAL(1) = MDTXAL(1)
        MRTXAL(2) = MDTXAL(2)
        MRCHH     = MDCHH
        DO 10 I=1,4
           MRCHOV(I) = MDCHOV(I)
10      CONTINUE
        MRTXFO = MDTXFO
        MRTXPR = MDTXPR
        ARCHXP = ADCHXP
        ARCHSP = ADCHSP
        MRTXCI = MDTXCI
C
C       FILL AREA ATTRIBUTES.
        MRFAIX    = MDFAIX
        MRPASZ(1) = MDPASZ(1)
        MRPASZ(2) = MDPASZ(2)
        MRPARF(1) = MDPARF(1)
        MRPARF(2) = MDPARF(2)
        MRFAIS    = MDFAIS
        MRFASI    = MDFASI
        MRFACI    = MDFACI
C
C       ASPECT SOURCE FLAGS.
        DO 20 I=1,13
          MRASF(I) = MDASF(I)
20      CONTINUE
C
C
C
        RETURN
C
        END
