C
C	$Id: gupdvi.f,v 1.1.1.1 1992-04-17 22:34:02 ncargd Exp $
C
        SUBROUTINE GUPDVI (INVAL, IPOINT, IPRIM)
C
C       UPDATE ATTRIBUTE DEFERRAL VARIABLES FOR A INTEGER-VALUED
C       ATTRIBUTE.
C
C
C       PARAMETERS:
C          INPUT
C               INVAL   - NEW VALUE FOR THE ATTRIBUTE ITEM.
C               IPOINT  - POINTER INTO DEFERRAL CONTROL VARIABLE
C                         STRUCTURES, AND INTO INDEX ARRAY THAT
C                         GIVES START OF PRIMITIVE IN ATTRIBUTE
C                         EQUIVALENCING ARRAY, INTEGER VERSION.
C               IPRIM   - OUTPUT PRIMITIVE TYPE ASSOCIATED WITH
C                         THE ATTRIBUTE (1=POLYLINE, 2=POLYMARKER,
C                         3=TEXT, 4=FILL AREA).  USED TO INDEX INTO
C                         OTHER OF THE CONTROL VARIABLE STRUCTURES.
C
C          OUTPUT
C               ADJUSTMENT OF ATTRIBUTE DEFERRAL CONTROL PARAMETERS
C               IN COMMON.
C
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
      COMMON  /G01ADC/  VALCHG(37)      ,ANYASF ,AGPEND(4)      ,
     +                  IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX ,
     +                  IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP  ,
     +                  IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR ,
     +                  IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ ,
     +                  IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF  ,
     +                  IP2AEA(26)      ,IL2AEA(26)     ,
     +                  IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS ,
     +                  IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI ,
     +                  IAFAIS  ,IAFASI ,IAFACI ,
     +                  NCGASF  ,NGKASF ,MASMAP(18)
        LOGICAL         VALCHG  ,ANYASF ,AGPEND
        INTEGER         IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX
        INTEGER         IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP
        INTEGER         IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR
        INTEGER         IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ
        INTEGER         IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF
        INTEGER         IP2AEA  ,IL2AEA
        INTEGER         IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS
        INTEGER         IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI
        INTEGER         IAFAIS  ,IAFASI ,IAFACI
        INTEGER         NCGASF  ,NGKASF ,MASMAP
C
        LOGICAL         ASFCHG(13)
        EQUIVALENCE     (VALCHG(25),ASFCHG(1))
C
C
C
        INTEGER INVAL(*), IPOINT, IPRIM
C
        LOGICAL CHGNS
        INTEGER I, IOFF, LNGVAL
C
C  OFFSET AND LENGTH OF ATTRIBUTE IN EQUIVALENCING STRUCTURES.
C
        IOFF   = IABS(IP2AEA(IPOINT))
        LNGVAL = IL2AEA(IPOINT)
C
C     IS THE NEW VALUE DIFFERENT FROM THE LAST SENT..
C
        CHGNS = INVAL(1).NE.MSAEQV(IOFF)
        DO 10 I=2,LNGVAL
           CHGNS = CHGNS.OR. INVAL(I).NE.MSAEQV(IOFF+I-1)
10      CONTINUE
C
        IF (CHGNS) THEN
C
C          NEW AND LAST SENT DIFFER.
C
C          SET AGGREGATE CHANGE-PENDING PARAMETER.
           AGPEND(IPRIM) = .TRUE.
        END IF
C
C       COPY NEW VALUE TO REQUESTED.
C
        MRAEQV(IOFF) = INVAL(1)
        DO 30 I=2,LNGVAL
           MRAEQV(IOFF+I-1) = INVAL(I)
30      CONTINUE
C
C       MARK VALUE CHANGE.
C
        VALCHG(IPOINT) = CHGNS
C
C       DONE.
C
        RETURN
C
        END
