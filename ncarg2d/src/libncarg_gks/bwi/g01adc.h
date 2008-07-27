C
C $Id: g01adc.h,v 1.4 2008-07-27 03:55:37 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C

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
