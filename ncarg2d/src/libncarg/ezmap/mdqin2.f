C
C $Id: mdqin2.f,v 1.6 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDQIN2 (
     +            ALFAQ,COSOQ,COSRQ,CSLSQ,CSLTQ,DCSAQ,DCSBQ,DSNAQ,DSNBQ,
     +            DTORQ,DTRHQ,OOPIQ,PLNOQ,  PIQ,PIOTQ,ROTAQ,RTDDQ,RTODQ,
     +            SALTQ,SINOQ,SINRQ,SRSSQ,SSMOQ,TOPIQ,UCNMQ,UMNMQ,UMXMQ,
     +            UOFFQ,URNMQ,VCNMQ,VMNMQ,VMXMQ,VOFFQ,VRNMQ,UTPAQ,IPRFQ,
     +            IPRJQ,IRODQ,ELPMQ)
C
      DOUBLE PRECISION
     +            ALFAQ,COSOQ,COSRQ,CSLSQ,CSLTQ,DCSAQ,DCSBQ,DSNAQ,DSNBQ,
     +            DTORQ,DTRHQ,OOPIQ,PLNOQ,  PIQ,PIOTQ,ROTAQ,RTDDQ,RTODQ,
     +            SALTQ,SINOQ,SINRQ,SRSSQ,SSMOQ,TOPIQ,UCNMQ,UMNMQ,UMXMQ,
     +            UOFFQ,URNMQ,VCNMQ,VMNMQ,VMXMQ,VOFFQ,VRNMQ,UTPAQ(15)
C
      INTEGER IPRFQ,IPRJQ,IRODQ
C
      LOGICAL ELPMQ
C
C Declare a special common block, containing only those variables that
C MDQINI needs to set to make MDQTRA, MDQTRI, and MDQTRN carry out the
C transformation in effect at the time MDQINI was called.
C
        COMMON /MAQCMN/  ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA,IPRF,
     +                   IPRJ,IROD,ELPM
        DOUBLE PRECISION ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA(15)
C
        INTEGER IPRF,IPRJ,IROD
C
        LOGICAL ELPM
C
        SAVE   /MAQCMN/
C
        INTEGER I
C
C Transfer the required variables to the local common block.
C
        ALFA=ALFAQ
        COSO=COSOQ
        COSR=COSRQ
        CSLS=CSLSQ
        CSLT=CSLTQ
        DCSA=DCSAQ
        DCSB=DCSBQ
        DSNA=DSNAQ
        DSNB=DSNBQ
        DTOR=DTORQ
        DTRH=DTRHQ
        ELPM=ELPMQ
        IPRJ=IPRJQ
        IROD=IRODQ
        OOPI=OOPIQ
        PLNO=PLNOQ
          PI=  PIQ
        PIOT=PIOTQ
        ROTA=ROTAQ
        RTDD=RTDDQ
        RTOD=RTODQ
        SALT=SALTQ
        SINO=SINOQ
        SINR=SINRQ
        SRSS=SRSSQ
        SSMO=SSMOQ
        TOPI=TOPIQ
        UCNM=UCNMQ
        UMNM=UMNMQ
        UMXM=UMXMQ
        UOFF=UOFFQ
        URNM=URNMQ
        VCNM=VCNMQ
        VMNM=VMNMQ
        VMXM=VMXMQ
        VOFF=VOFFQ
        VRNM=VRNMQ 
C
        IPRF=IPRFQ
C
        DO 101 I=1,15
          UTPA(I)=UTPAQ(I)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
