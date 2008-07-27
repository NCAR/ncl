C
C $Id: mdpin1.f,v 1.6 2008-07-27 00:17:03 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIN1
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Set up alternate names for a couple of variables in common.
C
        DOUBLE PRECISION FLT1,FLT2
        EQUIVALENCE      (PHIA,FLT1),(ROTA,FLT2)
C
C Declare local variables.
C
        DOUBLE PRECISION CHI1,CHI2,COST,SINT,TMP1,TMP2
        REAL             TST1,TST2,TST3
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL MAPBD
C
C Decide whether MAPTRN should use real or double-precision arithmetic.
C The subroutine call is necessary to fool some compilers into storing
C TST1, TST2, and TST3; otherwise, real precision may be used on
C machines on which double precision is necessary.  NOTE: As of 07/2001,
C double precision is being used everywhere except in the USGS routines,
C so the flag IROD is not as important as it was prior to that time.
C
        CALL MDPIN2 (TST1,TST2,TST3)
C
        IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
          IROD=0
        ELSE
          IROD=1
        END IF
C
C IPRJ equals JPRJ until we find out if fast-path projections are to be
C used.  PHOC is just a copy of PHIO.
C
        IPRJ=JPRJ
        PHOC=PHIO
C
        IF (IPRJ.EQ.1) THEN
C
C Compute quantities required for the Lambert conformal conic.
C
          SINO=SIGN(1.D0,.5D0*(FLT1+FLT2))
          CHI1=(90.D0-SINO*FLT1)*DTOR
C
          IF (FLT1.EQ.FLT2) THEN
            COSO=COS(CHI1)
          ELSE
            CHI2=(90.D0-SINO*FLT2)*DTOR
            COSO=LOG(SIN(CHI1)/SIN(CHI2))/
     +           LOG(TAN(.5D0*CHI1)/TAN(.5D0*CHI2))
          END IF
C
        ELSE
C
C Compute quantities required for all the other projections.
C
          TMP1=ROTA*DTOR
          TMP2=PHIA*DTOR
          SINR=SIN(TMP1)
          COSR=COS(TMP1)
          SINO=SIN(TMP2)
          COSO=COS(TMP2)
C
C Compute constants required only by the cylindrical projections.
C
          IF (IPRJ.GE.7.AND.IPRJ.LE.11) THEN
C
C See if fast-path transformations can be used (PLAT=0, ROTA=0 or 180).
C
            IF (ABS(PHIA).GE..000001D0.OR.(ABS(ROTA).GE..000001D0.AND.
     +                                  ABS(ROTA).LE.179.999999D0)) THEN
C
C No.  Compute constants for the ordinary cylindrical projections.
C
              SINT=COSO*COSR
              COST=SQRT(1.D0-SINT**2)
              TMP1=SINR/COST
              TMP2=SINO/COST
              PHOC=PHIO-ATAN2(TMP1,-COSR*TMP2)*RTOD
              SINR=TMP1*COSO
              COSR=-TMP2
              SINO=SINT
              COSO=COST
C
            ELSE
C
C Yes.  The fast paths are implemented as five additional projections.
C
              IPRJ=IPRJ+5
C
            END IF
C
          END IF
C
        END IF
C
        RETURN
C
      END
