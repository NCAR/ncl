C
C $Id: mdpin1.f,v 1.8 2008-09-11 04:11:36 kennison Exp $
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
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,PLNC,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,PLNC,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
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
        EQUIVALENCE      (PLTO,FLT1),(ROTA,FLT2)
C
C Declare local variables.
C
        DOUBLE PRECISION CHI1,CHI2
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
C used.  PLNC is just a copy of PLNO.
C
        IPRJ=JPRJ
        PLNC=PLNO
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
C See if fast-path transformations can by used (type cylindrical or
C mixed, PLAT=0, and ROTA=0 or 180) and, if so, arrange for it.  If
C not, compute required quantities that depend on PLAT and ROTA.
C
          IF (IPRJ.GE.7.AND.IPRJ.LE.15.AND.
     +         ABS(PLTO).LT.   .000001D0.AND.
     +        (ABS(ROTA).LT.   .000001D0.OR.
     +         ABS(ROTA).GT.179.999999D0)    ) THEN
            IPRJ=IPRJ+9
          ELSE
            SINO=SIN(PLTO*DTOR)
            COSO=COS(PLTO*DTOR)
            SINR=SIN(ROTA*DTOR)
            COSR=COS(ROTA*DTOR)
          END IF
C
C Note (09/10/2008): Originally, the arbitrary (non-fast-path) versions
C of the cylindrical and mixed projections were done using a clumsy
C technique involving code that appeared to be primarily intended for
C use in doing azimuthal projections.  This clumsy technique required
C some other variables to be computed at this point: SINT=COSO*COSR,
C COST=SQRT(1.D0-SINT**2), PLNC=PLNO-ATAN2(SINR/COST,-COSR*SINO/COST)*
C RTOD, SINR=SINR*COSO/COST, COSR=-SINO/COST, SINO=SINT, and COSO=COST.
C As it turns out, these transformations can be done more efficiently,
C simply, and logically (and in a manner that doesn't pose the risk of
C dividing by zero), so I have changed the code for them in both the
C forward and inverse transformation routines and removed the unneeded
C code here.
C
C (This comment is for use in the unlikely event that the need arises
C to resurrect the technique.  Search for "C Note (09/10/2008)" to find
C all comments about it.)
C 
        END IF
C
        RETURN
C
      END
