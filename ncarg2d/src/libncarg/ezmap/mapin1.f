C
C $Id: mapin1.f,v 1.1 1999-04-02 22:59:31 kennison Exp $
C
      SUBROUTINE MAPIN1
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
C
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE   /MAPDP1/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
C Some temporary values have to be double precision.
C
      DOUBLE PRECISION COST,SINT,TMP1,TMP2
C
C Set up alternate names for a couple of variables in common.
C
      EQUIVALENCE (PHIA,FLT1),(ROTA,FLT2)
C
C Ensure that the block data routine will load, so that variables will
C have the proper default values.
C
      EXTERNAL MAPBD
C
C Define some necessary constants.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Decide whether MAPTRN should use real or double-precision arithmetic.
C The subroutine call is necessary to fool some compilers into storing
C TST1, TST2, and TST3; otherwise, real precision may be used on
C machines on which double precision is necessary.
C
      CALL MAPIN2 (TST1,TST2,TST3)
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
        RSNO=SIGN(1.,.5*(FLT1+FLT2))
        DSNO=DBLE(RSNO)
        CHI1=(90.-RSNO*FLT1)*DTOR
        IF (FLT1.EQ.FLT2) THEN
          DCSO=COS(DBLE(CHI1))
          RCSO=REAL(DCSO)
        ELSE
          CHI2=(90.-RSNO*FLT2)*DTOR
          DCSO=LOG(SIN(DBLE(CHI1))/SIN(DBLE(CHI2)))/
     +         LOG(TAN(.5D0*DBLE(CHI1))/TAN(.5D0*DBLE(CHI2)))
          RCSO=REAL(DCSO)
        END IF
C
      ELSE
C
C Compute quantities required for all the other projections.
C
        TMP1=DBLE(ROTA*DTOR)
        TMP2=DBLE(PHIA*DTOR)
        DSNR=SIN(TMP1)
        RSNR=REAL(DSNR)
        DCSR=COS(TMP1)
        RCSR=REAL(DCSR)
        DSNO=SIN(TMP2)
        RSNO=REAL(DSNO)
        DCSO=COS(TMP2)
        RCSO=REAL(DCSO)
C
C Compute constants required only by the cylindrical projections.
C
        IF (IPRJ.GE.7) THEN
C
C See if fast-path transformations can be used (PLAT=0, ROTA=0 or 180).
C
          IF (ABS(PHIA).GE..0001.OR.(ABS(ROTA).GE..0001.AND.
     +                               ABS(ROTA).LE.179.9999)) THEN
C
C No.  Compute constants for the ordinary cylindrical projections.
C
            SINT=DCSO*DCSR
            COST=SQRT(1.D0-SINT**2)
            TMP1=DSNR/COST
            TMP2=DSNO/COST
            PHOC=PHIO-REAL(ATAN2(TMP1,-DCSR*TMP2))*RTOD
            DSNR=TMP1*DCSO
            RSNR=REAL(DSNR)
            DCSR=-TMP2
            RCSR=REAL(DCSR)
            DSNO=SINT
            RSNO=REAL(DSNO)
            DCSO=COST
            RCSO=REAL(DCSO)
C
          ELSE
C
C Yes.  The fast paths are implemented as three additional projections.
C
            IPRJ=IPRJ+4
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
