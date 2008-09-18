C
C $Id: mdsetr.f,v 1.11 2008-09-18 12:19:11 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDSETR (WHCH,RVAL)
C
        CHARACTER*(*) WHCH
        REAL          RVAL
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
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
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
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE   /MAPCMW/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        IF (ICFELL('MDSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          DDTS=DBLE(RVAL)
          DBTD=DDTS/DSCA
        ELSE IF (WHCH(1:2).EQ.'GD'.OR.WHCH(1:2).EQ.'gd') THEN
          GRDR=MAX(.001D0,MIN(10.D0,DBLE(RVAL)))
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          GRLO=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          GRPO=MAX(0.D0,MIN(90360.D0,DBLE(RVAL)))
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          GRID=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          GRLA=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          DPLT=DBLE(RVAL)
          DPSQ=DPLT*DPLT
        ELSE IF (WHCH(1:2).EQ.'OT'.OR.WHCH(1:2).EQ.'ot') THEN
          OTOL=MAX(0.D0,DBLE(RVAL))
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          PDRE=DBLE(RVAL)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          SALT=DBLE(RVAL)
          IF (ABS(SALT).GT.1.D0) THEN
            SSMO=SALT*SALT-1.D0
            SRSS=SQRT(SSMO)
          END IF
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SL'.OR.WHCH(1:2).EQ.'sl') THEN
          SLTD=MAX(-1.D0,MIN(91.D0,DBLE(RVAL)))
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          ALFA=ABS(DBLE(RVAL))
          DSNA=SIN(DTOR*ALFA)
          DCSA=COS(DTOR*ALFA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          BETA=DBLE(RVAL)
          DSNB=SIN(DTOR*BETA)
          DCSB=COS(DTOR*BETA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SR'.OR.WHCH(1:2).EQ.'sr') THEN
          SRCH=MAX(.001D0,MIN(10.D0,DBLE(RVAL)))
          INTF=.TRUE.
        ELSE
          GO TO 901
        END IF
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL MDPCEM ('MDSETR - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RETURN
C
      END
