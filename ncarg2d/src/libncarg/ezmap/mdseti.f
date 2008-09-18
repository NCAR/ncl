C
C $Id: mdseti.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDSETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
        INTEGER       IVAL
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
        COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
        INTEGER          IGI1,IGI2,NCRA,NOVS
        REAL             XCRA,YCRA
        SAVE   /MAPCMC/
C
        COMMON /MAPCMQ/  ICIN(8)
        INTEGER          ICIN
        SAVE   /MAPCMQ/
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        IF (ICFELL('MDSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'C1'.OR.WHCH(1:2).EQ.'c1') THEN
          ICIN(1)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C2'.OR.WHCH(1:2).EQ.'c2') THEN
          ICIN(2)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C3'.OR.WHCH(1:2).EQ.'c3') THEN
          ICIN(3)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C4'.OR.WHCH(1:2).EQ.'c4') THEN
          ICIN(4)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C5'.OR.WHCH(1:2).EQ.'c5') THEN
          ICIN(5)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C6'.OR.WHCH(1:2).EQ.'c6') THEN
          ICIN(6)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C7'.OR.WHCH(1:2).EQ.'c7') THEN
          ICIN(7)=IVAL
        ELSE IF (WHCH(1:2).EQ.'C8'.OR.WHCH(1:2).EQ.'c8') THEN
          ICIN(8)=IVAL
        ELSE IF (WHCH(1:2).EQ.'DA'.OR.WHCH(1:2).EQ.'da') THEN
          IDSH=IVAL
        ELSE IF (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          DDTS=DBLE(IVAL)
          DBTD=DDTS/DSCA
        ELSE IF (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
          IDTL=IVAL
        ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
          IDOT=IVAL
        ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
          ELPF=IVAL.NE.0
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          GRLO=DBLE(IVAL)
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          GRPO=MAX(0.D0,MIN(90360.D0,DBLE(IVAL)))
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          GRID=DBLE(IVAL)
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          GRLA=DBLE(IVAL)
        ELSE IF (WHCH(1:2).EQ.'G1'.OR.WHCH(1:2).EQ.'g1') THEN
          IGI1=IVAL
        ELSE IF (WHCH(1:2).EQ.'G2'.OR.WHCH(1:2).EQ.'g2') THEN
          IGI2=IVAL
        ELSE IF (WHCH(1:2).EQ.'II'.OR.WHCH(1:2).EQ.'ii') THEN
          NILN=MAX(1,MIN(999,MOD(IVAL,1000)))
          NILT=MAX(1,MIN(999,IVAL/1000))
        ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
          LBLF=IVAL.NE.0
        ELSE IF (WHCH(1:2).EQ.'LS'.OR.WHCH(1:2).EQ.'ls') THEN
          ILCW=IVAL
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          DPLT=DBLE(IVAL)
          DPSQ=DPLT*DPLT
        ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
          PRMF=IVAL.NE.0
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          PDRE=IVAL
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'RP'.OR.WHCH(1:2).EQ.'rp') THEN
          IDPF=MAX(0,MIN(2,IVAL))
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          SALT=DBLE(IVAL)
          IF (ABS(SALT).GT.1.D0) THEN
            SSMO=SALT*SALT-1.D0
            SRSS=SQRT(SSMO)
          END IF
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          ALFA=ABS(DBLE(IVAL))
          DSNA=SIN(DTOR*ALFA)
          DCSA=COS(DTOR*ALFA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          BETA=DBLE(IVAL)
          DSNB=SIN(DTOR*BETA)
          DCSB=COS(DTOR*BETA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'VS'.OR.WHCH(1:2).EQ.'vs') THEN
          NOVS=IVAL
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
  901   CALL MDPCEM ('MDSETI - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RETURN
C
      END
