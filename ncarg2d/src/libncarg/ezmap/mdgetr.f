C
C $Id: mdgetr.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDGETR (WHCH,RVAL)
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
        IF (ICFELL('MDGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          RVAL=REAL(DDTS)
        ELSE IF (WHCH(1:2).EQ.'GD'.OR.WHCH(1:2).EQ.'gd') THEN
          RVAL=REAL(GRDR)
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          RVAL=REAL(GRLO)
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          RVAL=REAL(GRPO)
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          RVAL=REAL(GRID)
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          RVAL=REAL(GRLA)
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          RVAL=REAL(DPLT)
        ELSE IF (WHCH(1:2).EQ.'OT'.OR.WHCH(1:2).EQ.'ot') THEN
          RVAL=REAL(OTOL)
        ELSE IF (WHCH(1:2).EQ.'PN'.OR.WHCH(1:2).EQ.'pn') THEN
          RVAL=REAL(PLNO)
        ELSE IF (WHCH(1:2).EQ.'PT'.OR.WHCH(1:2).EQ.'pt') THEN
          RVAL=REAL(PLTO)
        ELSE IF (WHCH(1:2).EQ.'P1'.OR.WHCH(1:2).EQ.'p1') THEN
          RVAL=REAL(PLA1)
        ELSE IF (WHCH(1:2).EQ.'P2'.OR.WHCH(1:2).EQ.'p2') THEN
          RVAL=REAL(PLA2)
        ELSE IF (WHCH(1:2).EQ.'P3'.OR.WHCH(1:2).EQ.'p3') THEN
          RVAL=REAL(PLA3)
        ELSE IF (WHCH(1:2).EQ.'P4'.OR.WHCH(1:2).EQ.'p4') THEN
          RVAL=REAL(PLA4)
        ELSE IF (WHCH(1:2).EQ.'P5'.OR.WHCH(1:2).EQ.'p5') THEN
          RVAL=REAL(PLB1)
        ELSE IF (WHCH(1:2).EQ.'P6'.OR.WHCH(1:2).EQ.'p6') THEN
          RVAL=REAL(PLB2)
        ELSE IF (WHCH(1:2).EQ.'P7'.OR.WHCH(1:2).EQ.'p7') THEN
          RVAL=REAL(PLB3)
        ELSE IF (WHCH(1:2).EQ.'P8'.OR.WHCH(1:2).EQ.'p8') THEN
          RVAL=REAL(PLB4)
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          RVAL=REAL(PDRE)
        ELSE IF (WHCH(1:2).EQ.'RO'.OR.WHCH(1:2).EQ.'ro') THEN
          RVAL=REAL(ROTA)
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          RVAL=REAL(SALT)
        ELSE IF (WHCH(1:2).EQ.'SL'.OR.WHCH(1:2).EQ.'sl') THEN
          RVAL=REAL(SLTD)
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          RVAL=REAL(ALFA)
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          RVAL=REAL(BETA)
        ELSE IF (WHCH(1:2).EQ.'SR'.OR.WHCH(1:2).EQ.'sr') THEN
          RVAL=REAL(SRCH)
        ELSE IF (WHCH(1:2).EQ.'XL'.OR.WHCH(1:2).EQ.'xl') THEN
          RVAL=REAL(XLOW)
        ELSE IF (WHCH(1:2).EQ.'XR'.OR.WHCH(1:2).EQ.'xr') THEN
          RVAL=REAL(XROW)
        ELSE IF (WHCH(1:2).EQ.'YB'.OR.WHCH(1:2).EQ.'yb') THEN
          RVAL=REAL(YBOW)
        ELSE IF (WHCH(1:2).EQ.'YT'.OR.WHCH(1:2).EQ.'yt') THEN
          RVAL=REAL(YTOW)
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
  901   CALL MDPCEM ('MDGETR - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RVAL=0.
        RETURN
C
      END
