C
C $Id: mdgeti.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDGETI (WHCH,IVAL)
C
        CHARACTER*(*) WHCH
        INTEGER       IVAL
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
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
        IF (ICFELL('MDGETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'C1'.OR.WHCH(1:2).EQ.'c1') THEN
          IVAL=ICIN(1)
        ELSE IF (WHCH(1:2).EQ.'C2'.OR.WHCH(1:2).EQ.'c2') THEN
          IVAL=ICIN(2)
        ELSE IF (WHCH(1:2).EQ.'C3'.OR.WHCH(1:2).EQ.'c3') THEN
          IVAL=ICIN(3)
        ELSE IF (WHCH(1:2).EQ.'C4'.OR.WHCH(1:2).EQ.'c4') THEN
          IVAL=ICIN(4)
        ELSE IF (WHCH(1:2).EQ.'C5'.OR.WHCH(1:2).EQ.'c5') THEN
          IVAL=ICIN(5)
        ELSE IF (WHCH(1:2).EQ.'C6'.OR.WHCH(1:2).EQ.'c6') THEN
          IVAL=ICIN(6)
        ELSE IF (WHCH(1:2).EQ.'C7'.OR.WHCH(1:2).EQ.'c7') THEN
          IVAL=ICIN(7)
        ELSE IF (WHCH(1:2).EQ.'C8'.OR.WHCH(1:2).EQ.'c8') THEN
          IVAL=ICIN(8)
        ELSE IF (WHCH(1:2).EQ.'DA'.OR.WHCH(1:2).EQ.'da') THEN
          IVAL=IDSH
        ELSE IF (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          IVAL=INT(DDTS)
        ELSE IF (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
          IVAL=IDTL
        ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
          IVAL=IDOT
        ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
          IVAL=0
          IF (ELPF) IVAL=1
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          IVAL=INT(GRLO)
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          IVAL=INT(GRPO)
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          IVAL=INT(GRID)
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          IVAL=INT(GRLA)
        ELSE IF (WHCH(1:2).EQ.'G1'.OR.WHCH(1:2).EQ.'g1') THEN
          IVAL=IGI1
        ELSE IF (WHCH(1:2).EQ.'G2'.OR.WHCH(1:2).EQ.'g2') THEN
          IVAL=IGI2
        ELSE IF (WHCH(1:2).EQ.'IN'.OR.WHCH(1:2).EQ.'in') THEN
          IVAL=0
          IF (INTF) IVAL=1
        ELSE IF (WHCH(1:2).EQ.'II'.OR.WHCH(1:2).EQ.'ii') THEN
          IVAL=1000*NILT+NILN
        ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
          IVAL=0
          IF (LBLF) IVAL=1
        ELSE IF (WHCH(1:2).EQ.'LS'.OR.WHCH(1:2).EQ.'ls') THEN
          IVAL=ILCW
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          IVAL=INT(DPLT)
        ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
          IVAL=0
          IF (PRMF) IVAL=1
        ELSE IF (WHCH(1:2).EQ.'PN'.OR.WHCH(1:2).EQ.'pn') THEN
          IVAL=INT(PLNO)
        ELSE IF (WHCH(1:2).EQ.'PT'.OR.WHCH(1:2).EQ.'pt') THEN
          IVAL=INT(PLTO)
        ELSE IF (WHCH(1:2).EQ.'P1'.OR.WHCH(1:2).EQ.'p1') THEN
          IVAL=INT(PLA1)
        ELSE IF (WHCH(1:2).EQ.'P2'.OR.WHCH(1:2).EQ.'p2') THEN
          IVAL=INT(PLA2)
        ELSE IF (WHCH(1:2).EQ.'P3'.OR.WHCH(1:2).EQ.'p3') THEN
          IVAL=INT(PLA3)
        ELSE IF (WHCH(1:2).EQ.'P4'.OR.WHCH(1:2).EQ.'p4') THEN
          IVAL=INT(PLA4)
        ELSE IF (WHCH(1:2).EQ.'P5'.OR.WHCH(1:2).EQ.'p5') THEN
          IVAL=INT(PLB1)
        ELSE IF (WHCH(1:2).EQ.'P6'.OR.WHCH(1:2).EQ.'p6') THEN
          IVAL=INT(PLB2)
        ELSE IF (WHCH(1:2).EQ.'P7'.OR.WHCH(1:2).EQ.'p7') THEN
          IVAL=INT(PLB3)
        ELSE IF (WHCH(1:2).EQ.'P8'.OR.WHCH(1:2).EQ.'p8') THEN
          IVAL=INT(PLB4)
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          IVAL=INT(PDRE)
        ELSE IF (WHCH(1:2).EQ.'RO'.OR.WHCH(1:2).EQ.'ro') THEN
          IVAL=INT(ROTA)
        ELSE IF (WHCH(1:2).EQ.'RP'.OR.WHCH(1:2).EQ.'rp') THEN
          IVAL=IDPF
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          IVAL=INT(SALT)
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          IVAL=INT(ALFA)
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          IVAL=INT(BETA)
        ELSE IF (WHCH(1:2).EQ.'VS'.OR.WHCH(1:2).EQ.'vs') THEN
          IVAL=NOVS
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
  901   CALL MDPCEM ('MDGETI - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        IVAL=0
        RETURN
C
      END
