C
C $Id: mapgti.f,v 1.7 1994-04-08 23:00:25 kennison Exp $
C
      SUBROUTINE MAPGTI (WHCH,IVAL)
C
      CHARACTER*(*) WHCH
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
      COMMON /MAPCMQ/ ICIN(7)
      SAVE /MAPCMQ/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
      IF (ICFELL('MAPGTI - UNCLEARED PRIOR ERROR',1).NE.0) THEN
        IIER=-1
        RETURN
      END IF
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
      ELSE IF (WHCH(1:2).EQ.'DA'.OR.WHCH(1:2).EQ.'da') THEN
        IVAL=IDSH
      ELSE IF (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
        IVAL=DDTS
      ELSE IF (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
        IVAL=IDTL
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        IVAL=IDOT
      ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
        IVAL=0
        IF (ELPF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'ER'.OR.WHCH(1:2).EQ.'er') THEN
        IVAL=IIER
      ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
        IVAL=GRID
      ELSE IF (WHCH(1:2).EQ.'G1'.OR.WHCH(1:2).EQ.'g1') THEN
        IVAL=IGI1
      ELSE IF (WHCH(1:2).EQ.'G2'.OR.WHCH(1:2).EQ.'g2') THEN
        IVAL=IGI2
      ELSE IF (WHCH(1:2).EQ.'IN'.OR.WHCH(1:2).EQ.'in') THEN
        IVAL=0
        IF (INTF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
        IVAL=0
        IF (LBLF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'LS'.OR.WHCH(1:2).EQ.'ls') THEN
        IVAL=ILCW
      ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
        IVAL=DPLT
      ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
        IVAL=0
        IF (PRMF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'PN'.OR.WHCH(1:2).EQ.'pn') THEN
        IVAL=PHIO
      ELSE IF (WHCH(1:2).EQ.'PT'.OR.WHCH(1:2).EQ.'pt') THEN
        IVAL=PHIA
      ELSE IF (WHCH(1:2).EQ.'P1'.OR.WHCH(1:2).EQ.'p1') THEN
        IVAL=PLA1
      ELSE IF (WHCH(1:2).EQ.'P2'.OR.WHCH(1:2).EQ.'p2') THEN
        IVAL=PLA2
      ELSE IF (WHCH(1:2).EQ.'P3'.OR.WHCH(1:2).EQ.'p3') THEN
        IVAL=PLA3
      ELSE IF (WHCH(1:2).EQ.'P4'.OR.WHCH(1:2).EQ.'p4') THEN
        IVAL=PLA4
      ELSE IF (WHCH(1:2).EQ.'P5'.OR.WHCH(1:2).EQ.'p5') THEN
        IVAL=PLB1
      ELSE IF (WHCH(1:2).EQ.'P6'.OR.WHCH(1:2).EQ.'p6') THEN
        IVAL=PLB2
      ELSE IF (WHCH(1:2).EQ.'P7'.OR.WHCH(1:2).EQ.'p7') THEN
        IVAL=PLB3
      ELSE IF (WHCH(1:2).EQ.'P8'.OR.WHCH(1:2).EQ.'p8') THEN
        IVAL=PLB4
      ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
        IVAL=PLTR
      ELSE IF (WHCH(1:2).EQ.'RO'.OR.WHCH(1:2).EQ.'ro') THEN
        IVAL=ROTA
      ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
        IVAL=SALT
      ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
        IVAL=ALFA
      ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
        IVAL=BETA
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
  901 IIER=2
      CALL MAPCEM ('MAPGTI - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      IVAL=0
      RETURN
C
      END
