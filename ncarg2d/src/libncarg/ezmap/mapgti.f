C
C	$Id: mapgti.f,v 1.1.1.1 1992-04-17 22:32:00 ncargd Exp $
C
C
C-----------------------------------------------------------------------
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
      IF (WHCH(1:2).EQ.'C1') THEN
        IVAL=ICIN(1)
      ELSE IF (WHCH(1:2).EQ.'C2') THEN
        IVAL=ICIN(2)
      ELSE IF (WHCH(1:2).EQ.'C3') THEN
        IVAL=ICIN(3)
      ELSE IF (WHCH(1:2).EQ.'C4') THEN
        IVAL=ICIN(4)
      ELSE IF (WHCH(1:2).EQ.'C5') THEN
        IVAL=ICIN(5)
      ELSE IF (WHCH(1:2).EQ.'C6') THEN
        IVAL=ICIN(6)
      ELSE IF (WHCH(1:2).EQ.'C7') THEN
        IVAL=ICIN(7)
      ELSE IF (WHCH(1:2).EQ.'DA') THEN
        IVAL=IDSH
      ELSE IF (WHCH(1:2).EQ.'DD') THEN
        IVAL=DDTS
      ELSE IF (WHCH(1:2).EQ.'DL') THEN
        IVAL=IDTL
      ELSE IF (WHCH(1:2).EQ.'DO') THEN
        IVAL=IDOT
      ELSE IF (WHCH(1:2).EQ.'EL') THEN
        IVAL=0
        IF (ELPF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'ER') THEN
        IVAL=IIER
      ELSE IF (WHCH(1:2).EQ.'GR') THEN
        IVAL=GRID
      ELSE IF (WHCH(1:2).EQ.'G1') THEN
        IVAL=IGI1
      ELSE IF (WHCH(1:2).EQ.'G2') THEN
        IVAL=IGI2
      ELSE IF (WHCH(1:2).EQ.'IN') THEN
        IVAL=0
        IF (INTF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'LA') THEN
        IVAL=0
        IF (LBLF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'LS') THEN
        IVAL=ILCW
      ELSE IF (WHCH(1:2).EQ.'MV') THEN
        IVAL=DPLT
      ELSE IF (WHCH(1:2).EQ.'PE') THEN
        IVAL=0
        IF (PRMF) IVAL=1
      ELSE IF (WHCH(1:2).EQ.'PN') THEN
        IVAL=PHIO
      ELSE IF (WHCH(1:2).EQ.'PT') THEN
        IVAL=PHIA
      ELSE IF (WHCH(1:2).EQ.'P1') THEN
        IVAL=PLA1
      ELSE IF (WHCH(1:2).EQ.'P2') THEN
        IVAL=PLA2
      ELSE IF (WHCH(1:2).EQ.'P3') THEN
        IVAL=PLA3
      ELSE IF (WHCH(1:2).EQ.'P4') THEN
        IVAL=PLA4
      ELSE IF (WHCH(1:2).EQ.'P5') THEN
        IVAL=PLB1
      ELSE IF (WHCH(1:2).EQ.'P6') THEN
        IVAL=PLB2
      ELSE IF (WHCH(1:2).EQ.'P7') THEN
        IVAL=PLB3
      ELSE IF (WHCH(1:2).EQ.'P8') THEN
        IVAL=PLB4
      ELSE IF (WHCH(1:2).EQ.'RE') THEN
        IVAL=PLTR
      ELSE IF (WHCH(1:2).EQ.'RO') THEN
        IVAL=ROTA
      ELSE IF (WHCH(1:2).EQ.'SA') THEN
        IVAL=SALT
      ELSE IF (WHCH(1:2).EQ.'S1') THEN
        IVAL=ALFA
      ELSE IF (WHCH(1:2).EQ.'S2') THEN
        IVAL=BETA
      ELSE IF (WHCH(1:2).EQ.'VS') THEN
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
      CALL MAPCEM (' MAPGTI - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      IVAL=0
      RETURN
C
      END
