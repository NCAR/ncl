C
C	$Id: mapgtr.f,v 1.1.1.1 1992-04-17 22:32:00 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPGTR (WHCH,RVAL)
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
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
      IF (WHCH(1:2).EQ.'DD') THEN
        RVAL=DDTS
      ELSE IF (WHCH(1:2).EQ.'GD') THEN
        RVAL=GRDR
      ELSE IF (WHCH(1:2).EQ.'GR') THEN
        RVAL=GRID
      ELSE IF (WHCH(1:2).EQ.'MV') THEN
        RVAL=DPLT
      ELSE IF (WHCH(1:2).EQ.'PN') THEN
        RVAL=PHIO
      ELSE IF (WHCH(1:2).EQ.'PT') THEN
        RVAL=PHIA
      ELSE IF (WHCH(1:2).EQ.'P1') THEN
        RVAL=PLA1
      ELSE IF (WHCH(1:2).EQ.'P2') THEN
        RVAL=PLA2
      ELSE IF (WHCH(1:2).EQ.'P3') THEN
        RVAL=PLA3
      ELSE IF (WHCH(1:2).EQ.'P4') THEN
        RVAL=PLA4
      ELSE IF (WHCH(1:2).EQ.'P5') THEN
        RVAL=PLB1
      ELSE IF (WHCH(1:2).EQ.'P6') THEN
        RVAL=PLB2
      ELSE IF (WHCH(1:2).EQ.'P7') THEN
        RVAL=PLB3
      ELSE IF (WHCH(1:2).EQ.'P8') THEN
        RVAL=PLB4
      ELSE IF (WHCH(1:2).EQ.'RE') THEN
        RVAL=PLTR
      ELSE IF (WHCH(1:2).EQ.'RO') THEN
        RVAL=ROTA
      ELSE IF (WHCH(1:2).EQ.'SA') THEN
        RVAL=SALT
      ELSE IF (WHCH(1:2).EQ.'S1') THEN
        RVAL=ALFA
      ELSE IF (WHCH(1:2).EQ.'S2') THEN
        RVAL=BETA
      ELSE IF (WHCH(1:2).EQ.'SR') THEN
        RVAL=SRCH
      ELSE IF (WHCH(1:2).EQ.'XL') THEN
        RVAL=XLOW
      ELSE IF (WHCH(1:2).EQ.'XR') THEN
        RVAL=XROW
      ELSE IF (WHCH(1:2).EQ.'YB') THEN
        RVAL=YBOW
      ELSE IF (WHCH(1:2).EQ.'YT') THEN
        RVAL=YTOW
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
  901 IIER=4
      CALL MAPCEM (' MAPGTR - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      RVAL=0.
      RETURN
C
      END
