C
C	$Id: mapstl.f,v 1.1.1.1 1992-04-17 22:32:06 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPSTL (WHCH,LVAL)
C
      CHARACTER*(*) WHCH
      LOGICAL LVAL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
      IF (WHCH(1:2).EQ.'DL') THEN
        IDTL=0
        IF (LVAL) IDTL=1
      ELSE IF (WHCH(1:2).EQ.'DO') THEN
        IDOT=0
        IF (LVAL) IDOT=1
      ELSE IF (WHCH(1:2).EQ.'EL') THEN
        ELPF=LVAL
      ELSE IF (WHCH(1:2).EQ.'LA') THEN
        LBLF=LVAL
      ELSE IF (WHCH(1:2).EQ.'PE') THEN
        PRMF=LVAL
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
  901 IIER=14
      CALL MAPCEM (' MAPSTL - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      RETURN
C
      END
