C
C $Id: mapgtl.f,v 1.4 1993-12-21 00:32:54 kennison Exp $
C
      SUBROUTINE MAPGTL (WHCH,LVAL)
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
      IF      (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
        LVAL=IDTL.NE.0
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        LVAL=IDOT.NE.0
      ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
        LVAL=ELPF
      ELSE IF (WHCH(1:2).EQ.'IN'.OR.WHCH(1:2).EQ.'in') THEN
        LVAL=INTF
      ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
        LVAL=LBLF
      ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
        LVAL=PRMF
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
  901 IIER=3
      CALL MAPCEM (' MAPGTL - UNKNOWN PARAMETER NAME ',WHCH,IIER,1)
      LVAL=.FALSE.
      RETURN
C
      END
