C
C $Id: mdsetl.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDSETL (WHCH,LVAL)
C
        CHARACTER*(*) WHCH
        LOGICAL       LVAL
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
        IF (ICFELL('MDSETL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
          IDTL=0
          IF (LVAL) IDTL=1
        ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
          IDOT=0
          IF (LVAL) IDOT=1
        ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
          ELPF=LVAL
        ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
          LBLF=LVAL
        ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
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
  901   CALL MDPCEM ('MDSETL - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RETURN
C
      END
