C
C $Id: mdppos.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPPOS (ARG1,ARG2,ARG3,ARG4)
C
        DOUBLE PRECISION ARG1,ARG2,ARG3,ARG4
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
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPPOS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check the arguments for errors.
C
        IF (ARG1.LT.0.D0.OR.ARG1.GE.ARG2.OR.ARG2.GT.1.D0) GO TO 901
        IF (ARG3.LT.0.D0.OR.ARG3.GE.ARG4.OR.ARG4.GT.1.D0) GO TO 901
C
C Transfer in the values.
C
        XLOW=ARG1
        XROW=ARG2
        YBOW=ARG3
        YTOW=ARG4
C
C Set the flag to indicate that initialization is now required.
C
        INTF=.TRUE.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   CALL SETER ('MDPPOS - ARGUMENTS ARE INCORRECT',2,1)
        RETURN
C
      END
