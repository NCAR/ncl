C
C $Id: mappos.f,v 1.2 1993-12-21 00:33:18 kennison Exp $
C
      SUBROUTINE MAPPOS (ARG1,ARG2,ARG3,ARG4)
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
C Check the arguments for errors.
C
      IF (ARG1.LT.0..OR.ARG1.GE.ARG2.OR.ARG2.GT.1.) GO TO 901
      IF (ARG3.LT.0..OR.ARG3.GE.ARG4.OR.ARG4.GT.1.) GO TO 901
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
  901 IIER=19
      CALL SETER ('MAPPOS - ARGUMENTS ARE INCORRECT',IIER,1)
      RETURN
C
      END
