C
C $Id: mdpset.f,v 1.10 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2(2),ARG3(2),ARG4(2),ARG5(2)
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
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the map limits.
C
        I=IDICTL(ARG1,LDCT,6)
        IF (I.EQ.0) I=IDICTL(ARG1,LDCL,6)
        IF (I.EQ.0) GO TO 901
        ILTS=I
C
        PLA1=ARG2(1)
        PLA2=ARG3(1)
        PLA3=ARG4(1)
        PLA4=ARG5(1)
C
        IF (I.EQ.3) THEN
          PLB1=ARG2(2)
          PLB2=ARG3(2)
          PLB3=ARG4(2)
          PLB4=ARG5(2)
        END IF
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
  901   CALL MDPCEM ('MDPSET - UNKNOWN MAP AREA SPECIFIER ',ARG1,2,1)
        RETURN
C
      END
