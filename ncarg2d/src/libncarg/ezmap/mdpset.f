C
C $Id: mdpset.f,v 1.1 2001-08-16 23:09:35 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MDPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2(2),ARG3(2),ARG4(2),ARG5(2)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),
     +                   PDCL(12)
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
