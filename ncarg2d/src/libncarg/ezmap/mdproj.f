C
C $Id: mdproj.f,v 1.3 2005-01-10 21:19:44 kennison Exp $
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
      SUBROUTINE MDPROJ (ARG1,ARG2,ARG3,ARG4)
C
        CHARACTER*(*)    ARG1
        DOUBLE PRECISION ARG2,ARG3,ARG4
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
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(13),
     +                   PDCL(13)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPROJ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the parameters defining the projection.
C
        I=IDICTL(ARG1,PDCT,13)
        IF (I.EQ.0) I=IDICTL(ARG1,PDCL,13)
        IF (I.EQ.0) GO TO 901
C
        JPRJ=I-1
C
        PHIA=MAX(-90.D0,MIN(90.D0,ARG2))
        PHIO=ARG3-SIGN(180.D0,ARG3+180.D0)+SIGN(180.D0,180.D0-ARG3)
        ROTA=ARG4-SIGN(180.D0,ARG4+180.D0)+SIGN(180.D0,180.D0-ARG4)
C
        IF (JPRJ.EQ.3) THEN
          CALL MDSETD ('SA',0.D0)
          IF (ICFELL('MDPROJ',2).NE.0) RETURN
        ELSE IF (JPRJ.EQ.11) THEN
          JPRJ=3
          IF (ABS(SALT).LE.1.D0) THEN
            CALL MDSETD ('SA',6.631D0)
            IF (ICFELL('MDPROJ',3).NE.0) RETURN
          END IF
        ELSE IF (JPRJ.EQ.12) THEN
          JPRJ=15
          PHIA=0.D0
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
  901   CALL MDPCEM ('MDPROJ - UNKNOWN PROJECTION NAME ',ARG1,4,1)
        RETURN
C
      END
