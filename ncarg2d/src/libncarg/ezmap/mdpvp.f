C
C $Id: mdpvp.f,v 1.4 2008-04-04 21:02:47 kennison Exp $
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
      SUBROUTINE MDPVP (UOLD,VOLD,U,V)
C
        DOUBLE PRECISION UOLD,VOLD,U,V
C
C Plot the line segment from (UOLD,VOLD) TO (U,V), using either a solid
C line or a dotted line (depending on the value of the common variable
C IDTL).
C
C Declare required common blocks.  See MAPBDX for descriptions of these
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
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPCMP/  NPTB,XPTB(50),YPTB(50)
        INTEGER          NPTB
        REAL             XPTB,YPTB
        SAVE   /MAPCMP/
C
C Declare local variables.
C
        DOUBLE PRECISION DELU,DELV,VLEN
C
C Select vector or dot mode.
C
        IF (IDTL.EQ.0) THEN
C
C Use a single vector.
C
          CALL VECTD (REAL(U),REAL(V))
          IF (ICFELL('MDPVP',1).NE.0) RETURN
C
        ELSE
C
C Use dots.  DELU and DELV are the u and v components of the vector
C joining (UOLD,VOLD) to (U,V) and VLEN is the length of the vector.
C
          DELU=U-UOLD
          DELV=V-VOLD
C
          VLEN=SQRT(DELU*DELU+DELV*DELV)
C
C Now distribute dots along the vector.  The first one is spaced just
C far enough along it (DATL units) to be DBTD units away from the last
C dot on the previous vector and the rest are DBTD units apart.
C
  101     IF (DATL.LT.VLEN) THEN
            IF (NPTB.GE.50) THEN
              CALL POINTS (XPTB,YPTB,NPTB,0,0)
              IF (ICFELL('MDPVP',2).NE.0) RETURN
              NPTB=0
            END IF
            NPTB=NPTB+1
            XPTB(NPTB)=REAL(UOLD+(DATL/VLEN)*DELU)
            YPTB(NPTB)=REAL(VOLD+(DATL/VLEN)*DELV)
            DATL=DATL+DBTD
            GO TO 101
          END IF
C
C Set DATL for the next call.
C
          DATL=DATL-VLEN
C
        END IF
C
C Done.
C
        RETURN
C
      END
