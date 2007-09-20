C
C $Id: mdrgfp.f,v 1.3 2007-09-20 21:44:45 kennison Exp $
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
      SUBROUTINE MDRGFP (XCRA,YCRA,NCRA,ITYP)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C Declare required EZMAP common blocks.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
        IF (NCRA.GT.2.AND.ITYP.GT.0) THEN
          IF (ICSF(ITYP).GE.0) THEN
            NCRM=0
            DO 101 I=1,NCRA
              CALL MAPTRN (OLAT+YCRA(I),OLON+XCRA(I),UVAL,VVAL)
              IF (UVAL.NE.1.E12) THEN
                NCRM=NCRM+1
                XCRA(NCRM)=UVAL
                YCRA(NCRM)=VVAL
              END IF
  101       CONTINUE
            IF (NCRM.GT.2) THEN
              CALL GSFACI (ICSF(ITYP))
              CALL GFA    (NCRM,XCRA,YCRA)
            END IF
          END IF
        END IF
C
        RETURN
C
      END
