C
C $Id: mdilon.f,v 1.1 2001-08-16 23:09:28 kennison Exp $
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
      SUBROUTINE MDILON (PLAT,PLON,UCOP,VCOP,
     +                   QLAT,QLON,UCOQ,VCOQ,
     +                        RLON,UCOR,VCOR)
C
C Given the lat/lon and U/V coordinates of two points P and Q and the
C longitude of a point R along the line in the U/V plane connecting the
C points P and Q, this routine returns the U and V coordinates of R.
C
        DOUBLE PRECISION PLAT,PLON,UCOP,VCOP
        DOUBLE PRECISION QLAT,QLON,UCOQ,VCOQ
        DOUBLE PRECISION      RLON,UCOR,VCOR
C
        DOUBLE PRECISION ALAT,ALON,UCOA,VCOA
        DOUBLE PRECISION BLAT,BLON,UCOB,VCOB
        DOUBLE PRECISION CLAT,CLON,UCOC,VCOC
C
C Declare local variables.
C
        INTEGER          NREP
C
        ALAT=PLAT
        ALON=PLON
        UCOA=UCOP
        VCOA=VCOP
        BLAT=QLAT
        BLON=QLON
        UCOB=UCOQ
        VCOB=VCOQ
        UCOC=UCOA
        VCOC=VCOA
C
        NREP=0
C
  101   IF (NREP.LT.100) THEN
          IF (ALON.EQ.BLON) GO TO 102
          UCOC=UCOA+((RLON-ALON)/(BLON-ALON))*(UCOB-UCOA)
          VCOC=VCOA+((RLON-ALON)/(BLON-ALON))*(VCOB-VCOA)
          CALL MDPTRI (UCOC,VCOC,CLAT,CLON)
          IF ((UCOC-UCOA)**2+(VCOC-VCOA)**2.GT.
     +        (UCOC-UCOB)**2+(VCOC-VCOB)**2) THEN
            ALAT=CLAT
            ALON=CLON
            UCOA=UCOC
            VCOA=VCOC
          ELSE
            BLAT=CLAT
            BLON=CLON
            UCOB=UCOC
            VCOB=VCOC
          END IF
          NREP=NREP+1
          IF (ABS(CLON-RLON).GT..001D0*ABS(QLON-PLON)) GO TO 101
        END IF
C
  102   UCOR=UCOC
        VCOR=VCOC
C
        RETURN
C
      END
