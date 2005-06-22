C
C $Id: mapset.f,v 1.16 2005-06-22 21:36:43 kennison Exp $
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
      SUBROUTINE MAPSET (ARG1,ARG2,ARG3,ARG4,ARG5)
        CHARACTER*(*) ARG1
        REAL          ARG2(2),ARG3(2),ARG4(2),ARG5(2)
        DOUBLE PRECISION DRG2(2),DRG3(2),DRG4(2),DRG5(2)
        DRG2(1)=DBLE(ARG2(1))
        DRG2(2)=DBLE(ARG2(2))
        DRG3(1)=DBLE(ARG3(1))
        DRG3(2)=DBLE(ARG3(2))
        DRG4(1)=DBLE(ARG4(1))
        DRG4(2)=DBLE(ARG4(2))
        DRG5(1)=DBLE(ARG5(1))
        DRG5(2)=DBLE(ARG5(2))
        CALL MDPSET (ARG1,DRG2,DRG3,DRG4,DRG5)
        IF (ICFELL('MAPSET',2).NE.0) RETURN
        RETURN
      END
