C
C $Id: qsfndp.f,v 1.4 2001-08-16 23:13:02 kennison Exp $
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
      DOUBLE PRECISION FUNCTION QSFNDP (ECCENT,SINPHI,COSPHI)
C
C FUNCTION TO COMPUTE CONSTANT (SMALL Q).
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA HALF,ONE,TWO /0.5D0,1.0D0,2.0D0/
      DATA EPSLN /1.0D-7/
C
      IF (ECCENT .LT. EPSLN) GO TO 020
      CON = ECCENT * SINPHI
      QSFNDP = (ONE - ECCENT * ECCENT) * (SINPHI / (ONE - CON * CON) -
     .         (HALF / ECCENT) * LOG ((ONE - CON) / (ONE + CON)))
      RETURN
C
  020 QSFNDP = TWO * SINPHI
      RETURN
      END
