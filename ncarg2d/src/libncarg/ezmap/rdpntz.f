C
C $Id: rdpntz.f,v 1.2 2005-06-22 21:36:49 kennison Exp $
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
      FUNCTION RDPNTZ (DVIN)
C
        DOUBLE PRECISION DVIN,DVAL,SVAL,TVAL
C
C "RDPNTZ" stands for "Round Double Precision Number Toward Zero".  The
C value of RDPNTZ is the real number having the largest absolute value
C which is less than or equal to the value of the double-precision
C argument DVIN.
C
        IF (DVIN.LT.0.D0) THEN
          DVAL=-DVIN
          DSGN=-1.
        ELSE
          DVAL=+DVIN
          DSGN=+1.
        END IF
C
        IF (DBLE(REAL(DVAL)).LE.DVAL) THEN
          RDPNTZ=SIGN(REAL(DVAL),DSGN)
        ELSE
          SVAL=.5D0
          TVAL=.25D0
          NTMS=0
  101     IF (DBLE(REAL(SVAL*DVAL)).LE.DVAL) THEN
            RDPNTZ=SIGN(REAL(SVAL*DVAL),DSGN)
            SVAL=SVAL+TVAL
            TVAL=TVAL/2.D0
            NTMS=NTMS+1
            IF (NTMS.LT.64) GO TO 101
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
