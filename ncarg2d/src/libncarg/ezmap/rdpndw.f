C
C $Id: rdpndw.f,v 1.1 2002-08-21 20:28:19 kennison Exp $
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
      FUNCTION RDPNDW (DVIN)
C
        DOUBLE PRECISION DVIN
C
C "RDPNUW" stands for "Round Double Precision Number UpWard".  The
C value of RDPNDW(DVIN) is the smallest real number whose value is
C greater than or equal to the value of the double-precision argument
C DVIN.
C
        IF      (DVIN.LT.0.D0) THEN
          RDPNDW=RDPNAZ(DVIN)
        ELSE IF (DVIN.GT.0.D0) THEN
          RDPNDW=RDPNTZ(DVIN)
        ELSE
          RDPNDW=0.
        END IF
C
C Done.
C
        RETURN
C
      END
