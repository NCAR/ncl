C
C $Id: mdilnb.f,v 1.1 2001-08-16 23:10:46 kennison Exp $
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
      INTEGER FUNCTION MDILNB (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of MDILNB(CHRS) is the index of the last non-blank in the
C character string CHRS.
C
C Declare local variables.
C
        INTEGER I
C
        DO 101 I=LEN(CHRS),1,-1
          IF (CHRS(I:I).NE.' ') THEN
            MDILNB=I
            RETURN
          END IF
  101   CONTINUE
C
        MDILNB=1
C
        RETURN
C
      END
