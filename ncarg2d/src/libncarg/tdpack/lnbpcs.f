C
C $Id: lnbpcs.f,v 1.3 2000-08-22 15:07:06 haley Exp $
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
      FUNCTION LNBPCS (CHRS)
C
        CHARACTER*(*) CHRS
C
C The value of LNBPCS(CHRS), where CHRS is a character string of
C arbitrary length starting with a non-blank, is the position of
C the last character in CHRS which is non-blank (the "Length of
C the Non-Blank Portion of the Character String").  For an all-blank
C string, the value 1 is returned.
C
        NCHS=LEN(CHRS)
C
        DO 101 I=NCHS,1,-1
          IF (CHRS(I:I).NE.' ') THEN
            LNBPCS=I
            RETURN
          END IF
  101   CONTINUE
C
        LNBPCS=1
C
C Done.
C
        RETURN
C
      END
