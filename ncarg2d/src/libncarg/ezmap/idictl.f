C
C $Id: idictl.f,v 1.7 2000-07-12 16:23:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      INTEGER FUNCTION IDICTL (ISTR,IDCT,NDCT)
C
      CHARACTER*(*) ISTR
      CHARACTER*2 IDCT(NDCT)
C
C The value of this function is the index in the NDCT-element dictionary
C IDCT of the string ISTR.  Only the first two characters of ISTR and
C IDCT(I) are compared.  If ISTR is not found in the dictionary, the
C function value is zero.
C
      DO 101 I=1,NDCT
        IF (ISTR(1:2).EQ.IDCT(I)) THEN
          IDICTL=I
          RETURN
        END IF
  101 CONTINUE
C
C Not found.  Return a zero.
C
      IDICTL=0
      RETURN
C
      END
