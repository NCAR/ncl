C
C	$Id: lw2up.f,v 1.2 2000-07-12 17:04:22 haley Exp $
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
      SUBROUTINE LW2UP (NM)
C
C  Convert all lower-case letters in NM to upper case.
C
      CHARACTER*(*) NM
C
      LN = LEN(NM)
      DO 10 I=1,LN
      IF (NM(I:I) .EQ. 'a') NM(I:I) = 'A'
      IF (NM(I:I) .EQ. 'b') NM(I:I) = 'B'
      IF (NM(I:I) .EQ. 'c') NM(I:I) = 'C'
      IF (NM(I:I) .EQ. 'd') NM(I:I) = 'D'
      IF (NM(I:I) .EQ. 'e') NM(I:I) = 'E'
      IF (NM(I:I) .EQ. 'f') NM(I:I) = 'F'
      IF (NM(I:I) .EQ. 'g') NM(I:I) = 'G'
      IF (NM(I:I) .EQ. 'h') NM(I:I) = 'H'
      IF (NM(I:I) .EQ. 'i') NM(I:I) = 'I'
      IF (NM(I:I) .EQ. 'j') NM(I:I) = 'J'
      IF (NM(I:I) .EQ. 'k') NM(I:I) = 'K'
      IF (NM(I:I) .EQ. 'l') NM(I:I) = 'L'
      IF (NM(I:I) .EQ. 'm') NM(I:I) = 'M'
      IF (NM(I:I) .EQ. 'n') NM(I:I) = 'N'
      IF (NM(I:I) .EQ. 'o') NM(I:I) = 'O'
      IF (NM(I:I) .EQ. 'p') NM(I:I) = 'P'
      IF (NM(I:I) .EQ. 'q') NM(I:I) = 'Q'
      IF (NM(I:I) .EQ. 'r') NM(I:I) = 'R'
      IF (NM(I:I) .EQ. 's') NM(I:I) = 'S'
      IF (NM(I:I) .EQ. 't') NM(I:I) = 'T'
      IF (NM(I:I) .EQ. 'u') NM(I:I) = 'U'
      IF (NM(I:I) .EQ. 'v') NM(I:I) = 'V'
      IF (NM(I:I) .EQ. 'w') NM(I:I) = 'W'
      IF (NM(I:I) .EQ. 'x') NM(I:I) = 'X'
      IF (NM(I:I) .EQ. 'y') NM(I:I) = 'Y'
      IF (NM(I:I) .EQ. 'z') NM(I:I) = 'Z'
   10 CONTINUE
C
      RETURN
      END
