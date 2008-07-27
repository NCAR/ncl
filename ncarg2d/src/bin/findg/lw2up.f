C
C	$Id: lw2up.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
