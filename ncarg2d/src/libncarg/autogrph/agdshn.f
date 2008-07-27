C
C $Id: agdshn.f,v 1.6 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      CHARACTER*16 FUNCTION AGDSHN (IDSH)
C
C The value of this function is the name of the dash pattern numbered
C IDSH - that is to say, the character string 'DASH/PATTERN/n.', where
C n is an integer between 1 and 99, equal to MAX(1,MIN(99,IDSH)).
C
      AGDSHN='DASH/PATTERN/  .'
C
      KDSH=MAX(1,MIN(99,IDSH))
C
      DO 101 I=15,14,-1
        AGDSHN(I:I)=CHAR(ICHAR('0')+MOD(KDSH,10))
        IF (KDSH.LE.9) GO TO 102
        KDSH=KDSH/10
  101 CONTINUE
C
  102 RETURN
C
      END
