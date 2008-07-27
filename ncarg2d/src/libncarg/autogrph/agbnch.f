C
C $Id: agbnch.f,v 1.6 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      CHARACTER*16 FUNCTION AGBNCH (IDSH)
C
C The value of this function is the character-dash-pattern equivalent of
C the integer dash pattern IDSH, a string of quotes and/or dollar signs.
C Note that the support routines IAND and ISHIFT are used.
C
      KDSH=IDSH
C
      DO 101 I=16,1,-1
        IF (IAND(KDSH,1).EQ.0) THEN
          AGBNCH(I:I)=''''
        ELSE
          AGBNCH(I:I)='$'
        END IF
        KDSH=ISHIFT(KDSH,-1)
  101 CONTINUE
C
      RETURN
C
      END
