C
C $Id: icloem.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION ICLOEM (MESSG)
C
        CHARACTER*(*) MESSG
C
C ICLOEM(MESSG) is the index of the last non-blank character in MESSG.
C
        DO 101 I=LEN(MESSG),1,-1
         IF (MESSG(I:I).NE.' ') THEN
           ICLOEM=I
           RETURN
         END IF
  101   CONTINUE
C
        ICLOEM=1
C
        RETURN
C
      END
