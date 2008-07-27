C
C $Id: line3w.f,v 1.5 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LINE3W (XA,YA,XB,YB)
      SAVE
      COMMON /PRM31/  Q          ,L
      GO TO ( 10, 30, 40),L
   10 UA = Q
      UB = Q
      VA = XA
      VB = XB
   20 WA = YA
      WB = YB
      GO TO  50
   30 UA = XA
      UB = XB
      VA = Q
      VB = Q
      GO TO  20
   40 UA = XA
      UB = XB
      VA = YA
      VB = YB
      WA = Q
      WB = Q
   50 CALL LINE3 (UA,VA,WA,UB,VB,WB)
      RETURN
      END
