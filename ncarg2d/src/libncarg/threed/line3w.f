C
C	$Id: line3w.f,v 1.1.1.1 1992-04-17 22:31:47 ncargd Exp $
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
