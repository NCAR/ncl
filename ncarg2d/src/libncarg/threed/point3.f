C
C $Id: point3.f,v 1.2 1993-03-14 19:01:20 kennison Exp $
C
      SUBROUTINE POINT3 (U,V,W)
      DIMENSION VIEW(4),WIND(4)
C
C Save the current SET parameters.
C
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C DEFINE NORMALIZATION TRANS TO BE USED WITH POLYMARKER
C
      CALL SET(0.0, 1.0, 0.0, 1.0, 1.0, 1024.0, 1.0, 1024.0, 1)
C
C SET MARKER TYPE TO 1
C
      CALL GSMK (1)
      CALL TRN32T (U,V,W,X,Y,ZDUM,2)
      PX = X
      PY = Y
      CALL GPM (1,PX,PY)
C
C Restore original SET call.
C
      CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     -         WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
      RETURN
      END
