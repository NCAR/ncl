C
C $Id: point3.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
