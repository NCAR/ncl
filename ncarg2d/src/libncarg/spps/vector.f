C
C $Id: vector.f,v 1.7 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VECTOR (PX,PY)
C
C Draw a vector (line segment) from the current pen position to the new
C pen position (PX,PY), in the user coordinate system, and then make
C (PX,PY) the current pen position.
C
      IF (ICFELL('VECTOR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(PX),CUFY(PY),1)
      IF (ICFELL('VECTOR',2).NE.0) RETURN
      RETURN
      END
