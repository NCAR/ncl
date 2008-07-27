C
C $Id: line.f,v 1.9 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      IF (ICFELL('LINE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      IF (ICFELL('LINE',2).NE.0) RETURN
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      IF (ICFELL('LINE',3).NE.0) RETURN
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('LINE',4).NE.0) RETURN
      RETURN
      END
