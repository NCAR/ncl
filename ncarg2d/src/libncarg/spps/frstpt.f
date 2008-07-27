C
C $Id: frstpt.f,v 1.7 2008-07-27 00:17:24 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FRSTPT (PX,PY)
C
C Given the user coordinates PX and PY of a point, FRSTPT generates a
C pen-up move to that point.
C
      IF (ICFELL('FRSTPT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(PX),CUFY(PY),0)
      IF (ICFELL('FRSTPT',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
