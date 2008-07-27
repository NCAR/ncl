C
C	$Id: wmdrfl.f,v 1.4 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDRFL(N,X,Y)
C
C  Draw the polyline (X(L),Y(L),L=1,N) with a thickness of SLINWD.
C
      DIMENSION X(N),Y(N)
C
      include 'wmcomn.h'
C
C  First, make sure that a line is drawn in case SLINWD is too
C  small.
C
      IF (SLINWD .LT. 0.01) THEN
        CALL GQLWSC(IER,OLDSCL)
        CALL GSLWSC(1.)
        CALL GPL(N,X,Y)
        CALL GSLWSC(OLDSCL)
      ENDIF
C
C  Draw fat line segments.
C
      DO 10 I=1,N-1
        CALL WMDRFS(X(I),Y(I),X(I+1),Y(I+1))
   10 CONTINUE
C
C  Draw circles at line segment joins so that there are no notches.
C
      CALL NGDOTS(X,Y,N,SLINWD,1)
C
      RETURN
      END
