C
C	$Id: wmdrfl.f,v 1.2 2000-07-12 16:27:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
