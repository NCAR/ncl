C
C	$Id: wmfpts.f,v 1.3 2000-08-22 15:07:44 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE WMFPTS(M,X,Y,N,XT,YT)
C
C  Filter out N points from the coordinates (X(I),Y(I),I=1,M) and
C  store them in (XT(I),YT(I),I=1,N).  It is assumed that M > N.
C
      DIMENSION X(M),Y(M),XT(N),YT(N)
C
      SCL = REAL(N-1)/REAL(M-1)
      OFF = REAL(M-N)/REAL(M-1)
C
      IX = 1
      XT(1) = X(1)
      YT(1) = Y(1)
      DO 100 I=2,M
        STMP = SCL*REAL(I)+OFF
        IT = MIN(INT(STMP),N)
        IF (IT .EQ. IX) GO TO 100
        IX = IT
        XT(IX) = X(I)
        YT(IX) = Y(I)
  100 CONTINUE
      XT(N) = X(M)
      YT(N) = Y(M)
C
      RETURN
      END
