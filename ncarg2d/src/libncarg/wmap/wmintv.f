C
C	$Id: wmintv.f,v 1.3 2000-08-22 15:07:45 haley Exp $
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
      SUBROUTINE WMINTV(X0,X,N,NDX)
C
C  Given a value X0 and an array X (of length N) of monotone
C  increasing values, this subroutine returns the index NDX
C  such that:
C
C    NDX = 1    if  X0 .LT. X(2)
C    NDX = N-1  if  X(N-1) .LT. X0
C    NDX = I    if  X(I) .LE. X0  and  X0 .LE. X(I+1)
C
      DIMENSION X(N)
C
      XX = X0
C
C  Handle extremes.
C
      IF (XX .LE. X(2)) THEN
        NDX = 1
        RETURN
      ELSE IF (XX .GE. X(N-1)) THEN
        NDX = N-1
        RETURN
      END IF
C
C  Binary search.
C
      IL = 2
      IH = N-1
   10 CONTINUE
      I = (IL+IH)/2
      IF (XX .LT. X(I)) THEN
         IH = I
      ELSE IF (XX .GT. X(I+1)) THEN
         IL = I+1
      ELSE
         NDX = I
         RETURN
      END IF
      GO TO 10
      END
