C
C	$Id: gactm.f,v 1.3 2000-07-12 16:39:38 haley Exp $
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
      SUBROUTINE GACTM(MINP,X0,Y0,DX,DY,PHI,SX,SY,SW,MOUT)
C
C  ACCUMULATE TRANSFORMATION MATRIX
C
      INTEGER EACTM
      PARAMETER (EACTM=106)
C
      REAL X0,Y0,DX,DY,PHI,MINP(2,3),MOUT(2,3)
      INTEGER SW
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,EACTM,IER)
      IF (IER .NE. 0) RETURN
C
C  The transformation takes place in NDC space, so do the
C  appropriate conversions.
C
      IF (SW .EQ. 0) THEN
        CALL GZW2NX(1,X0,XN0)
        CALL GZW2NY(1,Y0,YN0)
        CALL GZW2NX(1,DX,DTX)
        CALL GZW2NX(1,DY,DTY)
        CALL GZW2NX(1,0.,D0X)
        CALL GZW2NX(1,0.,D0Y)
        DNX = DTX - D0X
        DNY = DTY - D0Y
      ELSE
        XN0 = X0
        YN0 = Y0
        DNX = DX
        DNY = DY
      ENDIF        
C
C  Calculate the new transformtion matrix.
C
      IF (PHI .EQ. 0.) THEN
        MOUT(1,1) = SX*MINP(1,1)
        MOUT(1,2) = SX*MINP(1,2)
        MOUT(1,3) = SX*MINP(1,3)+DNX+XN0-XN0*SX
        MOUT(2,1) = SY*MINP(2,1)
        MOUT(2,2) = SY*MINP(2,2)
        MOUT(2,3) = SY*MINP(2,3)+DNY+YN0-YN0*SY
        GO TO 10
      ELSE
        CPHI = COS(PHI)
        SPHI = SIN(PHI)
C
        T11 = SX*CPHI
        T12 = -SY*SPHI
        T13 = DNX+XN0-XN0*SX*CPHI+YN0*SY*SPHI
        T21 = SX*SPHI
        T22 = SY*CPHI
        T23 = DNY+YN0-XN0*SX*SPHI-YN0*SY*CPHI
        MOUT(1,1) =  T11*MINP(1,1)+T12*MINP(2,1)
        MOUT(1,2) =  T11*MINP(1,2)+T12*MINP(2,2)
        MOUT(1,3) =  T11*MINP(1,3)+T12*MINP(2,3)+T13
        MOUT(2,1) =  T21*MINP(1,1)+T22*MINP(2,1)
        MOUT(2,2) =  T21*MINP(1,2)+T22*MINP(2,2)
        MOUT(2,3) =  T21*MINP(1,3)+T22*MINP(2,3)+T23
        GO TO 10
      ENDIF
C
   10 CONTINUE
      RETURN
      END
