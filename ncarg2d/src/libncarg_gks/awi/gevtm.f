C
C	$Id: gevtm.f,v 1.5 2000-08-22 15:08:00 haley Exp $
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
      SUBROUTINE GEVTM(X0,Y0,DX,DY,PHI,SX,SY,SW,MOUT)
C
C  EVALUATE TRANSFORMATION MATRIX
C
      INTEGER EEVTM
      PARAMETER (EEVTM=105)
C
      REAL X0,Y0,DX,DY,PHI,MOUT(2,3)
      INTEGER SW
C
C  Check if GKS is in the proper state.
C
C     CALL GZCKST(8,EEVTM,IER)
C     IF (IER .NE. 0) RETURN
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
C  Calculate the transformtion matrix.
C
      IF (PHI .EQ. 0.) THEN
        MOUT(1,1) = SX
        MOUT(1,2) = 0.
        MOUT(1,3) = DNX+XN0-XN0*SX
        MOUT(2,1) = 0.
        MOUT(2,2) = SY
        MOUT(2,3) = DNY+YN0-YN0*SY
        GO TO 10
      ELSE
        CPHI = COS(PHI)
        SPHI = SIN(PHI)
C
        MOUT(1,1) =  SX*CPHI
        MOUT(1,2) = -SY*SPHI
        MOUT(1,3) =  DNX+XN0-XN0*SX*CPHI+YN0*SY*SPHI
        MOUT(2,1) =  SX*SPHI
        MOUT(2,2) =  SY*CPHI
        MOUT(2,3) =  DNY+YN0-XN0*SX*SPHI-YN0*SY*CPHI
        GO TO 10
      ENDIF
C
   10 CONTINUE
      RETURN
      END
