C
C $Id: shgetnp.f,v 1.4 2000-08-23 21:21:34 fred Exp $
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
      SUBROUTINE SHGETNP(PX,PY,PZ,N,X,Y,Z,IFLAG,IWK,RWK,NP,IER)
      DIMENSION X(N),Y(N),Z(N),IWK(*),RWK(*)

C
C***********************************************************
C
C  This subroutine provides a simplified interface to
C  SHGETNP3.
C
C  On input --
C
C    PX,PY,PZ = Cartesian coordinates of the point P whose
C               nearest unmarked neighbor is to be found.
C
C    N = The number of input data values.
C
C    X,Y,Z = Arrays of length N, for N .GE. 2, containing
C            the Cartesian coordinates of the input data points.
C
C    IFLAG = 0 if this is the first call to this subroutine
C            for the given dataset and = 1 otherwise. 
C
C    IWK = An integer workspace dimensioned for at least 2*N.
C
C    RWK = A real workspace dimensioned for at least 11*N+6.
C
C  On output --
C
C    NP - (X(NP),Y(NP),Z(NP)) is the nearest input data point 
C         to P.  NP = 0 if IER .NE. 0.  On successive calls to
C         this subroutine after the first (IFLAG=1) you can 
C         find the Mth closest point to (PX,PY,PZ) with the Mth 
C         call.
C
C
C  Modules required by SHGETNP3 -- none
C
C  Intrinsic functions called by SHGETNP3 -- IABS, IFIX, SQRT
C
C***********************************************************
C
C  Define defaults for the parameter values if they have not
C  been set.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
C
      EXTERNAL SHBLDA
C
      IF (NMLSTQ .LT. 0) THEN
        NMLSTQ = MIN(17,N-1)
      ENDIF
C
      IF (NMINFL .LT. 0) THEN
        NMINFL = MIN(32,N-1)
      ENDIF
C
      IF (NMCELS .LT. 0) THEN
        RTMP = (REAL(N)/3.)**(0.333333)
        IF (RTMP-REAL(INT(RTMP)) .NE. 0.) THEN
          NMCELS = INT(RTMP)+1
        ELSE
          NMCELS = INT(RTMP)
        ENDIF 
      ENDIF
C
C  Initialize if this is the first call.
C
      IF (IFLAG .EQ. 0) THEN
        DO 10 I=1,N
          RWK(I) = X(I)
   10   CONTINUE
        CALL SHQSHEP(N,X,Y,Z,RWK(1),NMLSTQ,NMINFL,NMCELS,IWK(1),
     +              IWK(N+1),RWK(N+1),RWK(N+4),RMAX,
     +              RWK(N+7),RWK(2*N+7),IER)
      ENDIF
C
C  Get the index.
C
      CALL SHGETNP3(PX,PY,PZ,X,Y,Z,NMCELS,IWK(1),IWK(N+1),RWK(N+1),
     +              RWK(N+4),NP,DSQ)
C
      RETURN
      END
