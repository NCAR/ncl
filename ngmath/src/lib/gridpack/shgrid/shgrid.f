C
C $Id: shgrid.f,v 1.3 2000-07-13 02:49:30 haley Exp $
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
      SUBROUTINE SHGRID (N,X,Y,Z,F, NXO,NYO,NZO,XO,YO,ZO,FF, 
     +                   IWK,RWK,IER)
      INTEGER N, NXO, NYO, NZO, IWK(*), IER
      REAL    X(N), Y(N), Z(N), F(N), XO(NXO), YO(NYO), ZO(NZO),
     +        FF(NXO,NYO,NZO),  RWK(*)
C
C***********************************************************
C
C  This subroutine provides a simple interface to the Shepard
C  algortihm interpolation package for 3D interpolation.
C
C  ON INPUT
C
C     N - The number of input data points.
C     X - An array of X coordinate values of the input data points.
C     Y - An array of Y coordinate values of the input data points.
C     Z - An array of Z coordinate values of the input data points.
C     F - The functional value at coordinate (x,y,z).
C   NXO - The dimension of the array xo containing the X coordinate
C         values for the output grid.
C   NYO - The dimension of the array yo containing the Y coordinate
C         values for the output grid.
C   NZO - The dimension of the array zo containing the z coordinate
C         values for the output grid.
C    XO - The array containing the X coordinate values for the output
C         grid (must be monotone increasing, but need not be equally
C         spaced.
C    YO - The array containing the Y coordinate values for the output
C         grid (must be monotone increasing, but need not be equally
C         spaced.
C    ZO - The array containing the Z coordinate values for the output
C         grid (must be monotone increasing, but need not be equally
C         spaced.
C   IWK - An integer workspace dimensioned for at least 2*N.
C   RWK - A real workspace dimensioned for at least 11*N+6.
C
C ON OUTPUT:
C
C    FF - Interpolated values at the grid points if IER
C         .EQ. 0 .  FF(I,J,K) = F(XO(I),YO(J),ZO(K)) 
C         for I=1,...,NXO and J=1,...,NYO and K=1,NZO.
C
C
C    IER - Error indicator
C          = 3 - NQ must be at least 9
C          = 4 - NW must be at least 1
C          = 5 - must have N < NQ < 41
C          = 6 - must have N < NW < 41
C          = 7 - cell grid dimensions must be positive
C          = 8 - duplicate input points encountered
C          = 9 - collinear input, no unique solution
C
C***********************************************************
C
C
C  Define defaults for the parameter values if they have not
C  been set.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
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
      CALL SHQSHEP(N,X,Y,Z,F,NMLSTQ,NMINFL,NMCELS,IWK(1),
     +             IWK(N+1),RWK(N+1),RWK(N+4),RMAX,
     +             RWK(N+7),RWK(2*N+7),IER)
C
C  Interpolat at the desired points.
C
      DO 20 I=1,NXO
        DO 30 J=1,NYO
          DO 40 K=1,NZO
            FF(I,J,K) = SH3VAL(XO(I),YO(J),ZO(K),N,X,Y,Z,F,
     +                         NMCELS,IWK(1),IWK(N+1),RWK(N+1),
     +                         RWK(N+4),RMAX,RWK(N+7),RWK(2*N+7))
   40     CONTINUE
   30   CONTINUE
   20 CONTINUE
C
      RETURN
      END
