C
C	$Id: csaplyrt.f,v 1.3 2000-07-13 02:49:06 haley Exp $
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
      SUBROUTINE CSAPLYRT (G1P,G2P,CX,SX,CY,SY, G)
      DOUBLE PRECISION G1P, G2P, CX, SX, CY, SY, G(3)
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   05/09/92
C
C   This subroutine applies the inverse (transpose) of the
C rotation defined by Subroutine CSCONSTR to the vector
C (G1P G2P 0)**T, i.e., the gradient (G1P,G2P,0) in the rot-
C ated coordinate system is mapped to (G1,G2,G3) in the
C original coordinate system.
C
C On input:
C
C       G1P,G2P = X and Y components, respectively, of the
C                 gradient in the rotated coordinate system.
C
C       CX,SX,CY,SY = Elements of the rotation R constructed
C                     by Subroutine CSCONSTR.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       G = X, Y, and Z components (in that order) of the
C           inverse rotation applied to (G1P,G2P,0) --
C           gradient in the original coordinate system.
C
C Modules required by CSAPLYRT:  None
C
C***********************************************************
C
      DOUBLE PRECISION T
C
C Local parameters:
C
C T = Temporary variable
C
      T = SY*G1P
      G(1) = CY*G1P
      G(2) = CX*G2P - SX*T
      G(3) = -SX*G2P - CX*T
      RETURN
      END
