C
C	$Id: csaplyrt.f,v 1.5 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
