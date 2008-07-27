C
C	$Id: csarcint.f,v 1.5 2008-07-27 03:10:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CSARCINT (P,P1,P2,F1,F2,G1,G2,SIGMA, F,G,GN)
      DOUBLE PRECISION P(3), P1(3), P2(3), F1, F2, G1(3),
     .                 G2(3), SIGMA, F, G(3), GN
C
C***********************************************************
C
C                                              From SSRFPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/21/96
C
C   Given 3 points P, P1, and P2 lying on a common geodesic
C of the unit sphere with P between P1 and P2, along with
C data values and gradients at P1 and P2, this subroutine
C computes an interpolated value F and a gradient vector G
C AT P.  F and the tangential component of G are taken to be
C the value and derivative (with respect to arc-length) of
C a Hermite interpolatory tension spline defined by the end-
C point values and tangential gradient components.  The nor-
C mal component of G is obtained by linear interpolation of
C the normal components of the gradients at P1 and P2.
C
C On input:
C
C       P = Cartesian coordinates of a point lying on the
C           arc defined by P1 and P2.  P(1)**2 + P(2)**2 +
C           P(3)**2 = 1.
C
C       P1,P2 = Coordinates of distinct points on the unit
C               sphere defining an arc with length less than
C               180 degrees.
C
C       F1,F2 = Data values associated with P1 and P2,
C               respectively.
C
C       G1,G2 = Gradient vectors associated with P1 and P2.
C               G1 and G2 are orthogonal to P1 and P2,
C               respectively.
C
C       SIGMA = Tension factor associated with P1-P2.
C
C The above parameters are not altered by this routine.
C
C       G = Array of length 3.
C
C On output:
C
C       F = Interpolated value at P.
C
C       G = Interpolated gradient at P.
C
C       GN = Normal component of G with the direction
C            P1 X P2 taken to be positive.  The extrapola-
C            tion procedure requires this component.
C
C   For each vector V, V(1), V(2), and V(3) contain X, Y,
C and Z components, respectively.
C
C SSRFPACK modules required by CSARCINT:  CSARCLEN, CSSNHCSH
C
C Intrinsic functions called by CSARCINT:  ABS, EXP, SQRT
C
C***********************************************************
C
      DOUBLE PRECISION CSARCLEN
      INTEGER I, LUN
      DOUBLE PRECISION A, AL, B1, B2, CM, CMM, CM2, DUMMY,
     .                 D1, D2, E, EMS, E1, E2, GT, S, SB1,
     .                 SB2, SIG, SINH, SINH2, SM, SM2, TAU1,
     .                 TAU2, TM, TM1, TM2, TP1, TP2, TS,
     .                 UN(3), UNORM
      DATA    LUN/6/
C
C Local parameters:
C
C A =         Angle in radians (arc-length) between P1 and
C               P2
C AL =        Arc-length between P1 and P
C B1,B2 =     Local coordinates of P with respect to P1-P2
C CM,CMM =    Coshm(SIG) and Coshmm(SIG) -- refer to CSSNHCSH
C CM2 =       Coshm(SB2)
C DUMMY =     Dummy parameter for CSSNHCSH
C D1,D2 =     Scaled second differences
C E =         CM**2 - SM*Sinh = SIG*SM - 2*CMM (scaled by
C               2*EMS if SIG > .5)
C EMS =       Exp(-SIG)
C E1,E2 =     Exp(-SB1), Exp(-SB2)
C GT =        Tangential component of G -- component in the
C               direction UN X P
C I =         DO-loop index
C LUN =       Logical unit for error messages
C S =         Slope:  (F2-F1)/A
C SB1,SB2 =   SIG*B1, SIG*B2
C SIG =       Abs(SIGMA)
C SINH =      Sinh(SIGMA)
C SINH2 =     Sinh(SB2)
C SM,SM2 =    Sinhm(SIG), Sinhm(SB2)
C TAU1,TAU2 = Tangential derivatives (components of G1,G2)
C               at P1 and P2
C TM =        1-EMS
C TM1,TM2 =   1-E1, 1-E2
C TP1,TP2 =   1+E1, 1+E2
C TS =        TM**2
C UN =        Unit normal to the plane of P, P1, and P2
C UNORM =     Euclidean norm of P1 X P2 -- used to normalize
C               UN
C
C
C Compute unit normal UN.
C
      UN(1) = P1(2)*P2(3) - P1(3)*P2(2)
      UN(2) = P1(3)*P2(1) - P1(1)*P2(3)
      UN(3) = P1(1)*P2(2) - P1(2)*P2(1)
      UNORM = SQRT(UN(1)*UN(1) + UN(2)*UN(2) + UN(3)*UN(3))
      IF (UNORM .EQ. 0.) GO TO 2
C
C Normalize UN.
C
      DO 1 I = 1,3
        UN(I) = UN(I)/UNORM
    1   CONTINUE
C
C Compute tangential derivatives at the endpoints:
C   TAU1 = (G1,UN X P1) = (G1,P2)/UNORM and
C   TAU2 = (G2,UN X P2) = -(G2,P1)/UNORM.
C
      TAU1 = (G1(1)*P2(1) + G1(2)*P2(2) + G1(3)*P2(3))/UNORM
      TAU2 =-(G2(1)*P1(1) + G2(2)*P1(2) + G2(3)*P1(3))/UNORM
C
C Compute arc-lengths A, AL.
C
      A = CSARCLEN(P1,P2)
      IF (A .EQ. 0.) GO TO 2
      AL = CSARCLEN(P1,P)
C
C Compute local coordinates, slope, and second differences.
C
      B2 = AL/A
      B1 = 1. - B2
      S = (F2-F1)/A
      D1 = S - TAU1
      D2 = TAU2 - S
C
C Test the range of SIGMA.
C
      SIG = ABS(SIGMA)
      IF (SIG .LT. 1.D-9) THEN
C
C Hermite cubic interpolation.
C
        F = F1 + AL*(TAU1 + B2*(D1 + B1*(D1 - D2)))
        GT = TAU1 + B2*(D1 + D2 + 3.*B1*(D1 - D2))
      ELSEIF (SIG .LE. .5) THEN
C
C 0 < SIG .LE. .5.  Use approximations designed to avoid
C   cancellation error in the hyperbolic functions.
C
        SB2 = SIG*B2
        CALL CSSNHCSH (SIG, SM,CM,CMM)
        CALL CSSNHCSH (SB2, SM2,CM2,DUMMY)
        SINH = SM + SIG
        SINH2 = SM2 + SB2
        E = SIG*SM - CMM - CMM
        F = F1 + AL*TAU1 + A*((CM*SM2-SM*CM2)*(D1+D2) + SIG*
     .                        (CM*CM2-SINH*SM2)*D1)/(SIG*E)
        GT = TAU1 + ((CM*CM2-SM*SINH2)*(D1+D2) + SIG*
     .               (CM*SINH2-SINH*CM2)*D1)/E
      ELSE
C
C SIG > .5.  Use negative exponentials in order to avoid
C   overflow.  Note that EMS = EXP(-SIG).
C
        SB1 = SIG*B1
        SB2 = SIG - SB1
        E1 = EXP(-SB1)
        E2 = EXP(-SB2)
        EMS = E1*E2
        TM = 1. - EMS
        TS = TM*TM
        TM1 = 1. - E1
        TM2 = 1. - E2
        E = TM*(SIG*(1.+EMS) - TM - TM)
        F = F1 + AL*S + A*(TM*TM1*TM2*(D1+D2) + SIG*
     .                     ((E2*TM1*TM1-B1*TS)*D1 +
     .                      (E1*TM2*TM2-B2*TS)*D2))/(SIG*E)
        TP1 = 1. + E1
        TP2 = 1. + E2
        GT = S + (TM1*(TM*TP2-SIG*E2*TP1)*D1 -
     .            TM2*(TM*TP1-SIG*E1*TP2)*D2)/E
      ENDIF
C
C Compute GN.
C
      GN = B1*(UN(1)*G1(1) + UN(2)*G1(2) + UN(3)*G1(3)) +
     .     B2*(UN(1)*G2(1) + UN(2)*G2(2) + UN(3)*G2(3))
C
C Compute G = GT*(UN X P) + GN*UN.
C
      G(1) = GT*(UN(2)*P(3) - UN(3)*P(2)) + GN*UN(1)
      G(2) = GT*(UN(3)*P(1) - UN(1)*P(3)) + GN*UN(2)
      G(3) = GT*(UN(1)*P(2) - UN(2)*P(1)) + GN*UN(3)
      RETURN
C
C P1 X P2 = 0.  Print an error message and terminate
C   processing.
C
    2 WRITE (LUN,100) (P1(I),I=1,3), (P2(I),I=1,3)
  100 FORMAT ('1','ERROR IN CSARCINT -- P1 = ',2(F9.6,',  '),
     .        F9.6/1X,19X,'P2 = ',2(F9.6,',  '),F9.6)
      STOP
      END
