C
C $Id: curvidp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C NOTE: If you make any changes to this software, please remember to
C make the same changes to the corresponding single precision routine.
C
      FUNCTION CURVIDP(XL,XU,N,X,Y,YP,SIGMA)
      DOUBLE PRECISION CURVIDP
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION XXL
      DOUBLE PRECISION XXU
      DOUBLE PRECISION SSIGN
      DOUBLE PRECISION SUM
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION DELS
      DOUBLE PRECISION T1
      DOUBLE PRECISION T2
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION SS
      DOUBLE PRECISION CS
      DOUBLE PRECISION DELU1
      DOUBLE PRECISION DELU2
      DOUBLE PRECISION DELL1
      DOUBLE PRECISION DELL2
      DOUBLE PRECISION DELI
      DOUBLE PRECISION CU1
      DOUBLE PRECISION CU2
      DOUBLE PRECISION CL1
      DOUBLE PRECISION CL2
c
      INTEGER N
      DOUBLE PRECISION XL,XU,X(N),Y(N),YP(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function integrates a curve specified by a spline
c under tension between two given limits. the subroutine
c curv1 should be called earlier to determine necessary
c parameters.
c
c on input--
c
c   xl and xu contain the upper and lower limits of inte-
c   gration, respectively. (sl need not be less than or
c   equal to xu, curvidp (xl,xu,...) .eq. -curvidp (xu,xl,...) ).
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   yp is an array from subroutine curv1 containing
c   the values of the second derivatives at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, yp, and sigma should be input
c unaltered from the output of curv1.
c
c on output--
c
c   curvidp contains the integral value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvldp and
c snhcshdp.
c
c-----------------------------------------------------------
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N-1)/ (X(N)-X(1))
c
c determine actual upper and lower bounds
c
      XXL = XL
      XXU = XU
      SSIGN = 1.D0
      IF (XL.LT.XU) GO TO 1
      XXL = XU
      XXU = XL
      SSIGN = -1.D0
      IF (XL.GT.XU) GO TO 1
c
c return zero if xl .eq. xu
c
      CURVIDP = 0.D0
      RETURN
c
c search for proper intervals
c
    1 ILM1 = INTRVLDP(XXL,X,N)
      IL = ILM1 + 1
      IUM1 = INTRVLDP(XXU,X,N)
      IU = IUM1 + 1
      IF (IL.EQ.IU) GO TO 8
c
c integrate from xxl to x(il)
c
      SUM = 0.D0
      IF (XXL.EQ.X(IL)) GO TO 3
      DEL1 = XXL - X(ILM1)
      DEL2 = X(IL) - XXL
      DELS = X(IL) - X(ILM1)
      T1 = (DEL1+DELS)*DEL2/ (2.D0*DELS)
      T2 = DEL2*DEL2/ (2.D0*DELS)
      SUM = T1*Y(IL) + T2*Y(ILM1)
      IF (SIGMA.EQ.0.D0) GO TO 2
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      SUM = SUM + ((DELS*DELS* (CS-SS/2.D0)-DEL1*DEL1* (C1-SS/2.D0))*
     +      YP(IL)+DEL2*DEL2* (C2-SS/2.D0)*YP(ILM1))/
     +      (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 3
    2 SUM = SUM - T1*T1*DELS*YP(IL)/6.D0 -
     +      T2* (DEL1* (DEL2+DELS)+DELS*DELS)*YP(ILM1)/12.D0
c
c integrate over interior intervals
c
    3 IF (IU-IL.EQ.1) GO TO 6
      ILP1 = IL + 1
      DO 5 I = ILP1,IUM1
          DELS = X(I) - X(I-1)
          SUM = SUM + (Y(I)+Y(I-1))*DELS/2.D0
          IF (SIGMA.EQ.0.D0) GO TO 4
          CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
          SUM = SUM + (YP(I)+YP(I-1))*DELS* (CS-SS/2.D0)/
     +          (SIGMAP*SIGMAP* (1.D0+SS))
          GO TO 5
    4     SUM = SUM - (YP(I)+YP(I-1))*DELS*DELS*DELS/24.D0
    5 CONTINUE
c
c integrate from x(iu-1) to xxu
c
    6 IF (XXU.EQ.X(IUM1)) GO TO 10
      DEL1 = XXU - X(IUM1)
      DEL2 = X(IU) - XXU
      DELS = X(IU) - X(IUM1)
      T1 = DEL1*DEL1/ (2.D0*DELS)
      T2 = (DEL2+DELS)*DEL1/ (2.D0*DELS)
      SUM = SUM + T1*Y(IU) + T2*Y(IUM1)
      IF (SIGMA.EQ.0.D0) GO TO 7
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      SUM = SUM + (YP(IU)*DEL1*DEL1* (C1-SS/2.D0)+
     +      YP(IUM1)* (DELS*DELS* (CS-SS/2.D0)-DEL2*DEL2* (C2-SS/2.D0)))
     +      / (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 10
    7 SUM = SUM - T1* (DEL2* (DEL1+DELS)+DELS*DELS)*YP(IU)/12.D0 -
     +      T2*T2*DELS*YP(IUM1)/6.D0
      GO TO 10
c
c integrate from xxl to xxu
c
    8 DELU1 = XXU - X(IUM1)
      DELU2 = X(IU) - XXU
      DELL1 = XXL - X(IUM1)
      DELL2 = X(IU) - XXL
      DELS = X(IU) - X(IUM1)
      DELI = XXU - XXL
      T1 = (DELU1+DELL1)*DELI/ (2.D0*DELS)
      T2 = (DELU2+DELL2)*DELI/ (2.D0*DELS)
      SUM = T1*Y(IU) + T2*Y(IUM1)
      IF (SIGMA.EQ.0.D0) GO TO 9
      CALL SNHCSHDP(DUMMY,CU1,SIGMAP*DELU1,2)
      CALL SNHCSHDP(DUMMY,CU2,SIGMAP*DELU2,2)
      CALL SNHCSHDP(DUMMY,CL1,SIGMAP*DELL1,2)
      CALL SNHCSHDP(DUMMY,CL2,SIGMAP*DELL2,2)
      CALL SNHCSHDP(SS,DUMMY,SIGMAP*DELS,-1)
      SUM = SUM + (YP(IU)* (DELU1*DELU1* (CU1-SS/2.D0)-
     +      DELL1*DELL1* (CL1-SS/2.D0))+YP(IUM1)*
     +      (DELL2*DELL2* (CL2-SS/2.D0)-DELU2*DELU2* (CU2-SS/2.D0)))/
     +      (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 10
    9 SUM = SUM - T1* (DELU2* (DELS+DELU1)+DELL2* (DELS+DELL1))*YP(IU)/
     +      12.D0 - T2* (DELL1* (DELS+DELL2)+DELU1* (DELS+DELU2))*
     +      YP(IUM1)/12.D0
c
c correct sign and return
c
   10 CURVIDP = SSIGN*SUM
      RETURN
      END
