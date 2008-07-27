C
C $Id: curvpidp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      FUNCTION CURVPIDP(XL,XU,N,X,Y,P,YP,SIGMA)
      DOUBLE PRECISION CURVPIDP
      DOUBLE PRECISION SIGMAP
      DOUBLE PRECISION X1PP
      DOUBLE PRECISION XXL
      DOUBLE PRECISION XXU
      DOUBLE PRECISION XSAVE
      DOUBLE PRECISION XIL
      DOUBLE PRECISION XIU
      DOUBLE PRECISION S1
      DOUBLE PRECISION DELS
      DOUBLE PRECISION SS
      DOUBLE PRECISION CS
      DOUBLE PRECISION S2
      DOUBLE PRECISION DEL1
      DOUBLE PRECISION DEL2
      DOUBLE PRECISION T1
      DOUBLE PRECISION T2
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION S3
      DOUBLE PRECISION S4
      DOUBLE PRECISION S5
      DOUBLE PRECISION S6
      DOUBLE PRECISION S7
      DOUBLE PRECISION S8
      DOUBLE PRECISION DELU1
      DOUBLE PRECISION DELU2
      DOUBLE PRECISION DELL1
      DOUBLE PRECISION DELL2
      DOUBLE PRECISION DELI
      DOUBLE PRECISION CU1
      DOUBLE PRECISION CU2
      DOUBLE PRECISION CL1
      DOUBLE PRECISION CL2
      DOUBLE PRECISION SO
      DOUBLE PRECISION SI
c
      INTEGER N
      DOUBLE PRECISION XL,XU,X(N),Y(N),P,YP(N),SIGMA
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this function integrates a curve specified by a periodic
c spline under tension between two given limits. the
c subroutine curvp1 should be called earlier to determine
c necessary parameters.
c
c on input--
c
c   xl and xu contain the upper and lower limits of inte-
c   gration, respectively. (sl need not be less than or
c   equal to xu, curvpidp (xl,xu,...) .eq. -curvpidp (xu,xl,...) ).
c
c   n contains the number of points which were specified to
c   determine the curve.
c
c   x and y are arrays containing the abscissae and
c   ordinates, respectively, of the specified points.
c
c   p contains the period.
c
c   yp is an array from subroutine curvp1 containing
c   the values of the second derivatives at the nodes.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters n, x, y, p, yp, and sigma should be input
c unaltered from the output of curvp1.
c
c on output--
c
c
c   curvpidp contains the integral value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvpdp and
c snhcshdp.
c
c--------------------------------------------------------------
c
      INTEGER UPER
      LOGICAL BDY
c
c denormalize tension factor
c
      SIGMAP = ABS(SIGMA)*DBLE(N)/P
c
c determine actual upper and lower bounds
c
      X1PP = X(1) + P
      ISIGN = 1
      ILM1 = INTRVPDP(XL,X,N,P,XXL)
      LPER = INT((XL-X(1))/P)
      IF (XL.LT.X(1)) LPER = LPER - 1
      IUM1 = INTRVPDP(XU,X,N,P,XXU)
      UPER = INT((XU-X(1))/P)
      IF (XU.LT.X(1)) UPER = UPER - 1
      IDELTP = UPER - LPER
      BDY = DBLE(IDELTP)* (XXU-XXL) .LT. 0.D0
      IF ((IDELTP.EQ.0.AND.XXU.LT.XXL) .OR. IDELTP.LT.0) ISIGN = -1
      IF (BDY) IDELTP = IDELTP - ISIGN
      IF (XXU.GE.XXL) GO TO 1
      XSAVE = XXL
      XXL = XXU
      XXU = XSAVE
      ISAVE = ILM1
      ILM1 = IUM1
      IUM1 = ISAVE
    1 IL = ILM1 + 1
      IF (ILM1.EQ.N) IL = 1
      XIL = X(IL)
      IF (ILM1.EQ.N) XIL = X1PP
      IU = IUM1 + 1
      IF (IUM1.EQ.N) IU = 1
      XIU = X(IU)
      IF (IUM1.EQ.N) XIU = X1PP
      S1 = 0.D0
      IF (ILM1.EQ.1 .OR. (IDELTP.EQ.0.AND..NOT.BDY)) GO TO 4
c
c integrate from x(1) to x(ilm1), store in s1
c
      DO 3 I = 2,ILM1
          DELS = X(I) - X(I-1)
          S1 = S1 + (Y(I)+Y(I-1))*DELS/2.D0
          IF (SIGMA.EQ.0.D0) GO TO 2
          CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
          S1 = S1 + (YP(I)+YP(I-1))*DELS* (CS-SS/2.D0)/
     +         (SIGMAP*SIGMAP* (1.D0+SS))
          GO TO 3
    2     S1 = S1 - (YP(I)+YP(I-1))*DELS*DELS*DELS/24.D0
    3 CONTINUE
    4 S2 = 0.D0
      IF (X(ILM1).GE.XXL .OR. (IDELTP.EQ.0.AND..NOT.BDY)) GO TO 6
c
c integrate from x(ilm1) to xxl, store in s2
c
      DEL1 = XXL - X(ILM1)
      DEL2 = XIL - XXL
      DELS = XIL - X(ILM1)
      T1 = DEL1*DEL1/ (2.D0*DELS)
      T2 = (DEL2+DELS)*DEL1/ (2.D0*DELS)
      S2 = T1*Y(IL) + T2*Y(ILM1)
      IF (SIGMA.EQ.0.D0) GO TO 5
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      S2 = S2 + (YP(IL)*DEL1*DEL1* (C1-SS/2.D0)+
     +     YP(ILM1)* (DELS*DELS* (CS-SS/2.D0)-DEL2*DEL2* (C2-SS/2.D0)))/
     +     (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 6
    5 S2 = S2 - T1* (DEL2* (DEL1+DELS)+DELS*DELS)*YP(IL)/12.D0 -
     +     T2*T2*DELS*YP(ILM1)/6.D0
    6 S3 = 0.D0
      IF (XXL.GE.XIL .OR. (IDELTP.EQ.0.AND.BDY) .OR.
     +    ILM1.EQ.IUM1) GO TO 8
c
c integrate from xxl to xil, store in s3
c
      DEL1 = XXL - X(ILM1)
      DEL2 = XIL - XXL
      DELS = XIL - X(ILM1)
      T1 = (DEL1+DELS)*DEL2/ (2.D0*DELS)
      T2 = DEL2*DEL2/ (2.D0*DELS)
      S3 = T1*Y(IL) + T2*Y(ILM1)
      IF (SIGMA.EQ.0.D0) GO TO 7
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      S3 = S3 + ((DELS*DELS* (CS-SS/2.D0)-DEL1*DEL1* (C1-SS/2.D0))*
     +     YP(IL)+DEL2*DEL2* (C2-SS/2.D0)*YP(ILM1))/
     +     (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 8
    7 S3 = S3 - T1*T1*DELS*YP(IL)/6.D0 -
     +     T2* (DEL1* (DEL2+DELS)+DELS*DELS)*YP(ILM1)/12.D0
    8 S4 = 0.D0
      IF (ILM1.GE.IUM1-1 .OR. (IDELTP.EQ.0.AND.BDY)) GO TO 11
c
c integrate from xil to x(ium1), store in s4
c
      ILP1 = IL + 1
      DO 10 I = ILP1,IUM1
          DELS = X(I) - X(I-1)
          S4 = S4 + (Y(I)+Y(I-1))*DELS/2.D0
          IF (SIGMA.EQ.0.D0) GO TO 9
          CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
          S4 = S4 + (YP(I)+YP(I-1))*DELS* (CS-SS/2.D0)/
     +         (SIGMAP*SIGMAP* (1.D0+SS))
          GO TO 10
    9     S4 = S4 - (YP(I)+YP(I-1))*DELS*DELS*DELS/24.D0
   10 CONTINUE
   11 S5 = 0.D0
      IF (X(IUM1).GE.XXU .OR. (IDELTP.EQ.0.AND.BDY) .OR.
     +    ILM1.EQ.IUM1) GO TO 13
c
c integrate from x(ium1) to xxu, store in s5
c
      DEL1 = XXU - X(IUM1)
      DEL2 = XIU - XXU
      DELS = XIU - X(IUM1)
      T1 = DEL1*DEL1/ (2.D0*DELS)
      T2 = (DEL2+DELS)*DEL1/ (2.D0*DELS)
      S5 = T1*Y(IU) + T2*Y(IUM1)
      IF (SIGMA.EQ.0.D0) GO TO 12
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      S5 = S5 + (YP(IU)*DEL1*DEL1* (C1-SS/2.D0)+
     +     YP(IUM1)* (DELS*DELS* (CS-SS/2.D0)-DEL2*DEL2* (C2-SS/2.D0)))/
     +     (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 13
   12 S5 = S5 - T1* (DEL2* (DEL1+DELS)+DELS*DELS)*YP(IU)/12.D0 -
     +     T2*T2*DELS*YP(IUM1)/6.D0
   13 S6 = 0.D0
      IF (XXU.GE.XIU .OR. (IDELTP.EQ.0.AND..NOT.BDY)) GO TO 15
c
c integrate from xxu to xiu, store in s6
c
      DEL1 = XXU - X(IUM1)
      DEL2 = XIU - XXU
      DELS = XIU - X(IUM1)
      T1 = (DEL1+DELS)*DEL2/ (2.D0*DELS)
      T2 = DEL2*DEL2/ (2.D0*DELS)
      S6 = T1*Y(IU) + T2*Y(IUM1)
      IF (SIGMA.EQ.0.D0) GO TO 14
      CALL SNHCSHDP(DUMMY,C1,SIGMAP*DEL1,2)
      CALL SNHCSHDP(DUMMY,C2,SIGMAP*DEL2,2)
      CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
      S6 = S6 + ((DELS*DELS* (CS-SS/2.D0)-DEL1*DEL1* (C1-SS/2.D0))*
     +     YP(IU)+DEL2*DEL2* (C2-SS/2.D0)*YP(IUM1))/
     +     (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 15
   14 S6 = S6 - T1*T1*DELS*YP(IU)/6.D0 -
     +     T2* (DEL1* (DEL2+DELS)+DELS*DELS)*YP(IUM1)/12.D0
   15 S7 = 0.D0
      IF (IU.EQ.1 .OR. (IDELTP.EQ.0.AND..NOT.BDY)) GO TO 18
c
c integrate from xiu to x1pp, store in s7
c
      NP1 = N + 1
      IUP1 = IU + 1
      DO 17 II = IUP1,NP1
          IM1 = II - 1
          I = II
          IF (I.EQ.NP1) I = 1
          DELS = X(I) - X(IM1)
          IF (DELS.LE.0.D0) DELS = DELS + P
          S7 = S7 + (Y(I)+Y(IM1))*DELS/2.D0
          IF (SIGMA.EQ.0.D0) GO TO 16
          CALL SNHCSHDP(SS,CS,SIGMAP*DELS,3)
          S7 = S7 + (YP(I)+YP(IM1))*DELS* (CS-SS/2.D0)/
     +         (SIGMAP*SIGMAP* (1.D0+SS))
          GO TO 17
   16     S7 = S7 - (YP(I)+YP(IM1))*DELS*DELS*DELS/24.D0
   17 CONTINUE
   18 S8 = 0.D0
      IF (ILM1.LT.IUM1 .OR. (IDELTP.EQ.0.AND.BDY)) GO TO 20
c
c integrate from xxl to xxu, store in s8
c
      DELU1 = XXU - X(IUM1)
      DELU2 = XIU - XXU
      DELL1 = XXL - X(IUM1)
      DELL2 = XIU - XXL
      DELS = XIU - X(IUM1)
      DELI = XXU - XXL
      T1 = (DELU1+DELL1)*DELI/ (2.D0*DELS)
      T2 = (DELU2+DELL2)*DELI/ (2.D0*DELS)
      S8 = T1*Y(IU) + T2*Y(IUM1)
      IF (SIGMA.EQ.0.D0) GO TO 19
      CALL SNHCSHDP(DUMMY,CU1,SIGMAP*DELU1,2)
      CALL SNHCSHDP(DUMMY,CU2,SIGMAP*DELU2,2)
      CALL SNHCSHDP(DUMMY,CL1,SIGMAP*DELL1,2)
      CALL SNHCSHDP(DUMMY,CL2,SIGMAP*DELL2,2)
      CALL SNHCSHDP(SS,DUMMY,SIGMAP*DELS,-1)
      S8 = S8 + (YP(IU)* (DELU1*DELU1* (CU1-SS/2.D0)-DELL1*DELL1* (CL1-
     +     SS/2.D0))+YP(IUM1)* (DELL2*DELL2* (CL2-SS/2.D0)-
     +     DELU2*DELU2* (CU2-SS/2.D0)))/ (SIGMAP*SIGMAP*DELS* (1.D0+SS))
      GO TO 20
   19 S8 = S8 - T1* (DELU2* (DELS+DELU1)+DELL2* (DELS+DELL1))*YP(IU)/
     +     12.D0 - T2* (DELL1* (DELS+DELL2)+DELU1* (DELS+DELU2))*
     +     YP(IUM1)/12.D0
   20 SO = S1 + S2 + S6 + S7
      SI = S3 + S4 + S5 + S8
      IF (BDY) GO TO 21
      CURVPIDP = DBLE(IDELTP)* (SO+SI) + DBLE(ISIGN)*SI
      RETURN
   21 CURVPIDP = DBLE(IDELTP)* (SO+SI) + DBLE(ISIGN)*SO
      RETURN
      END
