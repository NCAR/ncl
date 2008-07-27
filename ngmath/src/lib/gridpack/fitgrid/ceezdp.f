C
C $Id: ceezdp.f,v 1.3 2008-07-27 03:10:10 haley Exp $
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
      SUBROUTINE CEEZDP(DEL1,DEL2,SIGMA,C1,C2,C3,N)
      DOUBLE PRECISION DEL
      DOUBLE PRECISION DUMMY
      DOUBLE PRECISION COSHM1
      DOUBLE PRECISION COSHM2
      DOUBLE PRECISION DELP
      DOUBLE PRECISION DELM
      DOUBLE PRECISION SINHMP
      DOUBLE PRECISION SINHMM
      DOUBLE PRECISION DENOM
c
      DOUBLE PRECISION DEL1,DEL2,SIGMA,C1,C2,C3
c
c                                 coded by alan kaylor cline
c                           from fitpack -- january 26, 1987
c                        a curve and surface fitting package
c                      a product of pleasant valley software
c                  8603 altus cove, austin, texas 78759, usa
c
c this subroutine determines the coefficients c1, c2, and c3
c used to determine endpoint slopes. specifically, if
c function values y1, y2, and y3 are given at points x1, x2,
c and x3, respectively, the quantity c1*y1 + c2*y2 + c3*y3
c is the value of the derivative at x1 of a spline under
c tension (with tension factor sigma) passing through the
c three points and having third derivative equal to zero at
c x1. optionally, only two values, c1 and c2 are determined.
c
c on input--
c
c   del1 is x2-x1 (.gt. 0.).
c
c   del2 is x3-x1 (.gt. 0.). if n .eq. 2, this parameter is
c   ignored.
c
c   sigma is the tension factor.
c
c and
c
c   n is a switch indicating the number of coefficients to
c   be returned. if n .eq. 2 only two coefficients are
c   returned. otherwise all three are returned.
c
c on output--
c
c   c1, c2, and c3 contain the coefficients.
c
c none of the input parameters are altered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      IF (N.EQ.2) GO TO 2
      IF (SIGMA.NE.0.D0) GO TO 1
      DEL = DEL2 - DEL1
c
c tension .eq. 0.
c
      C1 = - (DEL1+DEL2)/ (DEL1*DEL2)
      C2 = DEL2/ (DEL1*DEL)
      C3 = -DEL1/ (DEL2*DEL)
      RETURN
c
c tension .ne. 0.
c
    1 CALL SNHCSHDP(DUMMY,COSHM1,SIGMA*DEL1,1)
      CALL SNHCSHDP(DUMMY,COSHM2,SIGMA*DEL2,1)
      DELP = SIGMA* (DEL2+DEL1)/2.D0
      DELM = SIGMA* (DEL2-DEL1)/2.D0
      CALL SNHCSHDP(SINHMP,DUMMY,DELP,-1)
      CALL SNHCSHDP(SINHMM,DUMMY,DELM,-1)
      DENOM = COSHM1* (DEL2-DEL1) - 2.D0*DEL1*DELP*DELM* (1.D0+SINHMP)*
     +        (1.D0+SINHMM)
      C1 = 2.D0*DELP*DELM* (1.D0+SINHMP)* (1.D0+SINHMM)/DENOM
      C2 = -COSHM2/DENOM
      C3 = COSHM1/DENOM
      RETURN
c
c two coefficients
c
    2 C1 = -1.D0/DEL1
      C2 = -C1
      RETURN
      END
