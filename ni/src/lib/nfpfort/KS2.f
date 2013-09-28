C NCLFORTSTART
      SUBROUTINE TKOLMO(A,NA,B,NB,PROB,Z,RDMAX)
C==========>
C        Tests whether two one-dimensional sets of points
C        are compatible with coming from the same parent
C        distribution, using the Kolmogorov test
C   A is the vector of NA points in set one.
C   B is the vector of NB points in set two.
C   PROB is the probability of compatibility returned to
C      the calling program.
C---
C I can get this to work if the input A and B are in ascending order.
C---
C
C   F. James, April, 1987
C==========>
C   http://cernlib.web.cern.ch/cernlib/version.html
C==========>
C                          ! INPUT
      INTEGER NA,NB
      REAL A(NA),B(NB)
C                          ! OUTPUT
      REAL PROB, Z
C NCLEND

C                          ! LOCAL

C                 In case of an error, PROB is returned as -1.
      PROB = -1.0
C                 Require at least two points in each set
      IF (NA.LE.2 .OR. NB.LE.2)  GO TO 99
C                 Constants needed
      RNA = REAL(NA)
      RNB = REAL(NB)
      SA = 1.0/RNA
      SB = 1.0/RNB
C                 Starting values for main loop
      IF (A(1) .LT. B(1))  THEN
         RDIFF = -SA
         IA = 2
         IB = 1
      ELSE
         RDIFF = SB
         IB = 2
         IA = 1
      ENDIF
      RDMAX = ABS(RDIFF)
C
C             Main loop over point sets to find max distance
C             RDIFF is the running difference, and RDMAX the max.
      DO 10 I= 1, NA+NB
      IF (A(IA) .LT. B(IB)) THEN
         RDIFF = RDIFF - SA
         IA = IA + 1
         IF (IA .GT. NA)  GO TO 20
      ELSE
         RDIFF = RDIFF + SB
         IB = IB + 1
         IF (IB .GT. NB)  GO TO 20
      ENDIF
      RDMAX = MAX(RDMAX,ABS(RDIFF))
   10 CONTINUE
C            Should never terminate this loop!
      GO TO 99
   20 RDMAX = MAX(RDMAX,ABS(RDIFF))
      Z = RDMAX * SQRT(RNA*RNB/(RNA+RNB))
      PROB = PROBKL(Z)
C
   99 CONTINUE
      END
C------------ 
      FUNCTION PROBKL(X)

      REAL FJ(4),R(4)

      PARAMETER (PI = 3.14159265)
      PARAMETER (W  = 2.50662827)
      PARAMETER (C1 = -PI**2/8, C2 = 9*C1, C3 = 25*C1)

      DATA FJ /-2,-8,-18,-32/

      U=ABS(X)
      IF(U .LT. 0.2) THEN
       P=1.
      ELSEIF(U .LT. 0.755) THEN
       V=1/U**2
       P=1-W*(EXP(C1*V)+EXP(C2*V)+EXP(C3*V))/U
      ELSEIF(U .LT. 6.8116) THEN
       R(2)=0.
       R(3)=0.
       R(4)=0.
       V=U**2
       DO 1 J = 1,MAX(1,NINT(3/U))
    1  R(J)=EXP(FJ(J)*V)
       P=2*(R(1)-R(2)+R(3)-R(4))
CCC         PRINT '(35x,4e10.2)', (R(JJ),JJ=1,4)
      ELSE
       P=0
      ENDIF
      PROBKL=P
      RETURN
      END
c----------------------------------
C Numerical Recipies 
c----------------------------------
C NCLFORTSTART
      SUBROUTINE kstwo(data1,n1,data2,n2,d,prob,z)
      INTEGER n1,n2
      REAL d,prob,data1(n1),data2(n2),z
C NCLEND

CU    USES probks,sort
      INTEGER j1,j2
      REAL d1,d2,dt,en,en1,en2,fn1,fn2,probks
C DJS call sort(n1,data1)
C DJS call sort(n2,data2)
      en1=n1
      en2=n2
      j1=1
      j2=1
      fn1=0.
      fn2=0.
      d=0.
1     if(j1.le.n1.and.j2.le.n2)then
        d1=data1(j1)
        d2=data2(j2)
        if(d1.le.d2)then
          fn1=j1/en1
          j1=j1+1
        endif
        if(d2.le.d1)then
          fn2=j2/en2
          j2=j2+1
        endif
        dt=abs(fn2-fn1)
        if(dt.gt.d)d=dt
      goto 1
      endif

      en=sqrt(en1*en2/(en1+en2))
      z =d*en
      prob=probks((en+0.12+0.11/en)*d)
      return
      END

      FUNCTION probks(alam)
      REAL probks,alam,EPS1,EPS2
      PARAMETER (EPS1=0.001, EPS2=1.e-8)
      INTEGER j
      REAL a2,fac,term,termbf
      a2=-2.*alam**2
      fac=2.
      probks=0.
      termbf=0.
      do 11 j=1,100
        term=fac*exp(a2*j**2)
        probks=probks+term
        if(abs(term).le.EPS1*termbf.or.abs(term).le.EPS2*probks)return
        fac=-fac
        termbf=abs(term)
11    continue
      probks=1.
      return
      END

C	....................SSP...........................................
C
C	   SUBROUTINE KOLM2
C
C	   PURPOSE
C
C	      TESTS THE DIFFERENCE BETWEEN TWO SAMPLE DISTRIBUTION
C	      FUNCTIONS USING THE KOLMOGOROV-SMIRNOV TEST
C
C	   USAGE
C	      CALL KOLM2(X,Y,N,M,Z,PROB)
C
C	   DESCRIPTION OF PARAMETERS
C	      X    - INPUT VECTOR OF N INDEPENDENT OBSERVATIONS.  ON
C	             RETURN FROM KOLM2, X HAS BEEN SORTED INTO A
C	             MONOTONIC NON-DECREASING SEQUENCE.
C	      Y    - INPUT VECTOR OF M INDEPENDENT OBSERVATIONS.  ON
C	             RETURN FROM KOLM2, Y HAS BEEN SORTED INTO A
C	             MONOTONIC NON-DECREASING SEQUENCE.
C	      N    - NUMBER OF OBSERVATIONS IN X
C	      M    - NUMBER OF OBSERVATIONS IN Y
C	      Z    - OUTPUT VARIABLE CONTAINING THE GREATEST VALUE WITH
C	             RESPECT TO THE SPECTRUM OF X AND Y OF
C	             SQRT((M*N)/(M+N))*ABS(FN(X)-GM(Y)) WHERE
C	             FN(X) IS THE EMPIRICAL DISTRIBUTION FUNCTION OF THE
C	             SET (X) AND GM(Y) IS THE EMPIRICAL DISTRIBUTION
C	             FUNCTION OF THE SET (Y).
C	      PROB - OUTPUT VARIABLE CONTAINING THE PROBABILITY OF
C	             THE STATISTIC BEING GREATER THAN OR EQUAL TO Z IF
C	             THE HYPOTHESIS THAT X AND Y ARE FROM THE SAME PDF IS
C	             TRUE.  E.G., PROB= 0.05 IMPLIES THAT ONE CAN REJECT
C	             THE NULL HYPOTHESIS THAT THE SETS X AND Y ARE FROM
C	             THE SAME DENSITY WITH 5 PER CENT PROBABILITY OF BEING
C	             INCORRECT.  PROB = 1. - SMIRN(Z).
C
C	   REMARKS
C	      N AND M SHOULD BE GREATER THAN OR EQUAL TO 100.  (SEE THE
C	      MATHEMATICAL DESCRIPTION FOR THIS SUBROUTINE AND FOR THE
C	      SUBROUTINE SMIRN, CONCERNING ASYMPTOTIC FORMULAE).
C
C	      DOUBLE PRECISION USAGE---IT IS DOUBTFUL THAT THE USER WILL
C	      WISH TO PERFORM THIS TEST USING DOUBLE PRECISION ACCURACY.
C	      IF ONE WISHES TO COMMUNICATE WITH KOLM2 IN A DOUBLE
C	      PRECISION PROGRAM, HE SHOULD CALL THE FORTRAN SUPPLIED
C	      PROGRAM SNGL(X) PRIOR TO CALLING KOLM2, AND CALL THE
C	      FORTRAN SUPPLIED PROGRAM DBLE(X) AFTER EXITING FROM KOLM2.
C	      (NOTE THAT SUBROUTINE SMIRN DOES HAVE DOUBLE PRECISION
C	      CAPABILITY AS SUPPLIED BY THIS PACKAGE.)
C
C
C	   SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C	      SMIRN
C
C	   METHOD
C	      FOR REFERENCE, SEE (1) W. FELLER--ON THE KOLMOGOROV-SMIRNOV
C	      LIMIT THEOREMS FOR EMPIRICAL DISTRIBUTIONS--
C	      ANNALS OF MATH. STAT., 19, 1948.  177-189,
C	      (2) N. SMIRNOV--TABLE FOR ESTIMATING THE GOODNESS OF FIT
C	      OF EMPIRICAL DISTRIBUTIONS--ANNALS OF MATH. STAT., 19,
C	      1948.  279-281.
C	      (3) R. VON MISES--MATHEMATICAL THEORY OF PROBABILITY AND
C	      STATISTICS--ACADEMIC PRESS, NEW YORK, 1964.  490-493,
C	      (4) B.V. GNEDENKO--THE THEORY OF PROBABILITY--CHELSEA
C	      PUBLISHING COMPANY, NEW YORK, 1962.  384-401.
C
C	..................................................................
C
C C C   SUBOUTINE KOLM2(X,Y,N,M,Z,PROB)
C NCLFORTSTART
	SUBROUTINE KOLM2(X,Y,N,M,Z,PROB,D)
        INTEGER N,M
	REAL X(N),Y(M)
        REAL Z, PROB, D
C NCLEND

C
C	   SORT X INTO ASCENDING SEQUENCE
C
	DO 5 I=2,N
	IF(X(I)-X(I-1))1,5,5
1	TEMP=X(I)
	IM=I-1
	DO 3 J=1,IM
	L=I-J
	IF(TEMP-X(L))2,4,4
2	X(L+1)=X(L)
3	CONTINUE
	X(1)=TEMP
	GO TO 5
4	X(L+1)=TEMP
5	CONTINUE
C
C	   SORT Y INTO ASCENDING SEQUENCE
C
	DO 10 I=2,M
	IF(Y(I)-Y(I-1))6,10,10
6	TEMP=Y(I)
	IM=I-1
	DO 8  J=1,IM
	L=I-J
	IF(TEMP-Y(L))7,9,9
7	Y(L+1)=Y(L)
8	CONTINUE
	Y(1)=TEMP
	GO TO 10
9	Y(L+1)=TEMP
10	CONTINUE
C
C	   CALCULATE D = ABS(FN-GM) OVER THE SPECTRUM OF X AND Y
C
	XN=FLOAT(N)
	XN1=1./XN
	XM=FLOAT(M)
	XM1=1./XM
	D=0.0
	I=0
	J=0
	K=0
	L=0
11	IF(X(I+1)-Y(J+1))12,13,18
12	K=1
	GO TO 14
13	K=0
14	I=I+1
	IF(I-N)15,21,21
15	IF(X(I+1)-X(I))14,14,16
16	IF(K)17,18,17
C
C	   CHOOSE THE MAXIMUM DIFFERENCE, D
C
17	D=AMAX1(D,ABS(FLOAT(I)*XN1-FLOAT(J)*XM1))
	IF(L)22,11,22
18	J=J+1
	IF(J-M)19,20,20
19	IF(Y(J+1)-Y(J))18,18,17
20	L=1
	GO TO 17
21	L=1
	GO TO 16
C
C	   CALCULATE THE STATISTIC Z
C
22	Z=D*SQRT((XN*XM)/(XN+XM))
C
C	   CALCULATE THE PROBABILITY ASSOCIATED WITH Z
C
        CALL SMIRN(Z,PROB)
        PROB = 1.0-PROB
        
	RETURN
	END
C---
      SUBROUTINE SMIRN(X,Y)
      IF(X-.27)1,1,2
    1 Y=0.0
      GO TO 9
    2 IF(X-1.0)3,6,6
    3 Q1=EXP(-1.233701/X**2)
      Q2=Q1*Q1
      Q4=Q2*Q2
      Q8=Q4*Q4
      IF(Q8-1.0E-25)4,5,5
    4 Q8=0.0
    5 Y=(2.506628/X)*Q1*(1.0+Q8*(1.0+Q8*Q8))
      GO TO 9
    6 IF(X-3.1)8,7,7
    7 Y=1.0
      GO TO 9
    8 Q1=EXP(-2.0*X*X)
      Q2=Q1*Q1
      Q4=Q2*Q2
      Q8=Q4*Q4
      Y=1.0-2.0*(Q1-Q4+Q8*(Q1-Q8))
    9 RETURN
      END
 
C.......................................................................
C
C	   SUBROUTINE NDTR
C
C	   PURPOSE
C	      COMPUTES Y = P(X) = PROBABILITY THAT THE RANDOM VARIABLE  U,
C	      DISTRIBUTED NORMALLY(0,1), IS LESS THAN OR EQUAL TO X.
C	      F(X), THE ORDINATE OF THE NORMAL DENSITY AT X, IS ALSO
C	      COMPUTED.
C
C	   USAGE
C	      CALL NDTR(X,P,D)
C
C	   DESCRIPTION OF PARAMETERS
C	      X--INPUT SCALAR FOR WHICH P(X) IS COMPUTED.
C	      P--OUTPUT PROBABILITY.
C	      D--OUTPUT DENSITY.
C
C	   REMARKS
C	      MAXIMUM ERROR IS 0.0000007.
C
C	   SUBROUTINES AND SUBPROGRAMS REQUIRED
C	      NONE
C
C	   METHOD
C	      BASED ON APPROXIMATIONS IN C. HASTINGS, APPROXIMATIONS FOR
C	      DIGITAL COMPUTERS, PRINCETON UNIV. PRESS, PRINCETON, N.J.,
C	      1955.  SEE EQUATION 26.2.17, HANDBOOK OF MATHEMATICAL
C	      FUNCTIONS, ABRAMOWITZ AND STEGUN, DOVER PUBLICATIONS, INC.,
C	      NEW YORK.
C
C.......................................................................
C
	SUBROUTINE NDTR(X,P,D)
C
	AX=ABS(X)
	T=1.0/(1.0+.2316419*AX)
	D=0.3989423*EXP(-X*X/2.0)
	P = 1.0 - D*T*((((1.330274*T - 1.821256)*T + 1.781478)*T -
     1  0.3565638)*T + 0.3193815)
	IF(X)1,2,2
1	P=1.0-P
2	RETURN
	END
