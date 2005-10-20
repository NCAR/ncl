C NCLFORTSTART
      subroutine dcancorxy (nobs,mx,my,mxy,minxy,maxxy,lrdim,x,y      
     +                     ,ndf,canr,eval,wlam,chisq
     +                     ,coefxr,coefyl,rr,yx,rx,lrr,ier)
c
c NCL:   canr = cancor(x,y,opt)
c .             ndf, chisq, coefyl [1d], coefxr [1d] 
c .             coefyl and coefxr converted to 2D before return
c .             returned as attributes

      implicit none
c                                            INPUT
      integer              nobs,mx,my,mxy,minxy,maxxy,lrdim,lrr,ier
      double precision     x(nobs,mx) , y(nobs,my)
c                                            OUTPUT
      integer              ndf(mx)
      double precision     canr(mx)
      double precision     eval(mx), wlam(mx), chisq(mx)
      double precision     coefxr(lrdim), coefyl(lrdim)
c rr        - symmetric storage mode correlation matrix
c yx(nobs,m)- merged data matrix which contains the x and y variables
c rx        - 2D work array
      double precision     rr(lrr)
      double precision     yx(nobs,mxy)
      double precision     rx(mxy,mxy)
C NCLEND
c
c nobs   - total number of observations
c mx     - number of x variables
c my     - number of y variables
c mxy    - total number of variables [ mxy = mx+my ]
c minxy  - min( mx,my )
c maxxy  - max( mx,my )
c lrdim  - N*maxxy  [Normally, N=2]
c          The coefxr and coefyl arrays kept going out
c          of bounds in dcanorx. Arbitrarily made these larger.
c x      - independent variables  (nobs,mx)
c y      - dependent variables    (nobs,my)
c ndf    - number of dof
c eval   - eigenvalues
c wlam   - lambda ... used for debug
c chisq  - chisq numbers
c coefxr - x [A, RIGHT] cononical vectors
c coefyl - y [B, LEFT ] cononical vectors
c ier    - error code

c local
c
      integer              m, n, lrwork, icov
      integer              ncend, ncstrt, nc,ncs 
      double precision     xmsg
      logical              debug

      ier  = 0
      xmsg = 1.d20
c      lrr  = ((mxy+1)*mxy)/2 
      lrwork  = mxy*mxy 
      debug   = .false.

c      do m=1,minxy
      do m=1,mx
         ndf(m)   = 0
         canr(m)  = 0.0d0
         eval(m)  = 0.0d0
         wlam(m)  = 0.0d0
         chisq(m) = 0.0d0
      end do

      do m=1,lrdim
         coefyl(m) = 0.0d0
         coefxr(m) = 0.0d0
      end do

      if (ier.ne.0) return
c
c combine y and x  
c

      do m=1,my
        do n=1,nobs
           yx(n,m) = y(n,m)
        end do
      end do

      do m=1,mx
        do n=1,nobs
           yx(n,my+m) = x(n,m)
        end do
      end do
c
c use correlation matrix [icov=1]
c
      icov = 1
      if (icov.eq.1) then
          call dcrmssm(yx,nobs,mxy,nobs,mxy,xmsg,rr,lrr,ier)
      else
          call dvcmssm(yx,nobs,mxy,nobs,mxy,xmsg,rr,lrr,ier)
      end if

      if (debug) then
c                  print is lower triangular cuz it is easier
c                  Visually reverse output order foe upper triangular
          ncend=0
          print *, "cancor_xy: rr:"
          do nc=1,mxy
             ncstrt = ncend+1
             ncend  = ncstrt+nc-1
             write( * ,'(2x,i5,10(1x,f12.3),/, (7x,(10(1x,f12.3))))')
     *                   nc,(rr(n),n=ncstrt,ncend)
          end do
      end if

      call dcanorx (nobs,my,mx,rr,eval,wlam,canr,chisq,ndf
     +             ,coefxr,coefyl,rx,lrdim,lrr,lrwork)

      return
      end
c ------------------------
C     ..................................................................CANO  20
C http://pdp-10.trailing-edge.com/decuslib10-02/01/43,50145/canor.ssp
C     ..................................................................CANO  20
C                                                                       CANO  30
C        SUBROUTINE CANOR                                               CANO  40
C                                                                       CANO  50
C        PURPOSE                                                        CANO  60
C           COMPUTE THE CANONICAL CORRELATIONS BETWEEN TWO SETS OF      CANO  70
C           VARIABLES.  CANOR IS NORMALLY PRECEDED BY A CALL TO SUBROU- CANO  80
C           TINE CORRE.                                                 CANO  90
C                                                                       CANO 100
C        USAGE                                                          CANO 110
C           CALL CANOR (N,MP,MQ,RR,ROOTS,WLAM,CANR,CHISQ,NDF,COEFR,     CANO 120
C                       COEFL,R)                                        CANO 130
C                                                                       CANO 140
C        DESCRIPTION OF PARAMETERS                                      CANO 150
C           N     - NUMBER OF OBSERVATIONS                              CANO 160
C           MP    - NUMBER OF LEFT HAND VARIABLES                       CANO 170
c                 ? left refers to independent variables [x]
C           MQ    - NUMBER OF RIGHT HAND VARIABLES                      CANO 180
c                 ? right refers to dependent variables  [y]
C           RR    - INPUT MATRIX (ONLY UPPER TRIANGULAR PORTION OF THE  CANO 190
C                   SYMMETRIC MATRIX OF M X M, WHERE M = MP + MQ)       CANO 200
C                   CONTAINING CORRELATION COEFFICIENTS.  (STORAGE MODE CANO 210
C                   OF 1)                                               CANO 220
C           ROOTS - OUTPUT VECTOR OF LENGTH MQ CONTAINING EIGENVALUES   CANO 230
C                   COMPUTED IN THE NROOT SUBROUTINE.                   CANO 240
C           WLAM  - OUTPUT VECTOR OF LENGTH MQ CONTAINING LAMBDA.       CANO 250
C           CANR  - OUTPUT VECTOR OF LENGTH MQ CONTAINING CANONICAL     CANO 260
C                   CORRELATIONS.                                       CANO 270
C           CHISQ - OUTPUT VECTOR OF LENGTH MQ CONTAINING THE           CANO 280
C                   VALUES OF CHI-SQUARES.                              CANO 290
C           NDF   - OUTPUT VECTOR OF LENGTH MQ CONTAINING THE DEGREES   CANO 300
C                   OF FREEDOM ASSOCIATED WITH CHI-SQUARES.             CANO 310
C           COEFR - OUTPUT MATRIX (MQ X MQ) CONTAINING MQ SETS OF       CANO 320
C                   RIGHT HAND COEFFICIENTS COLUMNWISE.                 CANO 330
C           COEFL - OUTPUT MATRIX (MP X MQ) CONTAINING MQ SETS OF       CANO 340
C                   LEFT HAND COEFFICIENTS COLUMNWISE.                  CANO 350
C           R     - WORK MATRIX (M X M)                                 CANO 360
c
c       remark-djs
c       Using the recommended sizes resulted in bounds errors.
c       lrdim is an (arbitrarily) large number [say, 2*mp*mq] 
C                                                                       CANO 370
C        REMARKS                                                        CANO 380
C           THE NUMBER OF LEFT HAND VARIABLES (MP) SHOULD BE GREATER    CANO 390
C           THAN OR EQUAL TO THE NUMBER OF RIGHT HAND VARIABLES (MQ).   CANO 400
c              The above is why I *think* MP=MX  and MQ=MY
C           THE VALUES OF CANONICAL CORRELATION, LAMBDA, CHI-SQUARE,    CANO 410
C           DEGREES OF FREEDOM, AND CANONICAL COEFFICIENTS ARE COMPUTED CANO 420
C           ONLY FOR THOSE EIGENVALUES IN ROOTS WHICH ARE GREATER THAN  CANO 430
C           ZERO.                                                       CANO 440
C                                                                       CANO 450
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  CANO 460
C           MINV                                                        CANO 470
C           NROOT  (WHICH, IN TURN, CALLS THE SUBROUTINE EIGEN.)        CANO 480
C                                                                       CANO 490
C        METHOD                                                         CANO 500
C           REFER TO W. W. COOLEY AND P. R. LOHNES, 'MULTIVARIATE PRO-  CANO 510
C           CEDURES FOR THE BEHAVIORAL SCIENCES', JOHN WILEY AND SONS,  CANO 520
C           1962, CHAPTER 3.                                            CANO 530
C                                                                       CANO 540
c     added LCRX,LCLY,LRR,LRWORK for bounds checking
C     ..................................................................CANO 550
C                                                                       CANO 560
      SUBROUTINE DCANORX (N,MP,MQ,RR,ROOTS,WLAM,CANR,CHISQ,NDF,COEFR    CANO 570
     1                   ,COEFL,R,LRDIM,LRR,LRWORK)
      IMPLICIT NONE
      INTEGER          N, MP, MQ, NDF(MQ), LRDIM, LRR, LRWORK
      DOUBLE PRECISION RR(LRR),COEFL(LRDIM),COEFR(LRDIM),R(LRWORK)  
      DOUBLE PRECISION ROOTS(MQ),WLAM(MQ),CANR(MQ),CHISQ(MQ)
C                                                                       CANO 610
C        ...............................................................CANO 620
C                                                                       CANO 630
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  CANO 640
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      CANO 650
C        STATEMENT WHICH FOLLOWS.                                       CANO 660
C                                                                       CANO 670
C     DOUBLE PRECISION RR,ROOTS,WLAM,CANR,CHISQ,COEFR,COEFL,R,DET,SUM   CANO 680
C                                                                       CANO 690
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    CANO 700
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      CANO 710
C        ROUTINE.                                                       CANO 720
C                                                                       CANO 730
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      CANO 740
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENT CANO 750
C        165 MUST BE CHANGED TO DSQRT.  ALOG IN STATEMENT 175 MUST BE   CANO 760
C        CHANGED TO DLOG.                                               CANO 770
C                                                                       CANO 780
C        ...............................................................CANO 790
C                                                                       CANO 800
C     PARTITION INTERCORRELATIONS AMONG LEFT HAND VARIABLES, BETWEEN    CANO 810
C     LEFT AND RIGHT HAND VARIABLES, AND AMONG RIGHT HAND VARIABLES.    CANO 820
C                                                                       CANO 830
      INTEGER          M, N1, N2, N3, I, J, K, L, JJ 
      DOUBLE PRECISION FN, FMP, FMQ, SUMX, DET

      M=MP+MQ                                                           CANO 840
      N1=0                                                              CANO 850
      DO 105 I=1,M                                                      CANO 860
      DO 105 J=1,M                                                      CANO 870
      IF(I-J) 102, 103, 103                                             CANO 880
  102 L=I+(J*J-J)/2                                                     CANO 890
      GO TO 104                                                         CANO 900
  103 L=J+(I*I-I)/2                                                     CANO 910
  104 N1=N1+1                                                           CANO 920
  105 R(N1)=RR(L)                                                       CANO 930
c c c print *, "debug 105: N1=",N1," L=",L

      L=MP                                                              CANO 940
      DO 108 J=2,MP                                                     CANO 950
      N1=M*(J-1)                                                        CANO 960
      DO 108 I=1,MP                                                     CANO 970
      L=L+1                                                             CANO 980
      N1=N1+1                                                           CANO 990
  108 R(L)=R(N1)                                                        CANO1000
c c c print *, "debug 108: N1=",N1," L=",L

      N2=MP+1                                                           CANO1010
      L=0                                                               CANO1020
      DO 110 J=N2,M                                                     CANO1030
      N1=M*(J-1)                                                        CANO1040
      DO 110 I=1,MP                                                     CANO1050
      L=L+1                                                             CANO1060
      N1=N1+1                                                           CANO1070
c c c if (L.gt.lrdim .or. N1.gt.lrwork) then
c c c     print *,"debug: 110 bounds: L=",L,"  N1=",N1
c c c end if
  110 COEFL(L)=R(N1)                                                    CANO1080

c c c print *, "debug 110: COEFL: L=",L,"    <========"
      L=0                                                               CANO1090
      DO 120 J=N2,M                                                     CANO1100
      N1=M*(J-1)+MP                                                     CANO1110
      DO 120 I=N2,M                                                     CANO1120
      L=L+1                                                             CANO1130
      N1=N1+1                                                           CANO1140
c c c if (L.gt.lrdim .or. N1.gt.lrwork) then
c c c     print *,"debug: 120 bounds: L=",L,"  N1=",N1
c c c end if
  120 COEFR(L)=R(N1)                                                    CANO1150

c c c print *, "debug 120: COEFR: L=",L,"    <========"
C                                                                       CANO1160
C     SOLVE THE CANONICAL EQUATION                                      CANO1170
C                                                                       CANO1180
      L=MP*MP+1                                                         CANO1190
      K=L+MP                                                            CANO1200
c c c print *, "SOLVE THE CANONICAL EQUATION: L,K=",L,K,"    <========"
      CALL DMINVX (R,MP,DET,R(L),R(K))                                  CANO1210
C                                                                       CANO1220
C        CALCULATE T = INVERSE OF R11 * R12                             CANO1230
C                                                                       CANO1240
      DO 140 I=1,MP                                                     CANO1250
      N2=0                                                              CANO1260
      DO 130 J=1,MQ                                                     CANO1270
      N1=I-MP                                                           CANO1280
      ROOTS(J)=0.0D0                                                    CANO1290
      DO 130 K=1,MP                                                     CANO1300
      N1=N1+MP                                                          CANO1310
      N2=N2+1                                                           CANO1320
  130 ROOTS(J)=ROOTS(J)+R(N1)*COEFL(N2)                                 CANO1330
      L=I-MP                                                            CANO1340
      DO 140 J=1,MQ                                                     CANO1350
      L=L+MP                                                            CANO1360
  140 R(L)=ROOTS(J)                                                     CANO1370
C                                                                       CANO1380
C        CALCULATE A = R21 * T                                          CANO1390
C                                                                       CANO1400
      L=MP*MQ                                                           CANO1410
      N3=L+1                                                            CANO1420
      DO 160 J=1,MQ                                                     CANO1430
      N1=0                                                              CANO1440
      DO 160 I=1,MQ                                                     CANO1450
      N2=MP*(J-1)                                                       CANO1460
      SUMX=0.0D0                                                        CANO1470
      DO 150 K=1,MP                                                     CANO1480
      N1=N1+1                                                           CANO1490
      N2=N2+1                                                           CANO1500
  150 SUMX=SUMX+COEFL(N1)*R(N2)                                         CANO1510
      L=L+1                                                             CANO1520
  160 R(L)=SUMX                                                         CANO1530
C                                                                       CANO1540
C        CALCULATE EIGENVALUES WITH ASSOCIATED EIGENVECTORS OF THE      CANO1550
C        INVERSE OF R22 * A                                             CANO1560
C                                                                       CANO1570
      L=L+1                                                             CANO1580
      CALL DNROOTX (MQ,R(N3),COEFR,ROOTS,R(L))                          CANO1590
C                                                                       CANO1600
C     FOR EACH VALUE OF I = 1, 2, ..., MQ, CALCULATE THE FOLLOWING      CANO1610
C     STATISTICS                                                        CANO1620
C                                                                       CANO1630
      DO 210 I=1,MQ                                                     CANO1640
C                                                                       CANO1650
C        TEST WHETHER EIGENVALUE IS GREATER THAN ZERO                   CANO1660
C                                                                       CANO1670
      IF(ROOTS(I)) 220, 220, 165                                        CANO1680
C                                                                       CANO1690
C        CANONICAL CORRELATION                                          CANO1700
C                                                                       CANO1710
  165 CANR(I)= SQRT(ROOTS(I))                                           CANO1720
C                                                                       CANO1730
C        CHI-SQUARE                                                     CANO1740
C                                                                       CANO1750
      WLAM(I)=1.0D0                                                     CANO1760
      DO 170 J=I,MQ                                                     CANO1770
  170 WLAM(I)=WLAM(I)*(1.0D0-ROOTS(J))                                  CANO1780
      FN=N                                                              CANO1790
      FMP=MP                                                            CANO1800
      FMQ=MQ                                                            CANO1810
  175 CHISQ(I)=-(FN-0.5D0*(FMP+FMQ+1.0D0))*LOG(WLAM(I))                 CANO1820
C                                                                       CANO1830
C        DEGREES OF FREEDOM FOR CHI-SQUARE                              CANO1840
C                                                                       CANO1850
      N1=I-1                                                            CANO1860
      NDF(I)=(MP-N1)*(MQ-N1)                                            CANO1870
C                                                                       CANO1880
C        I-TH SET OF RIGHT HAND COEFFICIENTS                            CANO1890
C                                                                       CANO1900
      N1=MQ*(I-1)                                                       CANO1910
      N2=MQ*(I-1)+L-1                                                   CANO1920
      DO 180 J=1,MQ                                                     CANO1930
      N1=N1+1                                                           CANO1940
      N2=N2+1                                                           CANO1950
  180 COEFR(N1)=R(N2)                                                   CANO1960
C                                                                       CANO1970
C        I-TH SET OF LEFT HAND COEFFICIENTS                             CANO1980
C                                                                       CANO1990
      DO 200 J=1,MP                                                     CANO2000
      N1=J-MP                                                           CANO2010
      N2=MQ*(I-1)                                                       CANO2020
      K=MP*(I-1)+J                                                      CANO2030
      COEFL(K)=0.0D0                                                    CANO2040
      DO 190 JJ=1,MQ                                                    CANO2050
      N1=N1+MP                                                          CANO2060
      N2=N2+1                                                           CANO2070
  190 COEFL(K)=COEFL(K)+R(N1)*COEFR(N2)                                 CANO2080
  200 COEFL(K)=COEFL(K)/CANR(I)                                         CANO2090
  210 CONTINUE                                                          CANO2100
  220 RETURN                                                            CANO2110
      END                                                               CANO2120
C**************************************************************************
C
C     SUBROUTINE MINV
C
C     PURPOSE
C        INVERT A MATRIX
C
C     USAGE
C        CALL MINV(A,N,D,L,M)
C
C     DESCRIPTION OF PARAMETERS
C        A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
C             RESULTANT INVERSE.
C        N - ORDER OF MATRIX A
C        D - RESULTANT DETERMINANT
C        L - WORK VECTOR OF LENGTH N
C        M - WORK VECTOR OF LENGTH N
C
C     REMARKS
C        MATRIX AX MUST BE A GENERAL MATRIX
C
C     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C        NONE
C
C     METHOD
C        THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT
C        IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
C        THE MATRIX IS SINGULAR.
C
C **********************************************************************
C
      SUBROUTINE DMINVX(A,N,D,L,M)
      IMPLICIT  NONE
      INTEGER   N
      DOUBLE PRECISION   A(*), L(*), M(*) 
      DOUBLE PRECISION   BIGA, D, HOLD

      INTEGER   NK, I, J, K, IJ, IZ, KK, KI, JP, JK, JI, JQ, JR, IK, KJ
C
C **********************************************************************
C
C
C        SEARCH FOR LARGEST ELEMENT
C
      D=1.D0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
10    IF (ABS(BIGA)-ABS(A(IJ))) 15,20,20
15    BIGA=A(IJ)
      L(K)=I
      M(K)=J
20    CONTINUE
C
C        INTERCHANGE ROWS
C
      J=L(K)
      IF(J-K) 35,35,25
25    KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
30    A(JI)=HOLD
C
C        INTERCHANGE COLUMNS
C
35    I=M(K)
      IF(I-K) 45,45,38
38    JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
40    A(JI)=HOLD
C
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
C        CONTAINED IN BIGA)
C
45    IF(BIGA) 48,46,48
46    D=0.D0
      RETURN
48    DO 55 I=1,N
      IF (I-K) 50,55,50
50    IK=NK+I
      A(IK)=A(IK)/(-BIGA)
55    CONTINUE
C
C        REDUCE MATRIX
C
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K) 60,65,60
60    IF(J-K) 62,65,62
62    KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
65    CONTINUE
C
C        DIVIDE ROW BY PIVOT
C
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K) 70,75,70
70    A(KJ)=A(KJ)/BIGA
75    CONTINUE
C
C        PRODUCT OF PIVOTS
C
      D=D*BIGA
C
C        REPLACE PIVOT BY RECIPROCAL
C
      A(KK)=1.D0/BIGA
80    CONTINUE
C
C        FINAL ROW AND COLUMN INTERCHANGE
C
      K=N
100   K=(K-1)
      IF(K) 150,150,105
105   I=L(K)
      IF(I-K) 120,120,108
108   JQ=N*(K-1)
      JR=N*(I-1)
      DO 110 J=1,N
      JK=JQ+J
      HOLD=A(JK)
      JI=JR+J
      A(JK)=-A(JI)
110   A(JI)=HOLD
120   J=M(K)
      IF(J-K) 100,100,125
125   KI=K-N
      DO 130 I=1,N
      KI=KI+N
      HOLD=A(KI)
      JI=KI-K+J
      A(KI)=-A(JI)
130   A(JI)=HOLD
      GO TO 100
150   RETURN
      END
C     ..................................................................NROO  20
C                                                                       NROO  30
C        SUBROUTINE NROOT                                               NROO  40
C                   DNROOTX   [djs]
C                                                                       NROO  50
C        PURPOSE                                                        NROO  60
C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL NONSYMMETRIC NROO  70
C           MATRIX OF THE FORM B-INVERSE TIMES A.  THIS SUBROUTINE IS   NROO  80
C           NORMALLY CALLED BY SUBROUTINE CANOR IN PERFORMING A         NROO  90
C           CANONICAL CORRELATION ANALYSIS.                             NROO 100
C                                                                       NROO 110
C        USAGE                                                          NROO 120
C           CALL NROOT (M,A,B,XL,X)                                     NROO 130
C           CALL DNROOTX (M,A,B,XL,X) 
C                                                                       NROO 140
C        DESCRIPTION OF PARAMETERS                                      NROO 150
C           M  - ORDER OF SQUARE MATRICES A, B, AND X.                  NROO 160
C           A  - INPUT MATRIX (M X M).                                  NROO 170
C           B  - INPUT MATRIX (M X M).                                  NROO 180
C           XL - OUTPUT VECTOR OF LENGTH M CONTAINING EIGENVALUES OF    NROO 190
C                B-INVERSE TIMES A.                                     NROO 200
C           X  - OUTPUT MATRIX (M X M) CONTAINING EIGENVECTORS COLUMN-  NROO 210
C                WISE.                                                  NROO 220
C                                                                       NROO 230
C        REMARKS                                                        NROO 240
C           NONE                                                        NROO 250
C                                                                       NROO 260
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  NROO 270
C           EIGEN                                                       NROO 280
C                                                                       NROO 290
C        METHOD                                                         NROO 300
C           REFER TO W. W. COOLEY AND P. R. LOHNES, 'MULTIVARIATE PRO-  NROO 310
C           CEDURES FOR THE BEHAVIORAL SCIENCES', JOHN WILEY AND SONS,  NROO 320
C           1962, CHAPTER 3.                                            NROO 330
C                                                                       NROO 340
C     ..................................................................NROO 350
C                                                                       NROO 360
      SUBROUTINE DNROOTX (M,A,B,XL,X)                                   NROO 370
      IMPLICIT  NONE
      INTEGER   M
      DOUBLE PRECISION XL(M)                                            NROO 380
      DOUBLE PRECISION A(*),B(*),X(*)                                   NROO 380
C                                                                       NROO 390
C        ...............................................................NROO 400
C                                                                       NROO 410
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE  NROO 420
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION      NROO 430
C        STATEMENT WHICH FOLLOWS.                                       NROO 440
C                                                                       NROO 450
C     DOUBLE PRECISION A,B,XL,X,SUMV                                    NROO 460
C                                                                       NROO 470
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS    NROO 480
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS      NROO 490
C        ROUTINE.                                                       NROO 500
C                                                                       NROO 510
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO      NROO 520
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTSNROO 530
C        110 AND 175 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT 110    NROO 540
C        MUST BE CHANGED TO DABS.                                       NROO 550
C                                                                       NROO 560
C        ...............................................................NROO 570
      INTEGER I, J, K, L, MV, N1, N2
      DOUBLE PRECISION SUMV
C                                                                       NROO 580
C     COMPUTE EIGENVALUES AND EIGENVECTORS OF B                         NROO 590
C                                                                       NROO 600
      K=1                                                               NROO 610
      DO 100 J=2,M                                                      NROO 620
      L=M*(J-1)                                                         NROO 630
      DO 100 I=1,J                                                      NROO 640
      L=L+1                                                             NROO 650
      K=K+1                                                             NROO 660
  100 B(K)=B(L)                                                         NROO 670
C                                                                       NROO 680
C        THE MATRIX B IS A REAL SYMMETRIC MATRIX.                       NROO 690
C                                                                       NROO 700
      MV=0                                                              NROO 710
      CALL DEIGENX (B,X,M,MV)                                           NROO 720
C                                                                       NROO 730
C     FORM RECIPROCALS OF SQUARE ROOT OF EIGENVALUES.  THE RESULTS      NROO 740
C     ARE PREMULTIPLIED BY THE ASSOCIATED EIGENVECTORS.                 NROO 750
C                                                                       NROO 760
      L=0                                                               NROO 770
      DO 110 J=1,M                                                      NROO 780
      L=L+J                                                             NROO 790
  110 XL(J)=1.0D0/SQRT( ABS(B(L)))                                      NROO 800
      K=0                                                               NROO 810
      DO 115 J=1,M                                                      NROO 820
      DO 115 I=1,M                                                      NROO 830
      K=K+1                                                             NROO 840
  115 B(K)=X(K)*XL(J)                                                   NROO 850
C                                                                       NROO 860
C     FORM (B**(-1/2))PRIME * A * (B**(-1/2))                           NROO 870
C                                                                       NROO 880
      DO 120 I=1,M                                                      NROO 890
      N2=0                                                              NROO 900
      DO 120 J=1,M                                                      NROO 910
      N1=M*(I-1)                                                        NROO 920
      L=M*(J-1)+I                                                       NROO 930
      X(L)=0.0D0                                                        NROO 940
      DO 120 K=1,M                                                      NROO 950
      N1=N1+1                                                           NROO 960
      N2=N2+1                                                           NROO 970
  120 X(L)=X(L)+B(N1)*A(N2)                                             NROO 980
      L=0                                                               NROO 990
      DO 130 J=1,M                                                      NROO1000
      DO 130 I=1,J                                                      NROO1010
      N1=I-M                                                            NROO1020
      N2=M*(J-1)                                                        NROO1030
      L=L+1                                                             NROO1040
      A(L)=0.0D0                                                        NROO1050
      DO 130 K=1,M                                                      NROO1060
      N1=N1+M                                                           NROO1070
      N2=N2+1                                                           NROO1080
  130 A(L)=A(L)+X(N1)*B(N2)                                             NROO1090
C                                                                       NROO1100
C     COMPUTE EIGENVALUES AND EIGENVECTORS OF A                         NROO1110
C                                                                       NROO1120
      CALL DEIGENX (A,X,M,MV)                                           NROO1130
      L=0                                                               NROO1140
      DO 140 I=1,M                                                      NROO1150
      L=L+I                                                             NROO1160
  140 XL(I)=A(L)                                                        NROO1170
C                                                                       NROO1180
C     COMPUTE THE NORMALIZED EIGENVECTORS                               NROO1190
C                                                                       NROO1200
      DO 150 I=1,M                                                      NROO1210
      N2=0                                                              NROO1220
      DO 150 J=1,M                                                      NROO1230
      N1=I-M                                                            NROO1240
      L=M*(J-1)+I                                                       NROO1250
      A(L)=0.0D0                                                        NROO1260
      DO 150 K=1,M                                                      NROO1270
      N1=N1+M                                                           NROO1280
      N2=N2+1                                                           NROO1290
  150 A(L)=A(L)+B(N1)*X(N2)                                             NROO1300
      L=0                                                               NROO1310
      K=0                                                               NROO1320
      DO 180 J=1,M                                                      NROO1330
      SUMV=0.0D0                                                        NROO1340
      DO 170 I=1,M                                                      NROO1350
      L=L+1                                                             NROO1360
  170 SUMV=SUMV+A(L)*A(L)                                               NROO1370
  175 SUMV= SQRT(SUMV)                                                  NROO1380
      DO 180 I=1,M                                                      NROO1390
      K=K+1                                                             NROO1400
  180 X(K)=A(K)/SUMV                                                    NROO1410
      RETURN                                                            NROO1420
      END                                                               NROO1430
C SUBROUTINE 'EIGEN' FROM THE IBM SCIENTIFIC SUBROUTINE PACKAGE.
C
C NOTE:  TO CONFORM WITH THE FORTRAN77 STANDARD, DUMMY ARRAY DIMENSIONS
C        (1) HAVE BEEN CHANGED TO (*).
C
C     ..................................................................
C
C        SUBROUTINE  EIGEN 
C                   DEIGENX   [djs]
C
C        PURPOSE
C           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
C           MATRIX
C
C        USAGE
C           CALL EIGEN(A,R,N,MV)
C
C        DESCRIPTION OF PARAMETERS
C           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
C               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
C               MATRIX A IN DESCENDING ORDER.
C           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
C               IN SAME SEQUENCE AS EIGENVALUES)
C           N - ORDER OF MATRICES A AND R
C           MV- INPUT CODE
C                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
C                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
C                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
C                       SEQUENCE)
C
C        REMARKS
C           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
C           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
C           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
C           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
C           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
C
C     ..................................................................
C
      SUBROUTINE DEIGENX(A,R,N,MV)
      IMPLICIT NONE
      INTEGER  N, MV
      DOUBLE PRECISION A(*),R(*)
C
C        ...............................................................
C
C        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
C        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
C        STATEMENT WHICH FOLLOWS.
C
C     DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
C    1                 COSX2,SINCS,RANGE
C
C        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
C        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
C        ROUTINE.
C
C        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
C        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
C        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
C        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
C        BE CHANGED TO 1.0D-12.
C
C        ...............................................................
      INTEGER I, J, IQ, IJ, IND, MQ, LQ, ILO, IMQ, ILR, IMR,
     +        IA, L, M, LM, LL, MM, ILQ, IM, IL, JQ, K 
      DOUBLE PRECISION RANG, ANORM, ANRMX, THR, X, Y,
     +                 SINX, SINX2, COSX, COSX2, SINCS
C
C        GENERATE IDENTITY MATRIX
C
    5 RANG=1.0D-12
      IF(MV-1) 10,25,10
   10 IQ=-N
      DO 20 J=1,N
      IQ=IQ+N
      DO 20 I=1,N
      IJ=IQ+I
      R(IJ)=0.0
      IF(I-J) 20,15,20
   15 R(IJ)=1.0D0
   20 CONTINUE
C
C        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
C
   25 ANORM=0.0D0
      DO 35 I=1,N
      DO 35 J=I,N
      IF(I-J) 30,35,30
   30 IA=I+(J*J-J)/2
      ANORM=ANORM+A(IA)*A(IA)
   35 CONTINUE
      IF(ANORM) 165,165,40
   40 ANORM=1.414D0*SQRT(ANORM)
      ANRMX=ANORM*RANG/N
C
C        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
C
      IND=0
      THR=ANORM
   45 THR=THR/N
   50 L=1
   55 M=L+1
C
C        COMPUTE SIN AND COS
C
   60 MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 IF( ABS(A(LM))-THR) 130,65,65
   65 IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5D0*(A(LL)-A(MM))
   68 Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
      IF(X) 70,75,75
   70 Y=-Y
   75 SINX=Y/ SQRT(2.0D0*(1.0D0+( SQRT(1.0D0-Y*Y))))
      SINX2=SINX*SINX
   78 COSX= SQRT(1.0D0-SINX2)
      COSX2=COSX*COSX
      SINCS =SINX*COSX
C
C        ROTATE L AND M COLUMNS
C
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
      IQ=(I*I-I)/2
      IF(I-L) 80,115,80
   80 IF(I-M) 85,115,90
   85 IM=I+MQ
      GO TO 95
   90 IM=M+IQ
   95 IF(I-L) 100,105,105
  100 IL=I+LQ
      GO TO 110
  105 IL=L+IQ
  110 X=A(IL)*COSX-A(IM)*SINX
      A(IM)=A(IL)*SINX+A(IM)*COSX
      A(IL)=X
  115 IF(MV-1) 120,125,120
  120 ILR=ILQ+I
      IMR=IMQ+I
      X=R(ILR)*COSX-R(IMR)*SINX
      R(IMR)=R(ILR)*SINX+R(IMR)*COSX
      R(ILR)=X
  125 CONTINUE
      X=2.0D0*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
C
C        TESTS FOR COMPLETION
C
C        TEST FOR M = LAST COLUMN
C
  130 IF(M-N) 135,140,135
  135 M=M+1
      GO TO 60
C
C        TEST FOR L = SECOND FROM LAST COLUMN
C
  140 IF(L-(N-1)) 145,150,145
  145 L=L+1
      GO TO 55
  150 IF(IND-1) 160,155,160
  155 IND=0
      GO TO 50
C
C        COMPARE THRESHOLD WITH FINAL NORM
C
  160 IF(THR-ANRMX) 165,165,45
C
C        SORT EIGENVALUES AND EIGENVECTORS
C
  165 IQ=-N
      DO 185 I=1,N
      IQ=IQ+N
      LL=I+(I*I-I)/2
      JQ=N*(I-2)
      DO 185 J=I,N
      JQ=JQ+N
      MM=J+(J*J-J)/2
      IF(A(LL)-A(MM)) 170,185,185
  170 X=A(LL)
      A(LL)=A(MM)
      A(MM)=X
      IF(MV-1) 175,185,175
  175 DO 180 K=1,N
      ILR=IQ+K
      IMR=JQ+K
      X=R(ILR)
      R(ILR)=R(IMR)
  180 R(IMR)=X
  185 CONTINUE
      RETURN
      END
