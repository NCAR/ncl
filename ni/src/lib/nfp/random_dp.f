      DOUBLE PRECISION FUNCTION DGENCHI(DF)
C**********************************************************************
C
C       DOUBLE PRECISION FUNCTION DGENCHI( DF )
C                Generate random value of CHIsquare variable
C
C
C                              Function
C
C
C     Generates random deviate from the distribution of a chisquare
C     with DF degrees of freedom random variable.
C
C
C                              Arguments
C
C
C     DF --> Degrees of freedom of the chisquare
C            (Must be positive)
C                    DOUBLE PRECISION DF
C
C
C                              Method
C
C
C     Uses relation between chisquare and gamma.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION DF
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DGENGAM
      EXTERNAL DGENGAM
C     ..
C     .. Executable Statements ..
      IF (.NOT. (DF.LE.0.0D0)) GO TO 10
      WRITE (*,FMT=*) 'DF <= 0 in DGENCHI - ABORT'
      WRITE (*,FMT=*) 'Value of DF: ',DF
      STOP 'DF <= 0 in DGENCHI - ABORT'

   10 DGENCHI = 2.0D0*DGENGAM(1.0D0,DF/2.0D0)
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DGENGAM(A,R)
C**********************************************************************
C
C        DOUBLE PRECISION FUNCTION DGENGAM( A, R )
C           GENerates random deviates from GAMma distribution
C
C
C                              Function
C
C
C     Generates random deviates from the gamma distribution whose
C     density is
C          (A**R)/Gamma(R) * X**(R-1) * Exp(-A*X)
C
C
C                              Arguments
C
C
C     A --> Location parameter of Gamma distribution
C                         DOUBLE PRECISION A
C
C     R --> Shape parameter of Gamma distribution
C                         DOUBLE PRECISION R
C
C
C                              Method
C
C
C     Renames DSGAMMA from TOMS as slightly modified by BWB to use
C     DRANFD instead of SUNIF.
C
C     For details see:
C               (Case R >= 1.0)
C               Ahrens, J.H. and Dieter, U.
C               Generating Gamma Variates by a
C               Modified Rejection Technique.
C               Comm. ACM, 25,1 (Jan. 1982), 47 - 54.
C     Algorithm GD
C
C               (Case 0.0 <= R <= 1.0)
C               Ahrens, J.H. and Dieter, U.
C               Computer Methods for Sampling from Gamma,
C               Beta, Poisson and Binomial Distributions.
C               Computing, 12 (1974), 223-246/
C     Adapted algorithm GS.
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION A,R
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DSGAMMA
      EXTERNAL DSGAMMA
C     ..
C     .. Executable Statements ..
      DGENGAM = DSGAMMA(R)
      DGENGAM = DGENGAM/A
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DGENNOR(AV,SD)
C**********************************************************************
C
C        DOUBLE PRECISION FUNCTION DGENNOR( AV, SD )
C
C         GENerate random deviate from a NORmal distribution
C
C
C                              Function
C
C
C     Generates a single random deviate from a normal distribution
C     with mean, AV, and standard deviation, SD.
C
C
C                              Arguments
C
C
C     AV --> Mean of the normal distribution.
C                         DOUBLE PRECISION AV
C
C     SD --> Standard deviation of the normal distribution.
C                         DOUBLE PRECISION SD
C
C     DGENNOR <-- Generated normal deviate.
C                         DOUBLE PRECISION DGENNOR
C
C
C                              Method
C
C
C     Renames DSNORM from TOMS as slightly modified by BWB to use
C     DRANFD instead of SUNIF.
C
C     For details see:
C               Ahrens, J.H. and Dieter, U.
C               Extensions of Forsythe's Method for Random
C               Sampling from the Normal Distribution.
C               Math. Comput., 27,124 (Oct. 1973), 927 - 937.
C
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION AV,SD
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DSNORM
      EXTERNAL DSNORM
C     ..
C     .. Executable Statements ..
      DGENNOR = SD*DSNORM() + AV
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DGENUNF(LOW,HIGH)
C**********************************************************************
C
C         DOUBLE PRECISION FUNCTION DGENUNF( LOW, HIGH )
C
C               GeNerate Uniform Real between LOW and HIGH
C
C
C                              Function
C
C
C     Generates a real uniformly distributed between LOW and HIGH.
C
C
C                              Arguments
C
C
C     LOW --> Low bound (exclusive) on real value to be generated
C                    DOUBLE PRECISION LOW
C
C     HIGH --> High bound (exclusive) on real value to be generated
C                    DOUBLE PRECISION HIGH
C
C**********************************************************************
C     .. Scalar Arguments ..
      DOUBLE PRECISION HIGH,LOW
C     ..
C     .. External Functions ..
      DOUBLE PRECISION DRANFD
      EXTERNAL DRANFD
C     ..
C     .. Executable Statements ..
      IF (.NOT. (LOW.GT.HIGH)) GO TO 10
      WRITE (*,FMT=*) 'LOW > HIGH in DGENUNF: LOW ',LOW,' HIGH: ',HIGH
      WRITE (*,FMT=*) 'Abort'
      STOP 'LOW > High in DGENUNF - Abort'

   10 DGENUNF = LOW + (HIGH-LOW)*DRANFD()

      RETURN

      END
c -------------------------------------------------
      SUBROUTINE DGETCGN(G)
      INTEGER G
C**********************************************************************
C
C      SUBROUTINE DGETCGN(G)
C                         Get GeNerator
C
C     Returns in G the number of the current random number generator
C
C
C                              Arguments
C
C
C     G <-- Number of the current random number generator (1..32)
C                    INTEGER G
C
C**********************************************************************
C
      INTEGER CURNTG,NUMG
      SAVE CURNTG
      PARAMETER (NUMG=32)
      DATA CURNTG/1/
C
      G = CURNTG
      RETURN

      ENTRY SETCGN(G)
C**********************************************************************
C
C     SUBROUTINE SETCGN( G )
C                      Set GeNerator
C
C     Sets  the  current  generator to G.    All references to a
C     generat are to the current generator.
C
C
C                              Arguments
C
C
C     G --> Number of the current random number generator (1..32)
C                    INTEGER G
C
C**********************************************************************
C
C     Abort if generator number out of range
C
      IF (.NOT. (G.LT.0.OR.G.GT.NUMG)) GO TO 10
      WRITE (*,FMT=*) ' Generator number out of range in SETCGN:',
     +  ' Legal range is 1 to ',NUMG,' -- ABORT!'
      STOP ' Generator number out of range in SETCGN'

   10 CURNTG = G
      RETURN

      END
c -------------------------------------------------
      SUBROUTINE GETSD(ISEED1,ISEED2)
C**********************************************************************
C
C     SUBROUTINE GETSD(G,ISEED1,ISEED2)
C               GET SeeD
C
C     Returns the value of two integer seeds of the current generator
C
C     This is a transcription from Pascal to Fortran of routine
C     Get_State from the paper
C
C     L'Ecuyer, P. and  Cote,  S. "Implementing a Random Number
C     Package with Splitting Facilities."  ACM  Transactions on
C     Mathematical Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C
C     ISEED1 <- First integer seed of generator G
C                                   INTEGER ISEED1
C
C     ISEED2 <- Second integer seed of generator G
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISEED1,ISEED2
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER G
C     ..
C     .. External Functions ..
      LOGICAL QRGNIN
      EXTERNAL QRGNIN
C     ..
C     .. External Subroutines ..
      EXTERNAL DGETCGN
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/
C     ..
C     .. Executable Statements ..
C     Abort unless random number generator initialized
      IF (QRGNIN()) GO TO 10
      WRITE (*,FMT=*) ' GETSD called before random number generator ',
     +  ' initialized -- abort!'
      STOP ' GETSD called before random number generator initialized'

   10 CALL DGETCGN(G)
      ISEED1 = CG1(G)
      ISEED2 = CG2(G)
      RETURN

      END
c -------------------------------------------------
      INTEGER FUNCTION IGNLGI()
C**********************************************************************
C
C     INTEGER FUNCTION IGNLGI()
C               GeNerate LarGe Integer
C
C     Returns a random integer following a uniform distribution over
C     (1, 2147483562) using the current generator.
C
C     This is a transcription from Pascal to Fortran of routine
C     Random from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER CURNTG,K,S1,S2,Z
      LOGICAL QQSSD
C     ..
C     .. External Functions ..
      LOGICAL QRGNIN
      EXTERNAL QRGNIN
C     ..
C     .. External Subroutines ..
      EXTERNAL DGETCGN,INRGCM,RGNQSD,SETALL
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/
C     ..
C     .. Executable Statements ..
C
C     IF THE RANDOM NUMBER PACKAGE HAS NOT BEEN INITIALIZED YET, DO SO.
C     IT CAN BE INITIALIZED IN ONE OF TWO WAYS : 1) THE FIRST CALL TO
C     THIS ROUTINE  2) A CALL TO SETALL.
C
      IF (.NOT. (QRGNIN())) CALL INRGCM
      CALL RGNQSD(QQSSD)
      IF (.NOT. (QQSSD)) CALL SETALL(1234567890,123456789)
C
C     Get Current Generator
C
      CALL DGETCGN(CURNTG)
      S1 = CG1(CURNTG)
      S2 = CG2(CURNTG)
      K = S1/53668
      S1 = A1* (S1-K*53668) - K*12211
      IF (S1.LT.0) S1 = S1 + M1
      K = S2/52774
      S2 = A2* (S2-K*52774) - K*3791
      IF (S2.LT.0) S2 = S2 + M2
      CG1(CURNTG) = S1
      CG2(CURNTG) = S2
      Z = S1 - S2
      IF (Z.LT.1) Z = Z + M1 - 1
      IF (QANTI(CURNTG)) Z = M1 - Z
      IGNLGI = Z
      RETURN

      END
c -------------------------------------------------
      INTEGER FUNCTION IGNUIN(LOW,HIGH)
C**********************************************************************
C
C     INTEGER FUNCTION IGNUIN( LOW, HIGH )
C
C               GeNerate Uniform INteger
C
C
C                              Function
C
C
C     Generates an integer uniformly distributed between LOW and HIGH.
C
C
C                              Arguments
C
C
C     LOW --> Low bound (inclusive) on integer value to be generated
C                         INTEGER LOW
C
C     HIGH --> High bound (inclusive) on integer value to be generated
C                         INTEGER HIGH
C
C
C                              Note
C
C
C     If (HIGH-LOW) > 2,147,483,561 prints error message on * unit and
C     stops the program.
C
C**********************************************************************

C     IGNLGI generates integers between 1 and 2147483562
C     MAXNUM is 1 less than maximum generable value
C     .. Parameters ..
      INTEGER MAXNUM
      PARAMETER (MAXNUM=2147483561)
      CHARACTER*(*) ERR1,ERR2
      PARAMETER (ERR1='LOW > HIGH in IGNUIN',
     +          ERR2=' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN')
C     ..
C     .. Scalar Arguments ..
      INTEGER HIGH,LOW
C     ..
C     .. Local Scalars ..
      INTEGER ERR,IGN,MAXNOW,RANGE,RANP1
C     ..
C     .. External Functions ..
      INTEGER IGNLGI
      EXTERNAL IGNLGI
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C     ..
C     .. Executable Statements ..
      IF (.NOT. (LOW.GT.HIGH)) GO TO 10
      ERR = 1
C      ABORT-PROGRAM
      GO TO 80

   10 RANGE = HIGH - LOW
      IF (.NOT. (RANGE.GT.MAXNUM)) GO TO 20
      ERR = 2
C      ABORT-PROGRAM
      GO TO 80

   20 IF (.NOT. (LOW.EQ.HIGH)) GO TO 30
      IGNUIN = LOW
      RETURN

      GO TO 70

C     Number to be generated should be in range 0..RANGE
C     Set MAXNOW so that the number of integers in 0..MAXNOW is an
C     integral multiple of the number in 0..RANGE

   30 RANP1 = RANGE + 1
      MAXNOW = (MAXNUM/RANP1)*RANP1
   40 IGN = IGNLGI() - 1
      IF (.NOT. (IGN.LE.MAXNOW)) GO TO 50
      IGNUIN = LOW + MOD(IGN,RANP1)
      RETURN

   50 GO TO 40

   60 CONTINUE
   70 CONTINUE
   80 IF (.NOT. (ERR.EQ.1)) GO TO 90
      WRITE (*,FMT=*) ERR1
      GO TO 100

C     TO ABORT-PROGRAM
   90 WRITE (*,FMT=*) ERR2
  100 WRITE (*,FMT=*) ' LOW: ',LOW,' HIGH: ',HIGH
      WRITE (*,FMT=*) ' Abort on Fatal ERROR'
      IF (.NOT. (ERR.EQ.1)) GO TO 110
      STOP 'LOW > HIGH in IGNUIN'

      GO TO 120

  110 STOP ' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN'

  120 END
c -------------------------------------------------
      SUBROUTINE INITGN(ISDTYP)
C**********************************************************************
C
C     SUBROUTINE INITGN(ISDTYP)
C          INIT-ialize current G-e-N-erator
C
C     Reinitializes the state of the current generator
C
C     This is a transcription from Pascal to Fortran of routine
C     Init_Generator from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISDTYP -> The state to which the generator is to be set
C
C          ISDTYP = -1  => sets the seeds to their initial value
C          ISDTYP =  0  => sets the seeds to the first value of
C                          the current block
C          ISDTYP =  1  => sets the seeds to the first value of
C                          the next block
C
C                                   INTEGER ISDTYP
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISDTYP
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER G
C     ..
C     .. External Functions ..
      LOGICAL QRGNIN
      INTEGER MLTMOD
      EXTERNAL QRGNIN,MLTMOD
C     ..
C     .. External Subroutines ..
      EXTERNAL DGETCGN
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/
C     ..
C     .. Executable Statements ..
C     Abort unless random number generator initialized
      IF (QRGNIN()) GO TO 10
      WRITE (*,FMT=*) ' INITGN called before random number generator ',
     +  ' initialized -- abort!'
      STOP ' INITGN called before random number generator initialized'

   10 CALL DGETCGN(G)
      IF ((-1).NE. (ISDTYP)) GO TO 20
      LG1(G) = IG1(G)
      LG2(G) = IG2(G)
      GO TO 50

   20 IF ((0).NE. (ISDTYP)) GO TO 30
      CONTINUE
      GO TO 50
C     do nothing
   30 IF ((1).NE. (ISDTYP)) GO TO 40
      LG1(G) = MLTMOD(A1W,LG1(G),M1)
      LG2(G) = MLTMOD(A2W,LG2(G),M2)
      GO TO 50

   40 STOP 'ISDTYP NOT IN RANGE'

   50 CG1(G) = LG1(G)
      CG2(G) = LG2(G)
      RETURN

      END
c -------------------------------------------------
      SUBROUTINE INRGCM
C**********************************************************************
C
C     SUBROUTINE INRGCM()
C          INitialize Random number Generator CoMmon
C
C
C                              Function
C
C
C     Initializes common area for random number generator. This saves
C     the nuisance of a BLOCK DATA routine and the difficulty of
C     assuring that the routine is loaded with the other routines.
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER I
      LOGICAL QDUM
C     ..
C     .. External Functions ..
      LOGICAL QRGNSN
      EXTERNAL QRGNSN
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/
C     ..
C     .. Executable Statements ..
C     V=20;                            W=30;
C
C     A1W = MOD(A1**(2**W),M1)         A2W = MOD(A2**(2**W),M2)
C     A1VW = MOD(A1**(2**(V+W)),M1)    A2VW = MOD(A2**(2**(V+W)),M2)
C
C   If V or W is changed A1W, A2W, A1VW, and A2VW need to be
C   recomputed.  An efficient way to precompute a**(2*j) MOD m is to
C   start with a and square it j times modulo m using the function
C   MLTMOD.
C
      M1 = 2147483563
      M2 = 2147483399
      A1 = 40014
      A2 = 40692
      A1W = 1033780774
      A2W = 1494757890
      A1VW = 2082007225
      A2VW = 784306273
      DO 10 I = 1,NUMG
          QANTI(I) = .FALSE.
   10 CONTINUE
C
C     Tell the world that common has been initialized
C
      QDUM = QRGNSN(.TRUE.)
      RETURN

      END
c -------------------------------------------------
      INTEGER FUNCTION MLTMOD(A,S,M)
C**********************************************************************
C
C     INTEGER FUNCTION MLTMOD(A,S,M)
C
C                    Returns (A*S) MOD M
C
C     This is a transcription from Pascal to Fortran of routine
C     MULtMod_Decompos from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     A, S, M  -->
C                         INTEGER A,S,M
C
C**********************************************************************
C     .. Parameters ..
      INTEGER H
      PARAMETER (H=32768)
C     ..
C     .. Scalar Arguments ..
      INTEGER A,M,S
C     ..
C     .. Local Scalars ..
      INTEGER A0,A1,K,P,Q,QH,RH
C     ..
C     .. Executable Statements ..
C
C     H = 2**((b-2)/2) where b = 32 because we are using a 32 bit
C      machine. On a different machine recompute H
C
      IF (.NOT. (A.LE.0.OR.A.GE.M.OR.S.LE.0.OR.S.GE.M)) GO TO 10
      WRITE (*,FMT=*) ' A, M, S out of order in MLTMOD - ABORT!'
      WRITE (*,FMT=*) ' A = ',A,' S = ',S,' M = ',M
      WRITE (*,FMT=*) ' MLTMOD requires: 0 < A < M; 0 < S < M'
      STOP ' A, M, S out of order in MLTMOD - ABORT!'

   10 IF (.NOT. (A.LT.H)) GO TO 20
      A0 = A
      P = 0
      GO TO 120

   20 A1 = A/H
      A0 = A - H*A1
      QH = M/H
      RH = M - H*QH
      IF (.NOT. (A1.GE.H)) GO TO 50
      A1 = A1 - H
      K = S/QH
      P = H* (S-K*QH) - K*RH
   30 IF (.NOT. (P.LT.0)) GO TO 40
      P = P + M
      GO TO 30

   40 GO TO 60

   50 P = 0
C
C     P = (A2*S*H)MOD M
C
   60 IF (.NOT. (A1.NE.0)) GO TO 90
      Q = M/A1
      K = S/Q
      P = P - K* (M-A1*Q)
      IF (P.GT.0) P = P - M
      P = P + A1* (S-K*Q)
   70 IF (.NOT. (P.LT.0)) GO TO 80
      P = P + M
      GO TO 70

   80 CONTINUE
   90 K = P/QH
C
C     P = ((A2*H + A1)*S)MOD M
C
      P = H* (P-K*QH) - K*RH
  100 IF (.NOT. (P.LT.0)) GO TO 110
      P = P + M
      GO TO 100

  110 CONTINUE
  120 IF (.NOT. (A0.NE.0)) GO TO 150
C
C     P = ((A2*H + A1)*H*S)MOD M
C
      Q = M/A0
      K = S/Q
      P = P - K* (M-A0*Q)
      IF (P.GT.0) P = P - M
      P = P + A0* (S-K*Q)
  130 IF (.NOT. (P.LT.0)) GO TO 140
      P = P + M
      GO TO 130

  140 CONTINUE
  150 MLTMOD = P
C
      RETURN

      END
c -------------------------------------------------
      LOGICAL FUNCTION QRGNIN()
C**********************************************************************
C
C     LOGICAL FUNCTION QRGNIN()
C               Q Random GeNerators INitialized?
C
C     A trivial routine to determine whether or not the random
C     number generator has been initialized.  Returns .TRUE. if
C     it has, else .FALSE.
C
C**********************************************************************
C     .. Scalar Arguments ..
      LOGICAL QVALUE
C     ..
C     .. Local Scalars ..
      LOGICAL QINIT
C     ..
C     .. Entry Points ..
      LOGICAL QRGNSN
C     ..
C     .. Save statement ..
      SAVE QINIT
C     ..
C     .. Data statements ..
      DATA QINIT/.FALSE./
C     ..
C     .. Executable Statements ..
      QRGNIN = QINIT
      RETURN

      ENTRY QRGNSN(QVALUE)
C**********************************************************************
C
C     LOGICAL FUNCTION QRGNSN( QVALUE )
C               Q Random GeNerators Set whether iNitialized
C
C     Sets state of whether random number generator is initialized
C     to QVALUE.
C
C     This routine is actually an entry in QRGNIN, hence it is a
C     logical function.  It returns the (meaningless) value .TRUE.
C
C**********************************************************************
      QINIT = QVALUE
      QRGNSN = .TRUE.
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DRANFD()
C**********************************************************************
C     NAME CHANGED BY DJS TO AVOID NAMING CONFLICTS
C**********************************************************************
C
C          DOUBLE PRECISION FUNCTION DRANFD()
C                RANDom number generator as a Function
C
C     Returns a random floating point number from a uniform
C     distribution over 0 - 1 (endpoints of this interval are not
C     returned) using the current generator
C
C     This is a transcription from Pascal to Fortran of routine
C     Uniform_01 from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C**********************************************************************
C     .. External Functions ..
      INTEGER IGNLGI
      EXTERNAL IGNLGI
C     ..
C     .. Executable Statements ..
C
C     4.656613057E-10 is 1/M1  M1 is set in a data statement in IGNLGI
C      and is currently 2147483563. If M1 changes, change this also.
C
      DRANFD = IGNLGI()*4.656613057D-10
      RETURN

      END
c -------------------------------------------------
      SUBROUTINE SETALL(ISEED1,ISEED2)
C**********************************************************************
C
C      SUBROUTINE SETALL(ISEED1,ISEED2)
C               SET ALL random number generators
C
C     Sets the initial seed of generator 1 to ISEED1 and ISEED2. The
C     initial seeds of the other generators are set accordingly, and
C     all generators states are set to these seeds.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Initial_Seed from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISEED1 -> First of two integer seeds
C                                   INTEGER ISEED1
C
C     ISEED2 -> Second of two integer seeds
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISEED1,ISEED2
      LOGICAL QSSD
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER G,OCGN
      LOGICAL QQSSD
C     ..
C     .. External Functions ..
      INTEGER MLTMOD
      LOGICAL QRGNIN
      EXTERNAL MLTMOD,QRGNIN
C     ..
C     .. External Subroutines ..
      EXTERNAL DGETCGN,INITGN,INRGCM,SETCGN
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/,QQSSD
C     ..
C     .. Data statements ..
      DATA QQSSD/.FALSE./
C     ..
C     .. Executable Statements ..
C
C     TELL IGNLGI, THE ACTUAL NUMBER GENERATOR, THAT THIS ROUTINE
C      HAS BEEN CALLED.
C
      QQSSD = .TRUE.
      CALL DGETCGN(OCGN)
C
C     Initialize Common Block if Necessary
C
      IF (.NOT. (QRGNIN())) CALL INRGCM
      IG1(1) = ISEED1
      IG2(1) = ISEED2
      CALL INITGN(-1)
      DO 10 G = 2,NUMG
          IG1(G) = MLTMOD(A1VW,IG1(G-1),M1)
          IG2(G) = MLTMOD(A2VW,IG2(G-1),M2)
          CALL SETCGN(G)
          CALL INITGN(-1)
   10 CONTINUE
      CALL SETCGN(OCGN)
      RETURN

      ENTRY RGNQSD(QSSD)
C**********************************************************************
C
C     SUBROUTINE RGNQSD
C                    Random Number Generator Query SeeD set?
C
C     Returns (LOGICAL) QSSD as .TRUE. if SETALL has been invoked,
C     otherwise returns .FALSE.
C
C**********************************************************************
      QSSD = QQSSD
      RETURN

      END
c -------------------------------------------------
      SUBROUTINE SETSD(ISEED1,ISEED2)
C**********************************************************************
C
C     SUBROUTINE SETSD(ISEED1,ISEED2)
C               SET S-ee-D of current generator
C
C     Resets the initial seed of the current generator to ISEED1 and
C     ISEED2. The seeds of the other generators remain unchanged.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Seed from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISEED1 -> First integer seed
C                                   INTEGER ISEED1
C
C     ISEED2 -> Second integer seed
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER NUMG
      PARAMETER (NUMG=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER ISEED1,ISEED2
C     ..
C     .. Scalars in Common ..
      INTEGER A1,A1VW,A1W,A2,A2VW,A2W,M1,M2
C     ..
C     .. Arrays in Common ..
      INTEGER CG1(NUMG),CG2(NUMG),IG1(NUMG),IG2(NUMG),LG1(NUMG),
     +        LG2(NUMG)
      LOGICAL QANTI(NUMG)
C     ..
C     .. Local Scalars ..
      INTEGER G
C     ..
C     .. External Functions ..
      LOGICAL QRGNIN
      EXTERNAL QRGNIN
C     ..
C     .. External Subroutines ..
      EXTERNAL DGETCGN,INITGN
C     ..
C     .. Common blocks ..
      COMMON /GLOBE/M1,M2,A1,A2,A1W,A2W,A1VW,A2VW,IG1,IG2,LG1,LG2,CG1,
     +       CG2,QANTI
C     ..
C     .. Save statement ..
      SAVE /GLOBE/
C     ..
C     .. Executable Statements ..
C     Abort unless random number generator initialized
      IF (QRGNIN()) GO TO 10
      WRITE (*,FMT=*) ' SETSD called before random number generator ',
     +  ' initialized -- abort!'
      STOP ' SETSD called before random number generator initialized'

   10 CALL DGETCGN(G)
      IG1(G) = ISEED1
      IG2(G) = ISEED2
      CALL INITGN(-1)
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DSEXPO()
      DOUBLE PRECISION Q
      DOUBLE PRECISION Q1
      DOUBLE PRECISION A
      DOUBLE PRECISION U
      DOUBLE PRECISION DRANFD
      DOUBLE PRECISION USTAR
      DOUBLE PRECISION UMIN
C*********************************************************************C
C                                                                     C
C                                                                     C
C    (STANDARD-)  E X P O N E N T I A L   DISTRIBUTION                C
C                                                                     C
C                                                                     C
C*********************************************************************C
**********************************************************************C
C                                                                     C
C    FOR DETAILS SEE:                                                 C
C                                                                     C
C              AHRENS, J.H. AND DIETER, U.                            C
C              COMPUTER METHODS FOR SAMPLING FROM THE                 C
C              EXPONENTIAL AND NORMAL DISTRIBUTIONS.                  C
C              COMM. ACM, 15,10 (OCT. 1972), 873 - 882.               C
C                                                                     C
C    ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM       C
C    'SA' IN THE ABOVE PAPER (SLIGHTLY MODIFIED IMPLEMENTATION)       C
C                                                                     C
C    Modified by Barry W. Brown, Feb 3, 1988 to use DRANFD instead of C
C    SUNIF.  The argument IR thus goes away.                          C
C                                                                     C
C*********************************************************************C
C
      DIMENSION Q(8)
      EQUIVALENCE (Q(1),Q1)
C
C     Q(N) = SUM(ALOG(2.0)**K/K!)    K=1,..,N ,      THE HIGHEST N
C     (HERE 8) IS DETERMINED BY Q(N)=1.0 WITHIN STANDARD PRECISION
C
      DATA Q/.6931472D0,.9333737D0,.9888778D0,.9984959D0,.9998293D0,
     +     .9999833D0,.9999986D0,.9999999D0/
C
   10 A = 0.0D0
      U = DRANFD()
      GO TO 30

   20 A = A + Q1
   30 U = U + U
      IF (U.LE.1.0D0) GO TO 20
   40 U = U - 1.0D0
      IF (U.GT.Q1) GO TO 60
   50 DSEXPO = A + U
      RETURN

   60 I = 1
      USTAR = DRANFD()
      UMIN = USTAR
   70 USTAR = DRANFD()
      IF (USTAR.LT.UMIN) UMIN = USTAR
   80 I = I + 1
      IF (U.GT.Q(I)) GO TO 70
   90 DSEXPO = A + UMIN*Q1
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DSGAMMA(A)
      DOUBLE PRECISION A
      DOUBLE PRECISION Q1
      DOUBLE PRECISION Q2
      DOUBLE PRECISION Q3
      DOUBLE PRECISION Q4
      DOUBLE PRECISION Q5
      DOUBLE PRECISION Q6
      DOUBLE PRECISION Q7
      DOUBLE PRECISION A1
      DOUBLE PRECISION A2
      DOUBLE PRECISION A3
      DOUBLE PRECISION A4
      DOUBLE PRECISION A5
      DOUBLE PRECISION A6
      DOUBLE PRECISION A7
      DOUBLE PRECISION E1
      DOUBLE PRECISION E2
      DOUBLE PRECISION E3
      DOUBLE PRECISION E4
      DOUBLE PRECISION E5
      DOUBLE PRECISION AA
      DOUBLE PRECISION AAA
      DOUBLE PRECISION SQRT32
      DOUBLE PRECISION S2
      DOUBLE PRECISION S
      DOUBLE PRECISION D
      DOUBLE PRECISION Q0
      DOUBLE PRECISION B
      DOUBLE PRECISION SI
      DOUBLE PRECISION C
      DOUBLE PRECISION T
      DOUBLE PRECISION DSNORM
      DOUBLE PRECISION X
      DOUBLE PRECISION U
      DOUBLE PRECISION DRANFD
      DOUBLE PRECISION R
      DOUBLE PRECISION V
      DOUBLE PRECISION Q
      DOUBLE PRECISION E
      DOUBLE PRECISION DSEXPO
      DOUBLE PRECISION W
      DOUBLE PRECISION P
C*********************************************************************C
C                                                                     C
C                                                                     C
C    (STANDARD-)  G A M M A  DISTRIBUTION                             C
C                                                                     C
C                                                                     C
C*********************************************************************C
C*********************************************************************C
C                                                                     C
C              PARAMETER  A >= 1.0  !                                 C
C                                                                     C
C*********************************************************************C
C                                                                     C
C    FOR DETAILS SEE:                                                 C
C                                                                     C
C              AHRENS, J.H. AND DIETER, U.                            C
C              GENERATING GAMMA VARIATES BY A                         C
C              MODIFIED REJECTION TECHNIQUE.                          C
C              COMM. ACM, 25,1 (JAN. 1982), 47 - 54.                  C
C                                                                     C
C    STEP NUMBERS CORRESPOND TO ALGORITHM 'GD' IN THE ABOVE PAPER     C
C                                (STRAIGHTFORWARD IMPLEMENTATION)     C
C                                                                     C
C    Modified by Barry W. Brown, Feb 3, 1988 to use DRANFD instead of C
C    SUNIF.  The argument IR thus goes away.                          C
C                                                                     C
C*********************************************************************C
C                                                                     C
C              PARAMETER  0.0 < A < 1.0  !                            C
C                                                                     C
C*********************************************************************C
C                                                                     C
C    FOR DETAILS SEE:                                                 C
C                                                                     C
C              AHRENS, J.H. AND DIETER, U.                            C
C              COMPUTER METHODS FOR SAMPLING FROM GAMMA,              C
C              BETA, POISSON AND BINOMIAL DISTRIBUTIONS.              C
C              COMPUTING, 12 (1974), 223 - 246.                       C
C                                                                     C
C    (ADAPTED IMPLEMENTATION OF ALGORITHM 'GS' IN THE ABOVE PAPER)    C
C                                                                     C
C*********************************************************************C
C
C    INPUT: A =PARAMETER (MEAN) OF THE STANDARD GAMMA DISTRIBUTION
C    OUTPUT: DSGAMMA = SAMPLE FROM THE GAMMA-(A)-DISTRIBUTION
C
C    COEFFICIENTS Q(K) - FOR Q0 = SUM(Q(K)*A**(-K))
C    COEFFICIENTS A(K) - FOR Q = Q0+(T*T/2)*SUM(A(K)*V**K)
C    COEFFICIENTS E(K) - FOR EXP(Q)-1 = SUM(E(K)*Q**K)
C
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/.04166669D0,.02083148D0,.00801191D0,
     +     .00144121D0,-.00007388D0,.00024511D0,.00024240D0/
      DATA A1,A2,A3,A4,A5,A6,A7/.3333333D0,-.2500030D0,.2000062D0,
     +     -.1662921D0,.1423657D0,-.1367177D0,.1233795D0/
      DATA E1,E2,E3,E4,E5/1.D0,.4999897D0,.1668290D0,.0407753D0,
     +     .0102930D0/
C
C     PREVIOUS A PRE-SET TO ZERO - AA IS A', AAA IS A"
C     SQRT32 IS THE SQUAREROOT OF 32 = 5.656854249492380
C
      DATA AA/0.0D0/,AAA/0.0D0/,SQRT32/5.656854D0/
C
C     SAVE STATEMENTS
C
      SAVE AA,AAA,S2,S,D,Q0,B,SI,C
C
      IF (A.EQ.AA) GO TO 10
      IF (A.LT.1.0D0) GO TO 140
C
C     STEP  1:  RECALCULATIONS OF S2,S,D IF A HAS CHANGED
C
      AA = A
      S2 = A - 0.5D0
      S = SQRT(S2)
      D = SQRT32 - 12.0D0*S
C
C     STEP  2:  T=STANDARD NORMAL DEVIATE,
C               X=(S,1/2)-NORMAL DEVIATE.
C               IMMEDIATE ACCEPTANCE (I)
C
   10 T = DSNORM()
      X = S + 0.5D0*T
      DSGAMMA = X*X
      IF (T.GE.0.0D0) RETURN
C
C     STEP  3:  U= 0,1 -UNIFORM SAMPLE. SQUEEZE ACCEPTANCE (S)
C
      U = DRANFD()
      IF (D*U.LE.T*T*T) RETURN
C
C     STEP  4:  RECALCULATIONS OF Q0,B,SI,C IF NECESSARY
C
      IF (A.EQ.AAA) GO TO 40
      AAA = A
      R = 1.0D0/A
      Q0 = ((((((Q7*R+Q6)*R+Q5)*R+Q4)*R+Q3)*R+Q2)*R+Q1)*R
C
C               APPROXIMATION DEPENDING ON SIZE OF PARAMETER A
C               THE CONSTANTS IN THE EXPRESSIONS FOR B, SI AND
C               C WERE ESTABLISHED BY NUMERICAL EXPERIMENTS
C
      IF (A.LE.3.686D0) GO TO 30
      IF (A.LE.13.022D0) GO TO 20
C
C               CASE 3:  A .GT. 13.022
C
      B = 1.77D0
      SI = .75D0
      C = .1515D0/S
      GO TO 40
C
C               CASE 2:  3.686 .LT. A .LE. 13.022
C
   20 B = 1.654D0 + .0076D0*S2
      SI = 1.68D0/S + .275D0
      C = .062D0/S + .024D0
      GO TO 40
C
C               CASE 1:  A .LE. 3.686
C
   30 B = .463D0 + S + .178D0*S2
      SI = 1.235D0
      C = .195D0/S - .079D0 + .16D0*S
C
C     STEP  5:  NO QUOTIENT TEST IF X NOT POSITIVE
C
   40 IF (X.LE.0.0D0) GO TO 70
C
C     STEP  6:  CALCULATION OF V AND QUOTIENT Q
C
      V = T/ (S+S)
      IF (ABS(V).LE.0.25D0) GO TO 50
      Q = Q0 - S*T + 0.25D0*T*T + (S2+S2)*DLOG(1.0D0+V)
      GO TO 60

   50 Q=Q0+0.5D0*T*T*((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V
C
C     STEP  7:  QUOTIENT ACCEPTANCE (Q)
C
   60 IF (DLOG(1.0D0-U).LE.Q) RETURN
C
C     STEP  8:  E=STANDARD EXPONENTIAL DEVIATE
C               U= 0,1 -UNIFORM DEVIATE
C               T=(B,SI)-DOUBLE EXPONENTIAL (LAPLACE) SAMPLE
C
   70 E = DSEXPO()
      U = DRANFD()
      U = U + U - 1.0D0
      T = B + SIGN(SI*E,U)
      IF (.NOT. (U.GE.0.0D0)) GO TO 80
      T = B + SI*E
      GO TO 90

   80 T = B - SI*E

C
C     STEP  9:  REJECTION IF T .LT. TAU(1) = -.71874483771719
C
   90 IF (T.LT. (-.7187449D0)) GO TO 70
C
C     STEP 10:  CALCULATION OF V AND QUOTIENT Q
C
      V = T/ (S+S)
      IF (ABS(V).LE.0.25D0) GO TO 100
      Q = Q0 - S*T + 0.25D0*T*T + (S2+S2)*DLOG(1.0D0+V)
      GO TO 110

  100 Q=Q0+0.5D0*T*T*((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V
C
C     STEP 11:  HAT ACCEPTANCE (H) (IF Q NOT POSITIVE GO TO STEP 8)
C
  110 IF (Q.LE.0.0D0) GO TO 70
      IF (Q.LE.0.5D0) GO TO 120
      W = EXP(Q) - 1.0D0
      GO TO 130

  120 W = ((((E5*Q+E4)*Q+E3)*Q+E2)*Q+E1)*Q
C
C               IF T IS REJECTED, SAMPLE AGAIN AT STEP 8
C
  130 IF (C*ABS(U).GT.W*EXP(E-0.5D0*T*T)) GO TO 70
      X = S + 0.5D0*T
      DSGAMMA = X*X
      RETURN
C
C     ALTERNATE METHOD FOR PARAMETERS A BELOW 1  (.3678794=EXP(-1.))
C
  140 AA = 0.0D0
      B = 1.0D0 + .3678794D0*A
  150 P = B*DRANFD()
      IF (P.GE.1.0D0) GO TO 160
      DSGAMMA = EXP(DLOG(P)/A)
      IF (DSEXPO().LT.DSGAMMA) GO TO 150
      RETURN

  160 DSGAMMA = -DLOG((B-P)/A)
      IF (DSEXPO().LT. (1.0D0-A)*DLOG(DSGAMMA)) GO TO 150
      RETURN

      END
c -------------------------------------------------
      DOUBLE PRECISION FUNCTION DSNORM()
      DOUBLE PRECISION A
      DOUBLE PRECISION D
      DOUBLE PRECISION T
      DOUBLE PRECISION H
      DOUBLE PRECISION U
      DOUBLE PRECISION DRANFD
      DOUBLE PRECISION S
      DOUBLE PRECISION USTAR
      DOUBLE PRECISION AA
      DOUBLE PRECISION W
      DOUBLE PRECISION Y
      DOUBLE PRECISION TT
C*********************************************************************C
C                                                                     C
C                                                                     C
C    (STANDARD-)  N O R M A L  DISTRIBUTION                           C
C                                                                     C
C                                                                     C
C*********************************************************************C
C*********************************************************************C
C                                                                     C
C    FOR DETAILS SEE:                                                 C
C                                                                     C
C              AHRENS, J.H. AND DIETER, U.                            C
C              EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
C              SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
C              MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
C                                                                     C
C    ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
C    (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
C                                                                     C
C    Modified by Barry W. Brown, Feb 3, 1988 to use DRANFD instead of C
C    SUNIF.  The argument IR thus goes away.                          C
C                                                                     C
C*********************************************************************C
C
      DIMENSION A(32),D(31),T(31),H(31)
C
C     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
C     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
C
      DATA A/0.0D0,.3917609D-1,.7841241D-1,.1177699D0,.1573107D0,
     +     .1970991D0,.2372021D0,.2776904D0,.3186394D0,.3601299D0,
     +     .4022501D0,.4450965D0,.4887764D0,.5334097D0,.5791322D0,
     +     .6260990D0,.6744898D0,.7245144D0,.7764218D0,.8305109D0,
     +     .8871466D0,.9467818D0,1.009990D0,1.077516D0,1.150349D0,
     +     1.229859D0,1.318011D0,1.417797D0,1.534121D0,1.675940D0,
     +     1.862732D0,2.153875D0/
      DATA D/5*0.0D0,.2636843D0,.2425085D0,.2255674D0,.2116342D0,
     +     .1999243D0,.1899108D0,.1812252D0,.1736014D0,.1668419D0,
     +     .1607967D0,.1553497D0,.1504094D0,.1459026D0,.1417700D0,
     +     .1379632D0,.1344418D0,.1311722D0,.1281260D0,.1252791D0,
     +     .1226109D0,.1201036D0,.1177417D0,.1155119D0,.1134023D0,
     +     .1114027D0,.1095039D0/
      DATA T/.7673828D-3,.2306870D-2,.3860618D-2,.5438454D-2,
     +     .7050699D-2,.8708396D-2,.1042357D-1,.1220953D-1,.1408125D-1,
     +     .1605579D-1,.1815290D-1,.2039573D-1,.2281177D-1,.2543407D-1,
     +     .2830296D-1,.3146822D-1,.3499233D-1,.3895483D-1,.4345878D-1,
     +     .4864035D-1,.5468334D-1,.6184222D-1,.7047983D-1,.8113195D-1,
     +     .9462444D-1,.1123001D0,.1364980D0,.1716886D0,.2276241D0,
     +     .3304980D0,.5847031D0/
      DATA H/.3920617D-1,.3932705D-1,.3950999D-1,.3975703D-1,
     +     .4007093D-1,.4045533D-1,.4091481D-1,.4145507D-1,.4208311D-1,
     +     .4280748D-1,.4363863D-1,.4458932D-1,.4567523D-1,.4691571D-1,
     +     .4833487D-1,.4996298D-1,.5183859D-1,.5401138D-1,.5654656D-1,
     +     .5953130D-1,.6308489D-1,.6737503D-1,.7264544D-1,.7926471D-1,
     +     .8781922D-1,.9930398D-1,.1155599D0,.1404344D0,.1836142D0,
     +     .2790016D0,.7010474D0/
C
   10 U = DRANFD()
      S = 0.0D0
      IF (U.GT.0.5D0) S = 1.0D0
      U = U + U - S
   20 U = 32.0D0*U
      I = INT(U)
      IF (I.EQ.32) I = 31
      IF (I.EQ.0) GO TO 100
C
C                                START CENTER
C
   30 USTAR = U - DBLE(I)
      AA = A(I)
   40 IF (USTAR.LE.T(I)) GO TO 60
      W = (USTAR-T(I))*H(I)
C
C                                EXIT   (BOTH CASES)
C
   50 Y = AA + W
      DSNORM = Y
      IF (S.EQ.1.0D0) DSNORM = -Y
      RETURN
C
C                                CENTER CONTINUED
C
   60 U = DRANFD()
      W = U* (A(I+1)-AA)
      TT = (0.5D0*W+AA)*W
      GO TO 80

   70 TT = U
      USTAR = DRANFD()
   80 IF (USTAR.GT.TT) GO TO 50
   90 U = DRANFD()
      IF (USTAR.GE.U) GO TO 70
      USTAR = DRANFD()
      GO TO 40
C
C                                START TAIL
C
  100 I = 6
      AA = A(32)
      GO TO 120

  110 AA = AA + D(I)
      I = I + 1
  120 U = U + U
      IF (U.LT.1.0D0) GO TO 110
  130 U = U - 1.0D0
  140 W = U*D(I)
      TT = (0.5D0*W+AA)*W
      GO TO 160

  150 TT = U
  160 USTAR = DRANFD()
      IF (USTAR.GT.TT) GO TO 50
  170 U = DRANFD()
      IF (USTAR.GE.U) GO TO 150
      U = DRANFD()
      GO TO 140

      END
