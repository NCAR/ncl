C NCLFORTSTART
      SUBROUTINE DINT2P(PPIN,XXIN,PIN,XIN,P,X,NPIN,PPOUT,XXOUT,NPOUT
     +                 ,LINLOG,XMSG,IER)
      IMPLICIT NONE
c
c This code was designed for one simple task. It has since
c been mangled and abused for assorted reasons. For example,
c early gfortran compilers had some issues with automatic arrays.
c Hence, the C-Wrapper was used to create 'work' arrays which
c were then passed to this code.  The original focused (non-NCL) 
c task was to handle PPIN & PPOUT that had the same 'monotonicity.' 
c Extra code was added to handle the more general case. 
c Blah-Blah:  Punch line: it is embarrassingly convoluted!!!
c
c                                                ! input types
      INTEGER NPIN,NPOUT,LINLOG,IER
      DOUBLE PRECISION PPIN(NPIN),XXIN(NPIN),PPOUT(NPOUT),XMSG
C                                                ! output
      DOUBLE PRECISION XXOUT(NPOUT)
C                                                ! work
      DOUBLE PRECISION PIN(NPIN),XIN(NPIN),P(NPIN),X(NPIN)
      DOUBLE PRECISION POUT(NPOUT),XOUT(NPOUT)
C NCLEND

c local
      INTEGER J1,NP,NL,NIN,NLMAX,NPLVL,NLSAVE,NP1,NO1,N1,N2,LOGLIN,
     +        NLSTRT
      DOUBLE PRECISION SLOPE,PA,PB,PC
c
      LOGLIN = ABS(LINLOG)

c error check: enough points: pressures consistency?

      IER = 0
      IF (NPOUT.GT.0) THEN
          DO NP = 1,NPOUT
              XXOUT(NP) = XMSG
          END DO
      END IF

      IF (NPIN.LT.2 .OR. NPOUT.LT.1) IER = IER + 1

C C C IF (NPOUT.GT.1) THEN
C C C     IF ((PPIN(1).GT.PPIN(2).AND.PPOUT(1).LT.PPOUT(2)) .OR.
C C C+        (PPIN(1).LT.PPIN(2).AND.PPOUT(1).GT.PPOUT(2))) THEN
C C C         IER = IER + 10
C              PRINT *,'INT2P: ppout is in different order than ppin'
C              PRINT *,'       Must both be monotonic {in/de}creasing'
C C C     END IF
C C C END IF

      IF (IER.NE.0) THEN
C          PRINT *,'INT2P: error exit: ier=',IER
          RETURN
      END IF

c should *input arrays* be reordered? want p(1) > p(2) > p(3) etc
c so that it will match order for which code was originally designed
c copy to 'work'  arrays

      NP1 = 0
      NO1 = 0
      IF (PPIN(1).LT.PPIN(2)) THEN    
          NP1 = NPIN + 1
      END IF
      IF (PPOUT(1).LT.PPOUT(2)) THEN 
          NO1 = NPOUT + 1
      END IF

      DO NP = 1,NPIN
          PIN(NP) = PPIN(ABS(NP1-NP))
          XIN(NP) = XXIN(ABS(NP1-NP))
      END DO

      DO NP = 1,NPOUT   
          POUT(NP) = PPOUT(ABS(NO1-NP))
      END DO
c
c eliminate XIN levels with missing data. 
c .   This can happen with observational data.
c
      NL = 0
      DO NP = 1,NPIN
          IF (XIN(NP).NE.XMSG .AND. PIN(NP).NE.XMSG) THEN
              NL = NL + 1
              P(NL) = PIN(NP)
              X(NL) = XIN(NP)
          END IF
      END DO
      NLMAX = NL
C                                                ! all missing data
      IF (NLMAX.LT.2) THEN
          IER = IER + 1000
C          PRINT *,'INT2P: ier=',IER
          RETURN
      END IF

c ===============> pressure in decreasing order <================
c perform the interpolation  [pin(1)>pin(2)>...>pin(npin)]
c                                                      ( p ,x)
c ------------------------- p(nl+1), x(nl+1)   example (200,5)
c .
c ------------------------- pout(np), xout(np)         (250,?)
c .
c ------------------------- p(nl)  , x(nl)             (300,10)


c exact p-level matches
      NLSTRT = 1
      NLSAVE = 1
      DO NP = 1,NPOUT
          XOUT(NP) = XMSG
          DO NL = NLSTRT,NLMAX
              IF (POUT(NP).EQ.P(NL)) THEN
                  XOUT(NP) = X(NL)
                  NLSAVE = NL + 1
                  GO TO 10
              END IF
          END DO
   10     NLSTRT = NLSAVE
      END DO

      IF (LOGLIN.EQ.1) THEN
          DO NP = 1,NPOUT
              DO NL = 1,NLMAX - 1
                  IF (POUT(NP).LT.P(NL) .AND. POUT(NP).GT.P(NL+1)) THEN
                      SLOPE = (X(NL)-X(NL+1))/ (P(NL)-P(NL+1))
                      XOUT(NP) = X(NL+1) + SLOPE* (POUT(NP)-P(NL+1))
                  END IF
              END DO
          END DO
      ELSE
          DO NP = 1,NPOUT
              DO NL = 1,NLMAX - 1
                  IF (POUT(NP).LT.P(NL) .AND. POUT(NP).GT.P(NL+1)) THEN
                      PA = DLOG(P(NL))
                      PB = DLOG(POUT(NP))
c special case: In case someome inadvertently enter p=0.
                      if (p(nl+1).gt.0.d0) then
                          PC = DLOG(P(NL+1))
                      else
                          PC = DLOG(1.d-4)
                      end if

                      SLOPE = (X(NL)-X(NL+1))/ (PA-PC)
                      XOUT(NP) = X(NL+1) + SLOPE* (PB-PC)
                  END IF
              END DO
          END DO
      END IF

c extrapolate?
c . use the 'last' valid slope for extrapolating

      IF (LINLOG.LT.0) THEN
          DO NP = 1,NPOUT
              DO NL = 1,NLMAX
                  IF (POUT(NP).GT.P(1)) THEN
                      IF (LOGLIN.EQ.1) THEN
                          SLOPE = (X(2)-X(1))/ (P(2)-P(1))
                          XOUT(NP) = X(1) + SLOPE* (POUT(NP)-P(1))
                      ELSE
                          PA = DLOG(P(2))
                          PB = DLOG(POUT(NP))
                          PC = DLOG(P(1))
                          SLOPE = (X(2)-X(1))/ (PA-PC)
                          XOUT(NP) = X(1) + SLOPE* (PB-PC)
                      END IF
                  ELSE IF (POUT(NP).LT.P(NLMAX)) THEN
                      N1 = NLMAX
                      N2 = NLMAX - 1
                      IF (LOGLIN.EQ.1) THEN
                          SLOPE = (X(N1)-X(N2))/ (P(N1)-P(N2))
                          XOUT(NP) = X(N1) + SLOPE* (POUT(NP)-P(N1))
                      ELSE
                          PA = DLOG(P(N1))
                          PB = DLOG(POUT(NP))
                          PC = DLOG(P(N2))
                          SLOPE = (X(N1)-X(N2))/ (PA-PC)
                          XOUT(NP) = X(N1) + SLOPE* (PB-PC)
                      END IF
                  END IF
              END DO
          END DO
      END IF

c place results in the return array;
c .   possibly .... reverse to original order

      if (NO1.GT.0) THEN
          DO NP = 1,NPOUT
             n1 = ABS(NO1-NP)
             PPOUT(NP) = POUT(n1)
             XXOUT(NP) = XOUT(n1)
          END DO
      ELSE
          DO NP = 1,NPOUT
             PPOUT(NP) = POUT(NP)
             XXOUT(NP) = XOUT(NP)
          END DO
      END IF

      RETURN
      END
