C NCLFORTSTART
      SUBROUTINE DFOURINFO(X,NPTS,NHRET,SCLPHA,AMP,PHA,PCV)
      IMPLICIT NONE

C NCL:   finfo = fourier_info(x, nhret, sclpha)

c perform a fourier analysis on a series of data points
c return the amplitude, phase and % variance explained

c input
      INTEGER NPTS,NHRET
      DOUBLE PRECISION X(NPTS),SCLPHA
c outout
      DOUBLE PRECISION AMP(NHRET),PHA(NHRET),PCV(NHRET)
C NCLEND

c local (same adjustable arrays)
      INTEGER LWRK,NHAR,NH,NHX,IER
      DOUBLE PRECISION ANOT,SCALE
      DOUBLE PRECISION WRK(4*NPTS+100)
      DOUBLE PRECISION A(NPTS/2),B(NPTS/2)
      DOUBLE PRECISION AMPX(NPTS/2),PHAX(NPTS/2),PCVX(NPTS/2)

      IER = 0
      IF (NPTS.LT.1) IER = 1
      IF (IER.NE.0) RETURN

      NHAR = NPTS/2
      LWRK = 4*NPTS + 100

      CALL DFINFO(X,NPTS,NHAR,ANOT,A,B,WRK,LWRK,AMPX,PHAX,PCVX)

      NHX = NHRET
      IF (NHRET.GT.NHAR) NHX = NHAR

      SCALE = SCLPHA
      IF (SCALE.EQ.0.0D0) SCALE = 1.0D0

      DO NH = 1,NHX
          AMP(NH) = AMPX(NH)
          PHA(NH) = PHAX(NH)*SCALE
          PCV(NH) = PCVX(NH)
      END DO

      RETURN
      END
c ------------------------------------------------------------------
      SUBROUTINE DFINFO(X,NPTS,NHAR,ANOT,A,B,WRK,LWRK,AMP,PHASE,PCVAR)
      IMPLICIT NONE

c calculate the amplitude ,phase and % variance explained per harmonic
c .   of the fourier series.

      INTEGER NPTS,NHAR,LWRK
      DOUBLE PRECISION X(NPTS)
      DOUBLE PRECISION ANOT,A(NHAR),B(NHAR),AMP(NHAR),PHASE(NHAR),
     +                 PCVAR(NHAR)
      DOUBLE PRECISION WRK(LWRK)
c local
      INTEGER N
      DOUBLE PRECISION W,VARTOT,EPS

      W = DBLE(NPTS)/ (8.D0*ATAN(1.D0))
      EPS = 1.D-05

      DO N = 1,NHAR
          A(N) = 0.0D0
          B(N) = 0.0D0
      END DO

      CALL DEZFFTI(NPTS,WRK)
      CALL DEZFFTF(NPTS,X,ANOT,A,B,WRK)

      VARTOT = 0.D0
      DO N = 1,NHAR
          AMP(N) = SQRT(A(N)*A(N)+B(N)*B(N))

          IF (A(N).EQ.0.D0 .AND. B(N).EQ.0.D0) THEN
              PHASE(N) = 0.D0
          ELSE
              PHASE(N) = ATAN2(B(N),A(N))*W/DBLE(N)
              IF (PHASE(N).LT.0.0D0) THEN
                  IF (ABS(PHASE(N)).LE.EPS) THEN
                      PHASE(N) = 0.0D0
                  ELSE
                      PHASE(N) = PHASE(N) + DBLE(NPTS)/DBLE(N)
                  END IF
              END IF
          END IF

          IF (AMP(N).LT.EPS) THEN
              AMP(N) = 0.0D0
              PHASE(N) = 0.0D0
          END IF

          VARTOT = VARTOT + 0.5D0*AMP(N)*AMP(N)
          PCVAR(N) = 0.D0
      END DO


      IF (VARTOT.GT.EPS) THEN
          DO N = 1,NHAR
              PCVAR(N) = ((0.5D0*AMP(N)*AMP(N))/VARTOT)*100.D0
              IF (PCVAR(N).LT.EPS) PCVAR(N) = 0.0D0
          END DO
      ELSE
          DO N = 1,NHAR
              AMP(N) = 0.D0
              PHASE(N) = 0.D0
              PCVAR(N) = 0.D0
          END DO
      END IF

      RETURN
      END
