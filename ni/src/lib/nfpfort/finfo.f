C NCLFORTSTART
      SUBROUTINE DFOURINFO(X,NPTS,NHRET,NHAR,SCLPHA,AMP,PHA,PCV,A,B,
     +     AMPX,PHAX,PCVX,WRK,LWRK)
      IMPLICIT NONE

C NCL:   finfo = fourier_info(x, nhret, sclpha)

c perform a fourier analysis on a series of data points
c return the amplitude, phase and % variance explained

c input
      INTEGER NPTS,NHRET
      DOUBLE PRECISION X(NPTS),SCLPHA
c output
      DOUBLE PRECISION AMP(NHRET),PHA(NHRET),PCV(NHRET)
      INTEGER LWRK,NHAR
      DOUBLE PRECISION WRK(LWRK)
      DOUBLE PRECISION A(*), B(*), AMPX(*),PHAX(*),PCVX(*)
C NCLEND

c local
      INTEGER NH,NHX
      DOUBLE PRECISION ANOT,SCALE
C      DOUBLE PRECISION WRK(3*NPTS+15)
C      DOUBLE PRECISION A(NPTS/2),B(NPTS/2)
C      DOUBLE PRECISION AMPX(NPTS/2),PHAX(NPTS/2),PCVX(NPTS/2)

C      NHAR = NPTS/2
C      LWRK = 3*NPTS + 15

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
      DOUBLE PRECISION ANOT,A(*),B(*),AMP(*),PHASE(*),PCVAR(*)
      DOUBLE PRECISION WRK(LWRK)
c local
      INTEGER N
      DOUBLE PRECISION W,VARTOT,EPS

      W   = DBLE(NPTS)/ (8.D0*ATAN(1.D0))
      EPS = 1.D-12

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
                  PHASE(N) = PHASE(N) + DBLE(NPTS)/DBLE(N)
              END IF
          END IF

          VARTOT = VARTOT + 0.5D0*AMP(N)*AMP(N)
          PCVAR(N) = 0.D0
      END DO


      IF (VARTOT.GT.EPS) THEN
          DO N = 1,NHAR
              PCVAR(N) = ((0.5D0*AMP(N)*AMP(N))/VARTOT)*100.D0
          END DO
      ELSE
         DO N = 1,NHAR
            AMP(N)   = 0.0D0
            PHASE(N) = 0.0D0
            PCVAR(N) = 0.0D0
         END DO
      END IF

      RETURN
      END
