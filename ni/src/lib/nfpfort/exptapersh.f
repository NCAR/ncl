C NCLFORTSTART
      SUBROUTINE DEXPTAPER(N0,R,S,NLAT,JER)
      IMPLICIT NONE
c                                input
      INTEGER R,NLAT,JER
      DOUBLE PRECISION N0
c                                output [ALLOCATE IN INTERFACE]
      DOUBLE PRECISION S(NLAT)
C NCLEND

c NCL: wgt = exp_taper (n0, r)

      INTEGER N
      DOUBLE PRECISION CON

      JER = 0
      IF (R.LE.0) THEN
          JER = 1
          PRINT *,'DEXPTAPER: r must be > 0:  r=',R
      END IF
      IF (N0.LE.0.D0) THEN
          JER = 10
          PRINT *,'DEXPTAPER: no must be > 0:  n0=',N0
      END IF
      IF (JER.NE.0) RETURN
c
c equation 9
c
      CON = N0* (N0+1.D0)
      DO N = 1,NLAT
          S(N) = EXP(- (N* (N+1)/CON)**R)
      END DO
c now wgt the coef by the wgts
c
      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DEXPTAPERSH(MB,NB,AB,BB,N0,R,JER)
      IMPLICIT NONE
      INTEGER MB,NB,R,JER
      DOUBLE PRECISION AB(MB,NB),BB(MB,NB),N0
C NCLEND

c NCL: exp_tapersh (a,b, n0, r)
c      ab = exp_tapershC (ab, n0, r)

c     perform an exponential taper on the spherical harmonic coef

      INTEGER M,N,KER
      DOUBLE PRECISION S(2*NB)

      JER = 0
      IF ((N0+2).GT.NB) THEN
          JER = 8
          WRITE (*,FMT=
     +'(''SUB DEXPTAPERSH: N0 too large: ''
     +   ,''N0='',f8.0,3x,''iwave,nb='',2i5)') N0,R,NB
      ELSE IF (NB.NE.MB) THEN
          JER = 9
          WRITE (*,FMT=
     +'(''SUB DEXPTAPERSH: nb .ne. mb: ''
     +   ,''nb,mb='',2i5)') NB,MB
      END IF
      IF (JER.NE.0) RETURN
c equation 9
c
      CALL DEXPTAPER(N0,R,S,NB,KER)
c now wgt the coef by the wgts
c
      DO N = 1,NB
          DO M = 1,N
              AB(M,N) = AB(M,N)*S(N)
              BB(M,N) = BB(M,N)*S(N)
          END DO
      END DO

      RETURN
      END
