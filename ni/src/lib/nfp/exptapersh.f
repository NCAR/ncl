c -----------------------------------------------------
      SUBROUTINE DEXPTAPERSH(MB,NB,AB,BB,IWAVE,NOPTION,JER)

c NCL: exp_tapersh (a,b,iwave,n_option)
c      ab = exp_tapershC (ab, iwave, n_option)

c Mary in the interface set noption to 10

c     perform an exponential taper on the spherical harmonic coef
c     initially nOption should always be set to 10

      IMPLICIT NONE
      INTEGER MB,NB,IWAVE,NOPTION,JER
      DOUBLE PRECISION AB(MB,NB),BB(MB,NB)

      INTEGER M,N,J,JP,JW,NTWGT,NO
      PARAMETER (NTWGT=999)
      DOUBLE PRECISION TWGT(NTWGT),CON

      JER = 0

      IF ((IWAVE+2).GT.NB) THEN
          JER = 8
          WRITE (*,FMT=
     +'(''SUB EXPTAPERSH: iwave too large: ''
     +   ,''iwave,nb='',2i5)') IWAVE,NB
      ELSE IF (NB.NE.MB) THEN
          JER = 9
          WRITE (*,FMT=
     +'(''SUB EXPTAPERSH: nb .ne. mb: ''
     +   ,''nb,mb='',2i5)') NB,MB
      END IF
      IF (JER.NE.0) RETURN

      NO = NOPTION

C perform exponential tapering

C exponent; determines fall-off rate
      JP = MAX0(IWAVE/NO,1)
C coef which has wgt=exp(-1)
      JW = JP*NO


      IF ((IWAVE+1).GT.NTWGT) THEN
          WRITE (*,FMT=
     +'(''SUB EXPTAPERSH: ntwgt exceeded=''
     +   ,2i5)') NTWGT, (IWAVE+1)
          RETURN
      END IF

      CON = 1.D0/DBLE(JW* (JW+1))
      DO J = 0,IWAVE
          TWGT(J+1) = EXP(- (DBLE(J* (J+1))*CON)**JP)
      END DO
C now wgt the coef by the wgts

C traverse the diagonal
      DO N = IWAVE + 1,1,-1
          DO M = 1,N
              AB(M,N) = AB(M,N)*TWGT(N)
              BB(M,N) = BB(M,N)*TWGT(N)
          END DO
      END DO

      RETURN
      END
