c -----------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DRHOMBTRUNC(M,N,AR,AI,R)

C NCL:  ab = rhomb_trunC(ab,R) or procedure:   rhomb_trunc (Ar,Ai,R)

c original code was f90 from Dave Stepaniack
c /home/dataproc/davestep/SPHERE/SUBR_RHOMBOIDAL.f90

C*PL*ERROR* Comment line too long
c Produces rhomboidal truncation R of an MxN array A of spectral coefficients
c (with the restriction that  1 < R < N).

c Example; A is 18x20:
c (Let m represent zonal wavenumber, and n total wavenumber.)

C*PL*ERROR* Comment line too long
c m, n->                                                                         N
c |
c v
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c M 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7

c Example; A truncated at R9:

c    1   2   3   4   5   6   7   8   9

C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

      IMPLICIT NONE

C Extent of zonal wave  number dimension.
      INTEGER M
C Extent of total wave  number dimension.
      INTEGER N
C Rhomboidal truncation parameter. R is the
      INTEGER R
C constant number of total wave numbers to keep.

C Spectral coefficients (real).
      DOUBLE PRECISION AR(M,N)
C Spectral coefficients (real).
      DOUBLE PRECISION AI(M,N)
C NCLEND

C DO loop control variable for zonal wavenumber index.
      INTEGER IM
C Total wavenumber place holder.
      INTEGER JN
C Zonal wavenumber place holder.
      INTEGER KM

C local loop variables
      INTEGER I,J

C Check immediately to determine if R is an element of [1,N]:


C djs
      IF (R.EQ.0) THEN
          WRITE (*,FMT='(''SUB RHOMBOIDAL: R=0 no action taken'')')
          RETURN
      END IF

      IF (.NOT. ((1.LT.R).AND. (R.LT.N))) THEN
c c c IF ( .NOT. ( ( 1 < R )    .AND. ( R < N ) ) ) THEN

          WRITE (*,FMT='(A25,I3,A7,I2)')
     +'R outside of OPEN set (1,
     +',N,'); R = ',R
          WRITE (*,FMT='(A43)')
     +      'Execution stopped in SUBROUTINE DRHOMBOIDAL.'
          RETURN

      END IF

C Now zero out A according to rhomboidal truncation:


      IF (M.GE.N) THEN
c c c IF ( M >= N ) THEN

C Case M greater than or equal to N.


C Loop over zonal wavenumber (i.e. rows).
          DO IM = 1,N - R

              JN = IM

C Zero out above diagonal band of width R.
c c c     A( im, jn+R:N ) = 0.0
              DO J = JN + R,N
                  AR(IM,J) = 0.0D0
                  AI(IM,J) = 0.0D0
              END DO

              KM = IM + 1

C Zero out below diagonal band of width R.
c c c     A( km, 1:jn ) = 0.0
              DO J = 1,JN
                  AR(KM,J) = 0.0D0
                  AI(KM,J) = 0.0D0
              END DO

          END DO

      ELSE IF (M.LT.N) THEN
c c c ELSE IF ( M < N ) THEN

C Case M less than N.


          IM = 0

c c c   DO
          DO WHILE (((IM+R-1).GE.N) .OR. (IM.GT. (M-1)))

              IM = IM + 1
C Loop over zonal wavenumber (i.e. rows).


c c c     IF ( ( (im + R - 1) >= N ) .OR. ( im > (M - 1) ) ) EXIT

              JN = IM

C Zero out above diagonal band of width R.
c c c     A( im, jn+R:N ) = 0.0
              DO J = JN + R,N
                  AR(IM,J) = 0.0D0
                  AI(IM,J) = 0.0D0
              END DO


              KM = IM + 1

C Zero out below diagonal band of width R.
c c c     A( km, 1:jn ) = 0.0
              DO J = 1,JN
                  AR(KM,J) = 0.0D0
                  AI(KM,J) = 0.0D0
              END DO

          END DO

          IF ((JN+R+1).LE.N) THEN
c c c   IF ( (jn+R+1) <= N ) THEN

C Zero out last row ABOVE diagonal.
c c c     A( km, jn+R+1:N ) = 0.0
              DO J = JN + R + 1,N
                  AR(KM,J) = 0.0D0
                  AI(KM,J) = 0.0D0
              END DO

          END IF

      END IF

      IF ((KM+1).LE.M) THEN
c c c IF ( (km + 1) <= M ) THEN
C Zero out any remaining rows (if any).
c c c   A(km+1:M,:) = 0.
          DO I = KM + 1,M
              DO J = 1,N
                  AR(I,J) = 0.0D0
                  AI(I,J) = 0.0D0
              END DO
          END DO
      END IF

      RETURN
      END
c     END SUBROUTINE DRHOMBOIDAL
c ---------------------------------------------------------
      SUBROUTINE DTRITRUNC(M,N,AR,AI,T)

c NCL: ab = tri_trunc (ab, T)

C*PL*ERROR* Comment line too long
c Produces triangular truncation T of MxN spectral coefficient arrays Ar and Ai
C*PL*ERROR* Comment line too long
c (with the restriction that  1 < T < N-1 if M >=N  or 1 < T < M-1 if M < N).
c                               -   -                    -   -
C*PL*ERROR* Comment line too long
c Ar contains the real part and Ai the corresponding imaginary part of the
c spectral coefficients.

c Convention:

c Sectral coefficients Ar, Ai, are ordered m by n where
c m is the zonal wavenumber and n is the total wave number.
c For Gaussian grids the extents of the m and n dimensions
c are nlat for both dimensions.

c The spectral coefficent Ar(1,1) repesents zonal wavenumber
c 0 and total wavenumber 0; the spectral coefficient
c Ar(nlat,nlat) represents zonal wavenumber nlat-1 and
c total wavenumber nlat-1. Similarly for Ai.

c Example; Ar and Ai are 18x18:
c (Let m represent zonal wavenumber, and n total wavenumber.)

C*PL*ERROR* Comment line too long
c m, n->                                                                  N
c |
c v
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
C*PL*ERROR* Comment line too long
c M 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7

c Example; Ar and Ai truncated at T12:

c   (0)  1   2   3   4   5   6   7   8   9  10  11  12

C*PL*ERROR* Comment line too long
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
C*PL*ERROR* Comment line too long
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

      IMPLICIT NONE

C Extent of zonal wave number dimension.
      INTEGER M
C Extent of total wave number dimension.
      INTEGER N
C Real spectral coefficients.
      DOUBLE PRECISION AR(M,N)
C Real spectral coefficients.
      DOUBLE PRECISION AI(M,N)
C Triangular truncation parameter.
      INTEGER T
C NCLEND

C DO loop control variable for zonal wavenumber index.
      INTEGER IM
C Total wavenumber place holder.
      INTEGER JN
C Zonal wavenumber place holder.
      INTEGER KM
C Loop variables
      INTEGER I,J

C djs
      IF (T.EQ.0) THEN
          WRITE (*,FMT='(''SUB TRIANGULAR: T=0 no action taken'')')
          RETURN
      END IF

C Check immediately to determine if T is a valid truncation:


      IF (M.GE.N) THEN
c c c IF ( M >= N ) THEN

          IF (.NOT. ((1.LE.T).AND. (T.LE.N-1))) THEN
c c c   IF ( .NOT. ( ( 1 <= T ) .AND. ( T <= N-1 ) ) ) THEN
              WRITE (*,FMT='(A25)') 'Error for case M >= N :'
              WRITE (*,FMT='(A28,I3)') 'T not in range [1,N-1]; T = ',T
              WRITE (*,FMT='(A43)')
     +          'Exec stop in SUBROUTINE DTRIANGULAR'
          END IF

      ELSE IF (M.LT.N) THEN
c c c ELSE IF ( M < N ) THEN

          IF (.NOT. ((1.LE.T).AND. (T.LE.M-1))) THEN
c c c   IF ( .NOT. ( ( 1 <= T ) .AND. ( T <= M-1 ) ) ) THEN
              WRITE (*,FMT='(A25)') 'Error for case M < N :'
              WRITE (*,FMT='(A28,I3)') 'T not in range [1,N-1]; T = ',T
              WRITE (*,FMT='(A43)')
     +          'Exec stop in SUBROUTINE DTRIANGULAR'
              RETURN

          END IF

      END IF

C Now zero out Ar and Ai according to triangular truncation:


      IF (M.LE.N) THEN
C Case M less than or equal to N.
c c c IF ( M <= N ) THEN

C Loop over row index.
          DO IM = 1,M - 1

              JN = IM
              KM = IM + 1

C Zero out elements below diagonal.
c c c   Ar(km,1:jn) = 0.
c c c   Ai(km,1:jn) = 0.
              DO J = 1,JN
                  AR(KM,J) = 0.0D0
                  AI(KM,J) = 0.0D0
              END DO

          END DO

      ELSE IF (M.GT.N) THEN
c c c ELSE IF ( M > N ) THEN

C Loop over row index.
          DO IM = 1,N - 1

              JN = IM
              KM = IM + 1

C Zero out elements below diagonal.
c c c   Ar(km,1:jn) = 0.
c c c   Ai(km,1:jn) = 0.
              DO J = 1,JN
                  AR(KM,J) = 0.0D0
                  AI(KM,J) = 0.0D0
              END DO
          END DO


C (Partially) zero out remaining rows
c c c   Ar(km+1:M,1:jn+1) = 0.
c c c   Ai(km+1:M,1:jn+1) = 0.
          DO J = 1,JN + 1
              DO I = KM + 1,M
                  AR(I,J) = 0.0D0
                  AI(I,J) = 0.0D0
              END DO
          END DO

      END IF

      IF ((T+2).LE.N) THEN
C Truncate columns beyond T+1 (if necessary).
c c c IF ( (T + 2) <= N ) THEN

c c c   Ar(:,T+2:N) = 0.
c c c   Ai(:,T+2:N) = 0.
          DO J = T + 2,N
              DO I = 1,M
                  AR(I,J) = 0.0D0
                  AI(I,J) = 0.0D0
              END DO
          END DO
      END IF

      RETURN
      END
