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

      SUBROUTINE DTRITRUNC(NLAT2,T,NLAT,A,B)

      IMPLICIT NONE
      INTEGER NLAT,T,NLAT2
      DOUBLE PRECISION A(NLAT,NLAT2),B(NLAT,NLAT2)

      CALL DTRUNC(NLAT2,T,NLAT,A,B)

      RETURN
      END
c ---------------- original interface
      SUBROUTINE DTRUNC(NM,MS,ID,A,B)
      IMPLICIT NONE

      INTEGER NM,MS,ID
      DOUBLE PRECISION A(ID,NM),B(ID,NM)

c c c dimension a(id,1), b(id,1)    .... old fortran

C     Triangular truncation:
C     "Truncates spectral coefficients so that aliasing does not
C     occur when computing the spectral representation of the product
C     terms."
C     Taken from page 54 of Adams, John C., and Paul N. Swarztrauber,
C     1997: Spherepack 2.0: A model development facility. NCAR Technical
C     Note NCAR/TN-436-STR, 62 pp.

c     D Shea made some cosmetic changes

      INTEGER MP,N,M

      MP = MS + 2
      DO N = MP,NM
          DO M = 1,N
              A(M,N) = 0.D0
              B(M,N) = 0.D0
          END DO
      END DO

      RETURN
      END
