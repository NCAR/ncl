c ---------------- interface nomenclature for clariTY
      SUBROUTINE DTRITRUNCNEW(NLAT2,T,NLAT,A,B)

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
