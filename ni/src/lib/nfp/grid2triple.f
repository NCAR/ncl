C NCLFORTSTART
      SUBROUTINE GRID2TRIPLE(X,Y,Z,MX,NY,D,LDMAX,LD,ZMSG,IER)
      IMPLICIT NONE
c NCL:   d = grid2triple (x,y,z)     ;
c                                               input
      INTEGER MX,NY,LDMAX
      DOUBLE PRECISION X(MX),Y(NY),Z(MX,NY),ZMSG
c                                               output
      INTEGER LD,IER
      DOUBLE PRECISION D(LDMAX,3)
C NCLEND

c nomenclature
c .   x,y   - 1D coord variables dimensioned (mx and ny)
c .   z     - 2D grid containing functional values z(x,y)
c .   mx,ny - length of x and y; dimensions of z(mx,ny)
c .   d     - 2D vector of size (ldmax,3)
c .   ldmax - mx*ny
c .   ld    - actual number of non-missing values
c .   zmsg  - missing value code for z
c .           if no missing values set to some number which
c .           can not be encountered
c .   ier   - eeor flag
c                                               local
      INTEGER M,N

      IER = 0
      LD = 0

      DO N = 1,NY
          DO M = 1,MX
              IF (Z(M,N).NE.ZMSG) THEN
                  LD = LD + 1
                  D(LD,1) = X(M)
                  D(LD,2) = Y(N)
                  D(LD,3) = Z(M,N)
              END IF
          END DO
      END DO

      IF (LD.EQ.0) THEN
          IER = -10
C          PRINT *,'grid2triple: ld=0: all values missing'
      END IF
      RETURN
      END
