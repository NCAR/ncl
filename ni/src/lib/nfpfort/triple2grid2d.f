C NCLFORTSTART
      SUBROUTINE TRIPLE2GRID2D(X,Y,Z,KPTS,ZMSG,DISTMX,MOPT,LAT2D,LON2D,
     +                         ZGRID,NDIM,MDIM)
      IMPLICIT NONE
      INTEGER KPTS,NDIM,MDIM,MOPT
      DOUBLE PRECISION X(KPTS),Y(KPTS),Z(KPTS),ZMSG,DISTMX
      DOUBLE PRECISION LAT2D(MDIM,NDIM),LON2D(MDIM,NDIM),
     +                 ZGRID(MDIM,NDIM)
C NCLEND

C untested

c          SET EACH GRID POINT TO THE NEAREST OBSERVATION.
c .   The user can set distmx to some value. Setting to
c .   some very large value basically means that all grid
c .   points will be filled. Otherwise, only observations within
c .   distmx will be used.
c
c mopt   option for which distance formula to be used
c .      =  0 ; use pythag approx to calculate distance
c .             (quick/less-general]
c .      =  1 ; use great circle formula to calculate distance (slower)
c
c distmx any x/y observation .le. distmx will be used.
c .      Set to some very large number [ 1.e20 ] if all grid points
c .      are to be returned with some value.
c .      Observations .gt. distmx will be ignored. If distmx is set
c .        small than it is possible that some grid points will be 
c .        filled with missing values.

C NCL    zgrid = triple2grid2d (x,y,z, lon2d,lat2d, opt)

c                      local: re: rad earth (km) [only for mopt=1]
      INTEGER N,M,K
      DOUBLE PRECISION DIST(KPTS),DMIN,RE,RLAT,RAD,ATMP
      DATA RE/6371.2200D0/

      RAD = 4.D0*ATAN(1.0D0)/180.D0

      DO N = 1,NDIM
         DO M = 1,MDIM
            ZGRID(M,N) = ZMSG
c           do separately for optimization reasons
            IF (MOPT.EQ.0) THEN
               DO K = 1,KPTS
                  DIST(K) = SQRT((X(K)-LON2D(M,N))**2+
     +                 (Y(K)-LAT2D(M,N))**2)
               END DO
            ELSE
               RLAT = LAT2D(M,N)*RAD
               DO K = 1,KPTS
C
C The ATMP variable is necessary to make sure the
C value passed to ACOS is between -1 and 1.
C (Otherwise, you might get "NaN".)
C
                  ATMP = SIN(RLAT)*SIN(Y(K)*RAD)
     +                   + COS(RLAT)*COS(Y(K)*RAD)*
     +                     COS((X(K)-LON2D(M,N))*RAD)
                  ATMP  = MIN(1.D0,MAX(-1.D0,ATMP))
                  DIST(K) = ACOS(ATMP)*RE
               END DO
            END IF
c     assign z(k) to nearest grid point
            DMIN = 1.D+36
            DO K = 1,KPTS
               IF (Z(K).NE.ZMSG) THEN
                  IF (DIST(K).LT.DMIN .AND. DIST(K).LT.DISTMX) THEN
                     DMIN = DIST(K)
                     ZGRID(M,N) = Z(K)
                  END IF
               END IF
            END DO

         END DO
      END DO

      RETURN
      END
