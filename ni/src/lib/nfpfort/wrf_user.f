c----------------------------------------------

C NCLFORTSTART
      SUBROUTINE DGETIJLATLONG(LAT_ARRAY,LONG_ARRAY,LAT,LONGITUDE,
     +                         II,JJ,NX,NY)
      IMPLICIT NONE
      INTEGER NX,NY,II,JJ
      DOUBLE PRECISION LAT_ARRAY(NX,NY),LONG_ARRAY(NX,NY)
      DOUBLE PRECISION LAT,LONGITUDE
C NCLEND
      DOUBLE PRECISION LONGD,LATD
      INTEGER I,J
      DOUBLE PRECISION IR,JR
      DOUBLE PRECISION DIST_MIN,DIST

C Init to missing
      IR = -999
      JR = -999    

      DIST_MIN = 1.D+20
      DO J = 1,NY
          DO I = 1,NX
              LATD = (LAT_ARRAY(I,J)-LAT)**2
              LONGD = (LONG_ARRAY(I,J)-LONGITUDE)**2
C             LONGD = DMIN1((LONG_ARRAY(I,J)-LONGITUDE)**2,
C    +                (LONG_ARRAY(I,J)+LONGITUDE)**2)
              DIST = SQRT(LATD+LONGD)
              IF (DIST_MIN.GT.DIST) THEN
                  DIST_MIN = DIST
                  IR = DBLE(I)
                  JR = DBLE(J)
              END IF
          END DO
      END DO
C
C The original version of this routine returned IR and JR. But, then
C the NCL script that called this routine was converting IR and JR
C to integer, so why not just return II and JJ?
C
C Also, I'm subtracing 1 here, because it will be returned to NCL
C script which has 0-based indexing.
C 
      IF(IR.ne.-999.and.JR.ne.-999) then
        II = NINT(IR)-1
        JJ = NINT(JR)-1
      ELSE
        II = -999
        JJ = -999
      END IF

c we will just return the nearest point at present

      RETURN
      END

