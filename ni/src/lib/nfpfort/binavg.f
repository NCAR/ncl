C NCLFORTSTART
      SUBROUTINE BINDATAAVG(NZ,ZLON,ZLAT,Z,ZMSG,MLON,NLAT,GLON,GLAT,
     +                      GBINKNT,IOPT,IER)
      IMPLICIT NONE
c                                             ; INPUT
      INTEGER NLAT,MLON,NZ,IOPT,IER
      DOUBLE PRECISION ZLON(NZ),ZLAT(NZ),Z(NZ),ZMSG
      DOUBLE PRECISION GLON(MLON),GLAT(NLAT)
c                                             ; OUTPUT
      DOUBLE PRECISION GBINKNT(MLON,NLAT,2)
C NCLEND

c NCL:   function bin_avg(zlon[*],zlat[*],z[*], glon[*], glat[*], opt )

      INTEGER K,NL,ML,IFLAG
      DOUBLE PRECISION DLAT,DLON,GLATBND,GLONBND

      IER = 0

      DLAT = ABS(GLAT(2)-GLAT(1))
      DLON = ABS(GLON(2)-GLON(1))

c error checking
      IF (DLAT.LT.0.0D0 .OR. DLON.LT.0.0D0) THEN
          IER = 1
      END IF
c                            check for equal lat spacing
      DO NL = 1,NLAT - 1
          IF (ABS(GLAT(NL+1)-GLAT(NL)).NE.DLAT) THEN
              IER = IER + 10
              GO TO 10
          END IF
      END DO
   10 CONTINUE
c                            check for equal lon spacing
      DO ML = 1,MLON - 1
          IF (ABS(GLON(2)-GLON(1)).NE.DLON) THEN
              IER = IER + 100
              GO TO 20
          END IF
      END DO
   20 CONTINUE
c                            set all msg if error
      IF (IER.NE.0) THEN
          DO NL = 1,NLAT
              DO ML = 1,MLON
                  GBINKNT(ML,NL,1) = ZMSG
                  GBINKNT(ML,NL,2) = ZMSG
              END DO
          END DO
          RETURN
      END IF
c                            initialize
      DO NL = 1,NLAT
          DO ML = 1,MLON
              GBINKNT(ML,NL,1) = 0.0D0
              GBINKNT(ML,NL,2) = 0.0D0
          END DO
      END DO
c                            ; monotonically {in/de}creasing
      IF ((GLAT(2)-GLAT(1)).GT.0.0D0) THEN
          IFLAG = 1
      ELSE
          IFLAG = -1
      END IF
      GLATBND = GLAT(1) - IFLAG*DLAT
      GLONBND = GLON(1) - DLON/2

      DO K = 1,NZ
          IF (Z(K).NE.ZMSG) THEN
              NL = ABS((ZLAT(K)-GLATBND)/DLAT) + 1
              ML = (ZLON(K)-GLONBND)/DLON + 1
              IF (NL.GT.0 .AND. NL.LE.NLAT .AND. ML.GT.0 .AND.
     +            ML.LE.MLON) THEN
                  GBINKNT(ML,NL,1) = GBINKNT(ML,NL,1) + Z(K)
                  GBINKNT(ML,NL,2) = GBINKNT(ML,NL,2) + 1
              END IF
          END IF
      END DO
c                                     compute bin average
      DO NL = 1,NLAT
          DO ML = 1,MLON
              IF (GBINKNT(ML,NL,2).GT.0.0D0) THEN
                  GBINKNT(ML,NL,1) = GBINKNT(ML,NL,1)/GBINKNT(ML,NL,2)
              ELSE
                  GBINKNT(ML,NL,1) = ZMSG
              END IF
          END DO
      END DO

      RETURN
      END
