C NCLFORTSTART 
      SUBROUTINE BINDATAAVG(NZ,ZLON,ZLAT,Z,ZMSG,MLON,NLAT,GLON,GLAT 
     +                     ,GBINKNT,IOPT,IER)
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
      DOUBLE PRECISION DLAT,DLON
      DOUBLE PRECISION GLATBND1,GLATBND2
      DOUBLE PRECISION GLONBND1,GLONBND2

      IER = 0

c                            ; monotonically {in/de}creasing
      DLAT = GLAT(2)-GLAT(1)
      IF (DLAT.GT.0.0D0) THEN
          IFLAG = 1
      ELSEIF (DLAT.LT.0.0D0) THEN
          IFLAG = -1
      ELSE
          IER = 1
      END IF
      DLAT = ABS(DLAT)

c                            check for equal lat spacing
      DO NL = 1,NLAT - 1
          IF (ABS(GLAT(NL+1)-GLAT(NL)).NE.DLAT) THEN
              IER = IER + 10
              GO TO 10
          END IF
      END DO
   10 CONTINUE

c                            check for equal lon spacing
      DLON = GLON(2)-GLON(1)
      IF (DLON.LE.0.0D0) THEN
          IER = 1
      END IF

      DO ML = 1,MLON - 1
          IF (ABS(GLON(ML+1)-GLON(ML)).NE.DLON) THEN
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

      GLATBND1 = MIN(GLAT(1),GLAT(NLAT)) - DLAT/2
      GLATBND2 = MAX(GLAT(1),GLAT(NLAT)) + DLAT/2
      GLONBND1 = GLON(1)    - DLON/2
      GLONBND2 = GLON(MLON) + DLON/2

      DO K = 1,NZ
          IF (Z(K).NE.ZMSG .AND.
     +       (ZLAT(K).GE.GLATBND1 .AND. ZLAT(K).LE.GLATBND2)  .AND.
     +       (ZLON(K).GE.GLONBND1 .AND. ZLON(K).LE.GLONBND2)) THEN
              ML = ABS((ZLON(K)-GLONBND1)/DLON) + 1
              NL = ABS((ZLAT(K)-GLATBND1)/DLAT) + 1
              IF (IFLAG.EQ.-1) NL = NLAT-NL+1
c c c         IF (NL.GT.0 .AND. NL.LE.NLAT .AND. ML.GT.0 .AND.
c c c             ML.LE.MLON) THEN
                  GBINKNT(ML,NL,1) = GBINKNT(ML,NL,1) + Z(K)
                  GBINKNT(ML,NL,2) = GBINKNT(ML,NL,2) + 1
c c c         END IF
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
