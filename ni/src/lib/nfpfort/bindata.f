C NCLFORTSTART
      SUBROUTINE BINDATASUM3(MLON,NLAT,GBIN,GKNT,GLON,GLAT,NZ,ZLON,ZLAT,
     +                       Z,ZMSG)
      IMPLICIT NONE
      INTEGER NLAT,MLON,NZ
      DOUBLE PRECISION GLON(MLON),GLAT(NLAT)
      DOUBLE PRECISION ZLON(NZ),ZLAT(NZ),Z(NZ),ZMSG

      DOUBLE PRECISION GBIN(MLON,NLAT)
      INTEGER GKNT(MLON,NLAT)
C NCLEND

C
C NCL: bin_sum_util(gbin[*][*]:numeric,gknt[*][*]:integer,glat[*]:numeric
C                  ,glon[*]:numeric,zlon[*]:numeric,zlat[*]:numeric
C                  ,z[*]:numeric)

      INTEGER K,NL,ML,IFLAG
      DOUBLE PRECISION DLAT,DLON
      DOUBLE PRECISION GLATBND1,GLATBND2
      DOUBLE PRECISION GLONBND1,GLONBND2

      DLON = GLON(2)-GLON(1)
      IF (DLON.LE.0.0D0) THEN
          PRINT *, "BINDATASUM3: longitudes must be monotonically"
          PRINT *, " increasing: FATAL ERROR"
          STOP
      END IF

      DLAT = GLAT(2)-GLAT(1)
      IF (DLAT.GT.0.0D0) THEN
          IFLAG = 1
      ELSEIF (DLAT.LT.0.0D0) THEN
          IFLAG = -1
      END IF
      DLAT = ABS(DLAT)

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

c c c         IF (ML.GE.1 .AND. ML.LE.MLON .AND. NL.GE.1 .AND.
c c c             NL.LE.NLAT) THEN
                  GBIN(ML,NL) = GBIN(ML,NL) + Z(K)
                  GKNT(ML,NL) = GKNT(ML,NL) + 1
c c c         END IF

          END IF
      END DO

      RETURN
      END
