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

      DLAT = ABS(GLAT(2)-GLAT(1))
      DLON = ABS(GLON(2)-GLON(1))

      IF ((GLAT(2)-GLAT(1)).GT.0.0D0) THEN
          IFLAG = 1
      ELSE
          IFLAG = -1
      END IF

      GLATBND1 = GLAT(1) - IFLAG*DLAT/2
      GLONBND1 = GLON(1) - DLON/2
      GLATBND2 = GLAT(NLAT) + IFLAG*DLAT/2
      GLONBND2 = GLON(MLON) + DLON/2

      DO K = 1,NZ
          IF (Z(K).NE.ZMSG .AND.
     +       (ZLAT(K).GE.GLATBND1 .AND. ZLAT(K).LE.GLATBND2)  .AND.
     +       (ZLON(K).GE.GLONBND1 .AND. ZLON(K).LE.GLONBND2)) THEN
              NL = ABS((ZLAT(K)-GLATBND1)/DLAT) + 1
              ML = ABS((ZLON(K)-GLONBND1)/DLON) + 1
              IF (ML.GE.1 .AND. ML.LE.MLON .AND. NL.GE.1 .AND.                
     +            NL.LE.NLAT) THEN
                  GBIN(ML,NL) = GBIN(ML,NL) + Z(K)
                  GKNT(ML,NL) = GKNT(ML,NL) + 1
              END IF
          END IF
      END DO

      RETURN
      END
