C NCLFORTSTART
      SUBROUTINE DWGTAREASUM(T,WGTY,WGTX,MX,NY,TMSG,IFLAG,SUM)
      IMPLICIT NONE
      INTEGER MX,NY,IFLAG
      DOUBLE PRECISION T(MX,NY),WGTX(MX),WGTY(NY)
      DOUBLE PRECISION TMSG
      DOUBLE PRECISION SUM
C NCLEND

C NCL: sumX = wgt_areasum (x,wgty,wgtx,flag)

      INTEGER NL,ML,KMSG,KOK
      DOUBLE PRECISION SUMT,SUMW,WGT

C compute the weighted area sum

C Nomenclature:
C INPUT:
C t      - 2D array
C wgty   - 1D array of length "ny" [eg: cos(0.01745*lat) or gau_wgt(:)]
C wgtx   - 1D array of length "mx"
C mx     - 1st [faster varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower varying] dimension of "t" [eg, latitude ]
C tmsg   - msg value
C iflag  - flag
C          =0 compute  sum ignoring msg values
C          =1 if any msg data is encountered return as msg
c OUTPUT:
C sum    - area weighted sum

      SUMT = 0.0D0
      SUMW = 0.0D0
      KMSG = 0
      KOK  = 0
      SUM = TMSG

      DO NL = 1,NY
          DO ML = 1,MX
              IF (T(ML,NL).NE.TMSG) THEN
                  KOK = KOK + 1
                  WGT = WGTX(ML)*WGTY(NL)
                  SUMT = SUMT + T(ML,NL)*WGT
              ELSE
                  KMSG = KMSG + 1
              END IF
          END DO
c                                      return if user desired
          IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
      END DO
c
      IF (KOK.GT.0) THEN
          SUM = SUMT
      END IF

      RETURN
      END
