C NCLFORTSTART
      SUBROUTINE DWGTVOLAVE(T,WGTZ,WGTY,WGTX,MX,NY,KZ,TMSG,IFLAG,AVE)
      INTEGER MX,NY,KZ
      DOUBLE PRECISION T(MX,NY,KZ)
      DOUBLE PRECISION WGTX(MX),WGTY(NY),WGTZ(KZ)
      DOUBLE PRECISION TMSG
      DOUBLE PRECISION AVE
C NCLEND

C NCL: volT = wgtVolAve (T,wgtz,wgty,wgtx,flag)
C compute the weighted volume average

      INTEGER NL,ML
      DOUBLE PRECISION SUMT,SUMW,WGT

C Nomenclature:
C INPUT:
C t      - 3D array
C wgtz   - 1D array of length "kz"
C wgty   - 1D array of length "ny"
C wgtx   - 1D array of length "mx"
C mx     - 1st [faster varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower varying] dimension of "t" [eg, latitude ]
C kz     - 3rd [slower varying] dimension of "t" [eg, latitude ]
c tmsg   - msg value
C iflag  - flag
C          =0 compute  average ignoring msg values
C          =1 if any msg data is encountered return as msg

C OUTPUT:
C ave    - volume average [returned]

      SUMT = 0.0D0
      SUMW = 0.0D0
      AVE = TMSG

      IF (IFLAG.EQ.1) THEN
          DO KL = 1,KZ
              DO NL = 1,NY
                  DO ML = 1,MX
                      IF (T(ML,NL,KL).NE.TMSG) THEN
                          WGT = WGTX(ML)*WGTY(NL)*WGTZ(KL)
                          SUMT = SUMT + T(ML,NL,KL)*WGT
                          SUMW = SUMW + WGT
                      ELSE
                          RETURN
                      END IF
                  END DO
              END DO
          END DO
      ELSE
          DO KL = 1,KZ
              DO NL = 1,NY
                  DO ML = 1,MX
                      IF (T(ML,NL,KL).NE.TMSG) THEN
                          WGT = WGTX(ML)*WGTY(NL)*WGTZ(KL)
                          SUMT = SUMT + T(ML,NL,KL)*WGT
                          SUMW = SUMW + WGT
                      END IF
                  END DO
              END DO
          END DO
      END IF
c                          compute wgted average
      IF (SUMW.NE.0.D0) THEN
          AVE = SUMT/SUMW
      END IF

      RETURN
      END
