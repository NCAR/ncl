C NCLFORTSTART
      SUBROUTINE DWGTVOLAVE(T,WGTZ,WGTY,WGTX,MX,NY,KZ,TMSG,IFLAG,AVE)
      IMPLICIT NONE
      INTEGER MX,NY,KZ,IFLAG
      DOUBLE PRECISION T(MX,NY,KZ)
      DOUBLE PRECISION WGTX(MX),WGTY(NY),WGTZ(KZ)
      DOUBLE PRECISION TMSG
      DOUBLE PRECISION AVE
C NCLEND

C NCL: volT = wgtVolAve (T,wgtz,wgty,wgtx,flag)
C compute the weighted volume average
c .   This could have been programmed more efficiently.

      INTEGER NL,ML,KL,KMSG
      DOUBLE PRECISION SUMT,SUMW,WGTXY,SUMTZ,SUMWZ,TAVEZ

C Nomenclature:
C INPUT:
C t      - 3D array
C wgtz   - 1D array of length "kz"
C wgty   - 1D array of length "ny"
C wgtx   - 1D array of length "mx"
C mx     - 1st [fastest varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower  varying] dimension of "t" [eg, latitude ]
C kz     - 3rd [slowest varying] dimension of "t" [eg, level ]
c tmsg   - msg value
C iflag  - flag
C          =0 compute  average ignoring msg values
C          =1 if any msg data is encountered return as msg

C OUTPUT:
C ave    - volume average [returned]

c Note: I could do this in one loop but I did it this way for
c .     consistency with some other subroutines.

      SUMT = 0.0D0
      SUMW = 0.0D0
      KMSG = 0
      AVE = TMSG

      DO NL = 1,NY
          DO ML = 1,MX
              SUMTZ = 0.0D0
              SUMWZ = 0.0D0

              DO KL = 1,KZ
                  IF (T(ML,NL,KL).NE.TMSG) THEN
                      SUMTZ = SUMTZ + T(ML,NL,KL)*WGTZ(KL)
                      SUMWZ = SUMWZ + WGTZ(KL)
                  ELSE
                      KMSG = KMSG + 1
                  END IF
              END DO
c                                      return if user desired
              IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
c                                      wgt vertical average
c                                      by areal wgts x/lon and y/lat
              IF (SUMWZ.GT.0.0D0) THEN
                  TAVEZ = SUMTZ/SUMWZ
                  WGTXY = WGTX(ML)*WGTY(NL)
                  SUMT = SUMT + WGTXY*TAVEZ
                  SUMW = SUMW + WGTXY
              END IF

          END DO
      END DO
c                          compute wgted average
      IF (SUMW.GT.0.0D0) THEN
          AVE = SUMT/SUMW
      END IF

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DWGTVOLAVECCM(T,WGTZ,WGTY,WGTX,MX,NY,KZ,TMSG,IFLAG,AVE)
      IMPLICIT NONE
      INTEGER MX,NY,KZ,IFLAG
      DOUBLE PRECISION T(MX,NY,KZ)
      DOUBLE PRECISION WGTX(MX),WGTY(NY),WGTZ(MX,NY,KZ)
      DOUBLE PRECISION TMSG
      DOUBLE PRECISION AVE
C NCLEND

C NCL: volT = wgtVolAveCcm (T,wgtz,wgty,wgtx,flag)
C compute the weighted volume average
c .   This could have been programmed more efficiently.

      INTEGER NL,ML,KL,KMSG
      DOUBLE PRECISION SUMT,SUMW,WGTXY,SUMTZ,SUMWZ,TAVEZ

C Nomenclature:
C INPUT:
C t      - 3D array
C wgtz   - 3D array [same size as "t"]
c .        This is really designed for "delta p"
C wgty   - 1D array of length "ny"
C wgtx   - 1D array of length "mx"
C mx     - 1st [fastest varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower  varying] dimension of "t" [eg, latitude ]
C kz     - 3rd [slowest varying] dimension of "t" [eg, level ]
c tmsg   - msg value
C iflag  - flag
C          =0 compute  average ignoring msg values
C          =1 if any msg data is encountered return as msg

C OUTPUT:
C ave    - volume average [returned]

      SUMT = 0.0D0
      SUMW = 0.0D0
      KMSG = 0
      AVE = TMSG

      DO NL = 1,NY
          DO ML = 1,MX
              SUMTZ = 0.0D0
              SUMWZ = 0.0D0

              DO KL = 1,KZ
                  IF (T(ML,NL,KL).NE.TMSG) THEN
                      SUMTZ = SUMTZ + T(ML,NL,KL)*WGTZ(ML,NL,KL)
                      SUMWZ = SUMWZ + WGTZ(ML,NL,KL)
                  ELSE
                      KMSG = KMSG + 1
                  END IF
              END DO
c                                      return if user desired
              IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
c                                      wgt vertical average
c                                      by areal wgts x/lon and y/lat
              IF (SUMWZ.GT.0.0D0) THEN
                  TAVEZ = SUMTZ/SUMWZ
                  WGTXY = WGTX(ML)*WGTY(NL)
                  SUMT = SUMT + WGTXY*TAVEZ
                  SUMW = SUMW + WGTXY
              END IF

          END DO
      END DO
c                          compute wgted average
      IF (SUMW.GT.0.0D0) THEN
          AVE = SUMT/SUMW
      END IF

      RETURN
      END
