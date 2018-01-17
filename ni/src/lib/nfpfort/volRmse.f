C NCLFORTSTART
      SUBROUTINE DWGTVOLRMSE(T,Q,WGTZ,WGTY,WGTX,MX,NY,KZ,TMSG,QMSG,
     +                       IFLAG,RMSE)
      IMPLICIT NONE
      INTEGER MX,NY,KZ,IFLAG
      DOUBLE PRECISION T(MX,NY,KZ),Q(MX,NY,KZ)
      DOUBLE PRECISION WGTX(MX),WGTY(NY),WGTZ(KZ)
      DOUBLE PRECISION TMSG,QMSG
      DOUBLE PRECISION RMSE
C NCLEND

C NCL: rmsqTQ = wgtVolRmse (T,Q,wgtz,wgty,wgtx,flag)
C compute the weighted root-mean-square-difference [rmse]
c .   This could have been programmed more efficiently.

      INTEGER NL,ML,KL,KMSG
      DOUBLE PRECISION SUMD,SUMW,WGTXY,SUMTQ,SUMWZ,VARTQ

C Nomenclature:
C INPUT:
C t      - 3D array
C q      - 3D array
C wgtz   - 1D array of length "kz"
C wgty   - 1D array of length "ny"
C wgtx   - 1D array of length "mx"
C mx     - 1st [fastest varying] dimension of "t" [eg, longitude]
C ny     - 2nd                   dimension of "t" [eg, latitude ]
C kz     - 3rd [slowest varying] dimension of "t" [eg, latitude ]
c tmsg   - msg value
c qmsg   - msg value
C iflag  - flag
C          =0 compute  rmse ignoring msg values
C          =1 if any msg data is encountered return as msg

C OUTPUT:
C rmse   - volume root-mean-square-difference [weighted]

      SUMD = 0.0D0
      SUMW = 0.0D0
      RMSE = TMSG
      KMSG = 0

      DO NL = 1,NY
          DO ML = 1,MX
              SUMTQ = 0
              SUMWZ = 0.0D0
c                                     sum wgted vertical differences
c                                     at each lat/lon point
              DO KL = 1,KZ
                  IF (T(ML,NL,KL).NE.TMSG .AND.
     +                Q(ML,NL,KL).NE.QMSG) THEN
                      SUMTQ = SUMTQ + WGTZ(KL)*
     +                        (T(ML,NL,KL)-Q(ML,NL,KL))**2
                      SUMWZ = SUMWZ + WGTZ(KL)
                  ELSE
                      KMSG = KMSG + 1
                  END IF
              END DO
c                                      return if user desired
              IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
c                            wgt vertical mean square difference
c                            by areal wgts x/lon and y/lat
              IF (SUMWZ.GT.0.0D0) THEN
                  VARTQ = (SUMTQ/SUMWZ)
                  WGTXY = WGTX(ML)*WGTY(NL)
                  SUMD  = SUMD + WGTXY*VARTQ
                  SUMW = SUMW + WGTXY
              END IF

          END DO
      END DO
c                          compute wgted rmse
      IF (SUMW.GT.0.0D0) THEN
          RMSE = SQRT(SUMD/SUMW)
      END IF

      RETURN
      END

C NCLFORTSTART
      SUBROUTINE DWGTVOLRMSECCM(T,Q,DPT,DPQ,WGTY,WGTX,MX,NY,KZ,TMSG,
     +                         QMSG,IFLAG,RMSE)
      IMPLICIT NONE
      INTEGER MX,NY,KZ,IFLAG
      DOUBLE PRECISION T(MX,NY,KZ),Q(MX,NY,KZ)
      DOUBLE PRECISION WGTX(MX),WGTY(NY)
      DOUBLE PRECISION DPT(MX,NY,KZ),DPQ(MX,NY,KZ)
      DOUBLE PRECISION TMSG,QMSG
      DOUBLE PRECISION RMSE
C NCLEND

C NCL: rmsqTQ = wgtVolRmse_ccm (T,Q,dpT,dpQ,wgty,wgtx,flag)
C compute the weighted root-mean-square-difference [rmse]
c .   This could have been programmed more efficiently.

      INTEGER NL,ML,KL,KMSG
      DOUBLE PRECISION SUMD,SUMW,WGTXY,SUMTQ,SUMDP,WGTDP,VARTQ

C Nomenclature:
C INPUT:
C t      - 3D array
C q      - 3D array
C dpt    - 3D array same size as "t" and "q"
C dpq    - 3D array same size as "t" and "q"
C wgty   - 1D array of length "ny"
C wgtx   - 1D array of length "mx"
C mx     - 1st [faster varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower varying] dimension of "t" [eg, latitude ]
C kz     - 3rd [slower varying] dimension of "t" [eg, latitude ]
c tmsg   - msg value
c qmsg   - msg value
C iflag  - flag
C          =0 compute  rmse ignoring msg values
C          =1 if any msg data is encountered return as msg

C OUTPUT:
C rmse   - volume root-mean-square-difference [weighted]
C          Dave Williamson suggested the use of the mean "dp"

      SUMD = 0.0D0
      SUMW = 0.0D0
      RMSE = TMSG
      KMSG = 0

      DO NL = 1,NY
          DO ML = 1,MX
              SUMTQ = 0.0D0
              SUMDP = 0.0D0
c                                     sum wgted vertical differences
c                                     at each lat/lon point
              DO KL = 1,KZ
                  IF (T(ML,NL,KL).NE.TMSG .AND.
     +                Q(ML,NL,KL).NE.QMSG) THEN
                      WGTDP = 0.5D0* (DPT(ML,NL,KL)+DPQ(ML,NL,KL))
                      SUMTQ = SUMTQ + WGTDP*(T(ML,NL,KL)-Q(ML,NL,KL))**2
                      SUMDP = SUMDP + WGTDP
                  ELSE
                      KMSG = KMSG + 1
                  END IF
              END DO
c                                      return if user desired
              IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
c                        wgt vertical mean square difference
c                        by areal wgts x/lon and y/lat
              IF (SUMDP.GT.0.0D0) THEN
                  VARTQ = SUMTQ/SUMDP
                  WGTXY = WGTX(ML)*WGTY(NL)
                  SUMD  = SUMD + WGTXY*VARTQ
                  SUMW  = SUMW + WGTXY
              END IF

          END DO
      END DO
c                          compute wgted rmse
      IF (SUMW.GT.0.0D0) THEN
          RMSE = SQRT(SUMD/SUMW)
      END IF

      RETURN
      END
