C NCLFORTSTART
      SUBROUTINE DWGTAREARMSE2(T,Q,WGT,MX,NY,TMSG,QMSG,IFLAG,RMSE)
      IMPLICIT NONE
      INTEGER MX,NY,IFLAG
      DOUBLE PRECISION T(MX,NY),Q(MX,NY),WGT(MX,NY)
      DOUBLE PRECISION TMSG,QMSG
      DOUBLE PRECISION RMSE
C NCLEND

C NCL: rmseTQ = wgt_AreaRmse2 (t,q,wgt,flag)

      INTEGER NL,ML,KMSG
      DOUBLE PRECISION SUMD,SUMW

C compute the weighted root-mean-square-difference [rmse]

C Nomenclature:
C INPUT:
C t      - 2D array
C q      - 2D array
C wgty   - 1D array of length "ny" [eg: cos(0.01745*lat) or gau_wgt(:)]
C wgtx   - 1D array of length "mx"
C mx     - 1st [faster varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower varying] dimension of "t" [eg, latitude ]
C tmsg   - msg value
C iflag  - flag
C          =0 compute  rmse ignoring msg values
C          =1 if any msg data is encountered return as msg
c OUTPUT:
C rmse   - root-mean-square-difference [weighted]

      SUMD = 0.0D0
      SUMW = 0.0D0
      KMSG = 0
      RMSE = TMSG

      DO NL = 1,NY
          DO ML = 1,MX
              IF (T(ML,NL).NE.TMSG .AND. Q(ML,NL).NE.QMSG) THEN
                  SUMD = SUMD + WGT(ML,NL)* (T(ML,NL)-Q(ML,NL))**2
                  SUMW = SUMW + WGT(ML,NL)
              ELSE
                  KMSG = KMSG + 1
              END IF
          END DO
c                                      return if user desired
          IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
      END DO
c                          compute wgted differense
      IF (SUMW.NE.0.D0) THEN
          RMSE = SQRT(SUMD/SUMW)
      END IF

      RETURN
      END
