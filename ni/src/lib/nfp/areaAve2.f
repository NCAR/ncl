C NCLFORTSTART
      SUBROUTINE DWGTAREAAVE2(T,WGT,MX,NY,TMSG,IFLAG,AVE)
      IMPLICIT NONE
      INTEGER MX,NY,IFLAG
      DOUBLE PRECISION T(MX,NY),WGT(MX,NY)
      DOUBLE PRECISION TMSG
      DOUBLE PRECISION AVE
C NCLEND

C NCL: aveX = wgt_AreaAve2(x,wgt,flag)

      INTEGER NL,ML,KMSG
      DOUBLE PRECISION SUMT,SUMW

C compute the weighted area average

C Nomenclature:
C INPUT:
C t      - 2D array
C wgt    - 2D array
C mx     - 1st [faster varying] dimension of "t" [eg, longitude]
C ny     - 2nd [slower varying] dimension of "t" [eg, latitude ]
C tmsg   - msg value
C iflag  - flag
C          =0 compute  average ignoring msg values
C          =1 if any msg data is encountered return as msg
c OUTPUT:
C ave    - area average

      SUMT = 0.0D0
      SUMW = 0.0D0
      KMSG = 0
      AVE = TMSG

      DO NL = 1,NY
          DO ML = 1,MX
              IF (T(ML,NL).NE.TMSG) THEN
                  SUMT = SUMT + T(ML,NL)*WGT(ML,NL)
                  SUMW = SUMW + WGT(ML,NL)
              ELSE
                  KMSG = KMSG + 1
              END IF
          END DO
c                                      return if user desired
          IF (IFLAG.EQ.1 .AND. KMSG.NE.0) RETURN
      END DO
c                                      compute wgted average
      IF (SUMW.NE.0.D0) THEN
          AVE = SUMT/SUMW
      END IF

      RETURN
      END
