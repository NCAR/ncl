C ---------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE DWGTRUNAVE(X,NPTS,WGT,NWGT,KOPT,XMSG,WORK,LWORK,IER)

C NCL function: x = wgtrunwgt (x,wgt,kopt)

      INTEGER NPTS,NWGT,KOPT,IER
      DOUBLE PRECISION X(NPTS),WGT(NWGT),XMSG
C NCLEND
      INTEGER LWORK
      DOUBLE PRECISION WORK(LWORK)

      NHALF = NWGT/2
      LWORK = NPTS + 2*NHALF

      CALL DWRUNAVX77(X,NPTS,WGT,NWGT,KOPT,XMSG,WORK,LWORK,IER)

C NCL return (wrunwgt[x])
      RETURN
      END
c ---------------------------------------------------------
      SUBROUTINE DWRUNAVX77(X,NPTS,WGT,NWGT,KOPT,XMSG,WORK,LW,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION WSUM
      DOUBLE PRECISION SUM

c same as wrunavx: slightly faster but slightly larger memory for work

c this routine will perform a weighted  running average on x
c .   the end points may be treated in three different ways

c nomenclature:
c .   x         - on input the series to be smoothed
c .               on output the smoothed series
c .   npts      - no of pts in x
c .   wgt       - vector containing weights [length=nwgt]
c .   nwgt      - no. of pts to be utilized in the wgted running average
c .   kopt      - end-point option [prefer an odd number]
c .               kopt < 0 : utilize cyclic conditions
C*PL*ERROR* Comment line too long
c .                          e.g.,  nwgt=3 at n=1 and n=npts and wsum=SUM{wgt}
c .                          x(1)    = w(1)*x(npts)+w(2)*x(1)+w(3)*x(2)
C*PL*ERROR* Comment line too long
c .                          x(npts) = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(1)
c .                          e.g.,  nwgt=4 at n=1 and n=npts
C*PL*ERROR* Comment line too long
c .                          x(1)    = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(1)+w(4)*x(2)
c .                          x(npts) = w(1)*x(npts-2)+w(2)*x(npts-1)
c .                                   +w(3)*x(npts)  +w(4)*x(1)
C*PL*ERROR* Comment line too long
c .               kopt = 0 : set unsmoothed beginning and end pts to xmsg
C*PL*ERROR* Comment line too long
c .                          e.g.,  nwgt=3 , x(1) = xmsg and x(npts) = xmsg
C*PL*ERROR* Comment line too long
c .                                        , x(2) = w(1)*x(1)+w(2)*x(2)+w(3)*x(3)
c .                          e.g.,  nwgt=4 , x(1) = xmsg and x(2) = xmsg
c .                                        , x(3) = w(1)*x(1)+w(2)*x(2)
c .                                                +w(3)*x(3)+w(4)*x(4)
c .                                     x(npts-1) = xmsg, x(npts) = xmsg
c .               kopt > 0 : utilize reflective (symmetric) conditions
c .                          e.g.,  nwgt=3 at n=1 and n=npts
c .                          x(1)    = w(1)*x(2)+w(2)*x(1)+w(3)*x(2)
C*PL*ERROR* Comment line too long
c .                          x(npts) = w(1)*x(npts-1)+w(2)*x(npts)+w(3)*x(npts-1)
c .                          e.g.,  nwgt=4 at n=1 and n=npts
C*PL*ERROR* Comment line too long
c .                          x(1)    = w(1)*x(3)+w(2)*x(2)+w(3)*x(1)+w(4)*x(2)
c .                          x(npts) = w(1)*x(npts-2)+w(2)*x(npts-1)
c .                                   +w(3)*x(npts)  +w(4)*x(npts-1)
c .   xmsg      - missing/special value
c .               also; if kopt=0 set end points to this special value
c .   work      - work vector
c .   lw        - length of work (=npts+2*N where N=nwgt/2)
c .   ier       - error flag
c .               ier = 0 : no errors
c .               ier = -11 : npts @ 0
c .               ier = -12 : npts < nwgt
c .               ier = -13 : nwgt is not odd

      DOUBLE PRECISION X(1:NPTS),WORK(LW),WGT(NWGT)

      IER = 0
      IF (NPTS.LT.1) IER = -11
      IF (NWGT.GT.NPTS) IER = -12
      IF (IER.LT.0) RETURN

      IF (NWGT.LE.1) THEN
          DO N = 1,NPTS
C historical reasons
              WORK(N) = X(N)
          END DO
          RETURN
      END IF

      NAV = NWGT
      NAV2 = NAV/2
      LWORK = NPTS + 2*NAV2
      NOE = 0
      IF (MOD(NWGT,2).EQ.0) NOE = 1

      DO N = 1,LWORK
C preset to xmsg
          WORK(N) = XMSG
      END DO

      DO N = 1,NPTS
C stuff x into middle of work
          WORK(NAV2+N) = X(N)
      END DO

      WSUM = 0.0D0
      DO N = 1,NWGT
          WSUM = WSUM + WGT(N)
      END DO
      IF (WSUM.GT.1.0D0) THEN
          WSUM = 1.D0/WSUM
      ELSE
C This is a "do nothing" op
          WSUM = 1.0D0
      END IF
C pad work with appropriate values

      IF (KOPT.GT.0) THEN
          DO N = 1,NAV2
              WORK(NAV2+1-N) = X(N+1)
              WORK(NPTS+NAV2+N) = X(NPTS-N)
          END DO
      ELSE IF (KOPT.LT.0) THEN
          DO N = 1,NAV2
              WORK(NAV2+1-N) = X(NPTS+1-N)
              WORK(NPTS+NAV2+N) = X(N)
          END DO
      END IF

      DO N = 1,NPTS
          KMSG = 0
          SUM = 0.D0
          NMID = N + NAV2 + NOE
          MSTRT = NMID - NAV2
          MLAST = MSTRT + NAV - 1
          DO M = MSTRT,MLAST
              IF (WORK(M).NE.XMSG) THEN
                  SUM = SUM + WORK(M)*WGT(M-MSTRT+1)
              ELSE
                  KMSG = KMSG + 1
              END IF
          END DO

          IF (KMSG.EQ.0) THEN
C all values
              X(N) = SUM*WSUM
          ELSE
C msg values encountered
              X(N) = XMSG
          END IF
      END DO

      RETURN
      END
