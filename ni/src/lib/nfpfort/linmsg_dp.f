c -------------------------------------------------------------------
      SUBROUTINE DLINMSG(X,NPTS,XMSG,MFLAG,MPTCRT)
c     implicit none

c NCL: xnew = linmsg(x,mflag)

c given a series x of length npts : this routine will linearly
c .   interpolate to fill in the missing pts. missing values at the
c .   beginning and end of the series will be be determined
c .   by the sign of mprcrt.
c
c nomenclature :
c .   x         - input series which may or may not contain msg values
c .   npts      - length of the series
c .   xmsg      - missing code
c .   mflag     - note: if mflag.lt.0 then the missing values at the
c .               beginning and end of the series will be set to the
c .               value of the nearest non-msg value. if mflag.ge.0
c .               then set these values to missing.
c .   mptcrt    - if more than "mptcrt" consecutive values are 
c .               encountered, the routine will not interpolate across
c .               that segment. If mptcrt=npts [most common option], 
c .               then the routine will interpolate as many values as
c .               it can.
c .
c OTHER variables
c .   ncode     - code number
c .               ncode = -1  : whole series is missing
c .               ncode =  0  : series has no missing points upon return
c .                             to the calling routine. either the serie
c .                             had no missing points or this routine
c .                             has filled them with interpolated values
c .               ncode = nn  : series still has missing values. this
c .                             occurs when iabs(mptcrt) is exceeded.
c .                             nn is the number of missing values
c .                             still present.
c .   nitp      - No. of InTerpolated Points : user shouldcheck
c .               the ratio (nitp/npts)

      INTEGER NPTS,MPTCRT,NCODE,NITP,N,NEND,NSTRT
      DOUBLE PRECISION X(1:NPTS),XMSG
      INTEGER NPTCRT,NN,NBASE
      DOUBLE PRECISION SLOPE
C
C This do loop was added later to check for the special
C case were all values in X are missing.
C
      DO 5 N=1,NPTS
         IF (X(N).NE.XMSG) GO TO 10
 5    END DO   
      RETURN

c c c MPTCRT = NPTS   ! updated version

 10   NSTRT = 0
      NEND = 0
      NCODE = 0
      NITP = 0
      NPTCRT = IABS(MPTCRT)
      DO 40 N = 1,NPTS
          IF (X(N).EQ.XMSG) THEN
C must be a msg pt : set indices
              IF (NSTRT.EQ.0) NSTRT = N
              NEND = N
          ELSE

c must be a valid pt : check for prior missing values
c        (1) if nstrt=0 then there are no msg prior values : skip out
c        (2) if (nend-nstrt+1).gt.nptcrt the set ncode : skip out
c        (3) if nstrt=1 then initial series values are msg : set to
c            first non-msg value
c        ... else
c            perform the linear interpolation

              IF (NSTRT.NE.0) THEN
                  IF ((NEND-NSTRT+1).GT.NPTCRT) THEN
                      GO TO 30
                  END IF

                  IF (NSTRT.EQ.1) THEN
                      NITP = NITP + (NEND-NSTRT+1)
                      IF (MFLAG.LT.0) THEN
                          DO 15 NN = NSTRT,NEND
   15                     X(NN) = X(N)
                      ELSE
                          DO 16 NN = NSTRT,NEND
   16                     X(NN) = XMSG
                      END IF
                  ELSE
                      NBASE = NSTRT - 1
                      SLOPE = (X(N)-X(NBASE))/DBLE(N-NBASE)
                      NITP = NITP + (NEND-NSTRT+1)
                      DO 20 NN = NSTRT,NEND
   20                 X(NN) = X(NBASE) + SLOPE*DBLE(NN-NBASE)
                  END IF

   30             NSTRT = 0
                  NEND = 0
              END IF
          END IF
   40 CONTINUE

c check the end points

      IF (NEND.EQ.NPTS) THEN
          NITP = NITP + (NEND-NSTRT+1)
          IF (MFLAG.LT.0) THEN
              DO 49 NN = NSTRT,NPTS
   49         X(NN) = X(NSTRT-1)
          ELSE
              DO 50 NN = NSTRT,NPTS
   50         X(NN) = XMSG
          END IF
      END IF

c     nn = 0
c     do 60 n=1,npts
c     if (x(n).eq.xmsg) then
c         nn = nn+1
c     endif
c  60 continue

c     ncode = nn
c     if (nn.eq.npts) then
C for historical reasons
c         ncode = -1
c     endif

      RETURN
      END
