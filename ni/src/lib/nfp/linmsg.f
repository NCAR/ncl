c -------------------------------------------------------------------
      subroutine linmsg (x,npts,xmsg,mflag)
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
c .               beginning and end of the series will be set to the value
c .               of the nearest non-msg value. if mflag.ge.0 then
c .               set these values to missing.
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
c .                             nn is the number of missing values still present.
c .   nitp      - No. of InTerpolated Points : user shouldcheck
c .               the ratio (nitp/npts)
 
      integer npts, mptcrt, ncode, nitp, n, nend, nstrt
      real    x(1:npts), xmsg
      integer nptcrt, nn, nbase
      real    slope

      mptcrt = npts
 
      nstrt = 0
      nend  = 0
      ncode = 0
      nitp  = 0
      nptcrt = iabs(mptcrt)
      do 40 n=1,npts
      if (x(n).eq.xmsg) then
         if (nstrt.eq.0) nstrt = n ! must be a msg pt : set indices
         nend = n
      else
 
c must be a valid pt : check for prior missing values
c        (1) if nstrt=0 then there are no msg prior values : skip out
c        (2) if (nend-nstrt+1).gt.nptcrt the set ncode : skip out
c        (3) if nstrt=1 then initial series values are msg : set to
c            first non-msg value
c        ... else
c            perform the linear interpolation
 
         if (nstrt.ne.0) then
            if ((nend-nstrt+1).gt.nptcrt) then
               go to 30
            endif
 
            if (nstrt.eq.1)then
               nitp = nitp + (nend-nstrt+1)
               if (mflag.lt.0) then
                   do 15 nn=nstrt,nend
   15              x(nn) = x(n)
               else
                   do 16 nn=nstrt,nend
   16              x(nn) = xmsg
               endif
            else
               nbase = nstrt-1
               slope = (x(n)-x(nbase))/ float(n-nbase)
               nitp = nitp + (nend-nstrt+1)
               do 20 nn=nstrt,nend
   20          x(nn) = x(nbase) + slope*float(nn-nbase)
            endif
 
   30       nstrt = 0
            nend   = 0
         endif
      endif
   40 continue
 
c check the end points
 
      if (nend.eq.npts) then
         nitp = nitp + (nend-nstrt+1)
         if (mflag.lt.0) then
             do 49 nn=nstrt,npts
   49        x(nn) = x(nstrt-1)
         else
             do 50 nn=nstrt,npts
   50        x(nn) = xmsg
         endif
      endif
 
c     nn = 0
c     do 60 n=1,npts
c     if (x(n).eq.xmsg) then
c         nn = nn+1
c     endif
c  60 continue
 
c     ncode = nn
c     if (nn.eq.npts) then
c         ncode = -1           ! for historical reasons
c     endif
 
      return
      end
