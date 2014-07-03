c.......subroutine bfilter
c
c	Written by:  David R. Russell, AFTAC/TT 10 December 2004
c
c       Subroutine bfilter executes a fast, stable zero phase butterworth
c       bandpass filter of order (m), which is optimized for narrow band
c       applications.  The method produces a complex time series output,
c       of which the real portion is used to calculate the filtered time
c       series, and the modulus is used to calculate the envelope function.
c       Stability of the method is achieved by reducing the bandpass 
c       filter calculations to simple cascaded first order filters,
c       which are forward and reverse filtered for zero phase.  The method
c       also does a linear shift of a butterworth lowpass filter 
c       to an equivalent bandpass, without going through a standard
c       non-linear translation (Kanasewich, E.R., 1975) to bandpass. An option
c       is included to remove the signal mean initially to compensate for
c       large DC offsets
c         
c	INPUT:
c
c	m:      Order of Butterworth filter
c	n:      Number of input, output time series points
c       mzer:   Integer flag to remove mean (0 no, 1 yes)
c	xr(n):  Input (real) time series
c       f0:     Center value of bandpass filter
c       fc:     Corners of filter [ flow= f0-fc; fhigh= f0+fc ]
c       dt:     Time series sampling interval
c
c	OUTPUT:
c
c       yr(n):  Output filtered (real) time series
c	er(n):  Output envelope function for filtered time series
c       ermx:   Maximum value of envelope function er.
c
c****************************     
      subroutine bfilter(xr,yr,er,ermx,f0,fc,dt,m,n,mzer)
c****************************
c
c.......nmax = total possible number of time series points
c       mmax = highest possible order of butterworth filter
c
      parameter (nmax=65000,mmax=10)
c
      double complex z1(nmax),z2(nmax),a1(mmax),a2(mmax),
     &               a1c(mmax),a2c(mmax),p,s,ctemp
      double precision pi,w0,wc,w1,w2,dtemp,dtt
      dimension xr(n),yr(n),er(n)
c
c.......error check on frequencies
c
      fnyq=1.0/(2.0*dt)
      if ((f0-fc).le.0.0) then
        write(6,*)'low corner frequency (f0-fc) <= 0.0'
        stop
      endif
      if ((f0+fc).ge.fnyq) then
        write(6,*)'high corner frequency (f0+fc) >= nyquist'
        stop
      endif
c
c.......initialize double precision pi, dtt, angular frequencies w0,wc
c
      pi=3.14159265358979d0
      w0=2.0d0*pi*dble(f0)
      wc=2.0d0*pi*dble(fc)
      dtt=dble(dt)
c
c.......prewarp frequencies for bilinear z-transform
c
      w1=w0-wc
      w2=w0+wc
      w1=2.0d0/dtt*dtan(w1*dtt/2.0d0)
      w2=2.0d0/dtt*dtan(w2*dtt/2.0d0)
      w0=(w1+w2)/2.0d0
      wc=(w2-w1)/2.0d0
c
c.......calculate (m) prototype lowpass poles (p), translate into bandpass
c       poles (s), calculate bilinear recursive coefficients (a1,a2),
c       conjugates of coefficients (a1c,a2c)
c
      do j=1,m
        dtemp=pi*(2.0d0*dble(j)-1.0d0+dble(m))/(2.0d0*dble(m))
        ctemp=dcmplx(0.0d0,dtemp)
        p=cdexp(ctemp)
        s=p*wc+dcmplx(0.0d0,w0)
        a1(j)=wc*dtt/(2.0d0-s*dtt)
        a2(j)=(2.0d0+s*dtt)/(2.0d0-s*dtt)
        a1c(j)=dconjg(a1(j))
        a2c(j)=dconjg(a2(j))
      enddo
c
c.......put real time series xr into complex series z1 and remove mean
c       if mzer set to 1
c
      xmean=0.0
      if(mzer.eq.1) then
        do k=1,n
          xmean=xmean+xr(k)
        enddo
        xmean=xmean/float(n)
      endif
      do k=1,n
        z1(k)=xr(k)-xmean
      enddo
c
c.......calculate (m) cascaded first order complex filters
c 
      do j=1,m
        do k=1,n
          z2(k)=z1(k)
        enddo
        z1(1)=a1(j)*z2(1)
        do k=2,n
          z1(k)=a1(j)*(z2(k)+z2(k-1))+a2(j)*z1(k-1)
        enddo
      enddo
c
c.......reverse filtered time series
c
      do k=1,n
        z2(k)=z1(n-k+1)
      enddo
c
c.......calculate (m) cascaded first order complex filters on
c       reversed series - note conjugate bilinear coefficients
c       for complex conjugate of filter
c
      do j=1,m
        do k=1,n
          z1(k)=z2(k)
        enddo
        z2(1)=a1c(j)*z1(1)
        do k=2,n
          z2(k)=a1c(j)*(z1(k)+z1(k-1))+a2c(j)*z2(k-1)
        enddo
      enddo
c
c.......reverse filtered time series
c
      do k=1,n
        z1(k)=z2(n-k+1)
      enddo
c
c.......calculate real output time series (yr), envelope (er),
c       envelope maximum (ermx)
c
      ermx=0.0
      do k=1,n
        yr(k)=2.0*dreal(z1(k))
        er(k)=2.0*cdabs(z1(k))
        ermx=amax1(er(k),ermx)
      enddo
      return
      end
