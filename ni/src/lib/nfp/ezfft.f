c =======fftpack.f subset==============================================
c     subroutine ezfftf(n,r,azero,a,b,wsave)
c
c     subroutine ezfftf computes the fourier coefficients of a real
c     perodic sequence (fourier analysis). the transform is defined
c     below at output parameters azero,a and b. ezfftf is a simplified
c     but slower version of rfftf.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is must efficient when n is the product of small primes.
c
c     r       a real array of length n which contains the sequence
c             to be transformed. r is not destroyed.
c
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             in the program that calls ezfftf. the wsave array must be
c             initialized by calling subroutine ezffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by ezfftf and ezfftb.
c
c     output parameters
c
c     azero   the sum from i=1 to i=n of r(i)/n
c
c     a,b     for n even b(n/2)=0. and a(n/2) is the sum from i=1 to
c             i=n of (-1)**(i-1)*r(i)/n
c
c             for n even define kmax=n/2-1
c             for n odd  define kmax=(n-1)/2
c
c             then for  k=1,...,kmax
c
c                  a(k) equals the sum from i=1 to i=n of
c
c                       2./n*r(i)*cos(k*(i-1)*2*pi/n)
c
c                  b(k) equals the sum from i=1 to i=n of
c
c                       2./n*r(i)*sin(k*(i-1)*2*pi/n)
c
c
      subroutine ezfftf(n, r, azero, a, b, wsave)
      dimension r(*), a(*), b(*), wsave(*)
c
      if (n - 2 .le. 0) then
         if (n - 2 .ne. 0) then
            azero = r(1)
            return 
         endif
         azero = .5*(r(1)+r(2))
         a(1) = .5*(r(1)-r(2))
         return 
      endif
      do i = 1, n
         wsave(i) = r(i)
      end do
      call rfftf (n, wsave, wsave(n+1))
      cf = 2./float(n)
      cfm = -cf
      azero = .5*cf*wsave(1)
      ns2 = (n + 1)/2
      ns2m = ns2 - 1
      do i = 1, ns2m
         a(i) = cf*wsave(2*i)
         b(i) = cfm*wsave(2*i+1)
      end do
      b(ns2) = 0.
      if (mod(n,2) .eq. 1) return 
      a(ns2) = .5*cf*wsave(n)
      b(ns2) = 0.
      return 
      end 
c
c =====================================================================
c     subroutine rfftf(n,r,wsave)
c
c     subroutine rfftf computes the fourier coefficients of a real
c     perodic sequence (fourier analysis). the transform is defined
c     below at output parameter r.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is most efficient when n is a product of small primes.
c             n may change so long as different work arrays are provided
c
c     r       a real array of length n which contains the sequence
c             to be transformed
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls rfftf. the wsave array must be
c             initialized by calling subroutine rffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by rfftf and rfftb.
c
c
c     output parameters
c
c     r       r(1) = the sum from i=1 to i=n of r(i)
c
c             if n is even set l =n/2   , if n is odd set l = (n+1)/2
c
c               then for k = 2,...,l
c
c                  r(2*k-2) = the sum from i = 1 to i = n of
c
c                       r(i)*cos((k-1)*(i-1)*2*pi/n)
c
c                  r(2*k-1) = the sum from i = 1 to i = n of
c
c                      -r(i)*sin((k-1)*(i-1)*2*pi/n)
c
c             if n is even
c
c                  r(n) = the sum from i = 1 to i = n of
c
c                       (-1)**(i-1)*r(i)
c
c      *****  note
c                  this transform is unnormalized since a call of rfftf
c                  followed by a call of rfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of rfftf or rfftb.
c
      subroutine rfftf(n, r, wsave)
      dimension r(*), wsave(*)
c
      if (n .eq. 1) return 
      call rfftf1 (n, r, wsave, wsave(n+1), wsave(2*n+1))
      return 
      end 
c
      subroutine rfftf1(n, c, ch, wa, ifac)
      dimension ch(*), c(*), wa(*), ifac(*)
      nf = ifac(2)
      na = 1
      l2 = n
      iw = n
      do k1 = 1, nf
         kh = nf - k1
         ip = ifac(kh+3)
         l1 = l2/ip
         ido = n/l2
         idl1 = ido*l1
         iw = iw - (ip - 1)*ido
         na = 1 - na
         if (ip .eq. 4) then
            ix2 = iw + ido
            ix3 = ix2 + ido
            if (na .eq. 0) then
               call radf4 (ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3))
               go to 110
            endif
            call radf4 (ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3))
            go to 110
         endif
         if (ip .eq. 2) then
            if (na .eq. 0) then
               call radf2 (ido, l1, c, ch, wa(iw))
               go to 110
            endif
            call radf2 (ido, l1, ch, c, wa(iw))
            go to 110
         endif
  104    continue
         if (ip .eq. 3) then
            ix2 = iw + ido
            if (na .eq. 0) then
               call radf3 (ido, l1, c, ch, wa(iw), wa(ix2))
               go to 110
            endif
            call radf3 (ido, l1, ch, c, wa(iw), wa(ix2))
            go to 110
         endif
  106    continue
         if (ip .eq. 5) then
            ix2 = iw + ido
            ix3 = ix2 + ido
            ix4 = ix3 + ido
            if (na .eq. 0) then
               call radf5(ido,l1,c,ch,wa(iw),wa(ix2),wa(ix3),wa(ix4))
               go to 110
            endif
            call radf5(ido,l1,ch,c,wa(iw),wa(ix2),wa(ix3),wa(ix4))
            go to 110
         endif
  108    continue
         if (ido .eq. 1) na = 1 - na
         if (na .eq. 0) then
            call radfg (ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw))
            na = 1
         else
            call radfg (ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw))
            na = 0
         endif
  110    continue
         l2 = l1
      end do
      if (na .eq. 1) return 
      do i = 1, n
         c(i) = ch(i)
      end do
      return 
      end 
c ==============================================================
c     subroutine ezfftb(n,r,azero,a,b,wsave)
c
c     subroutine ezfftb computes a real perodic sequence from its
c     fourier coefficients (fourier synthesis). the transform is
c     defined below at output parameter r. ezfftb is a simplified
c     but slower version of rfftb.
c
c     input parameters
c
c     n       the length of the output array r.  the method is most
c             efficient when n is the product of small primes.
c
c     azero   the constant fourier coefficient
c
c     a,b     arrays which contain the remaining fourier coefficients
c             these arrays are not destroyed.
c
c             the length of these arrays depends on whether n is even or
c             odd.
c
c             if n is even n/2    locations are required
c             if n is odd (n-1)/2 locations are required
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             in the program that calls ezfftb. the wsave array must be
c             initialized by calling subroutine ezffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by ezfftf and ezfftb.
c
c
c     output parameters
c
c     r       if n is even define kmax=n/2
c             if n is odd  define kmax=(n-1)/2
c
c             then for i=1,...,n
c
c                  r(i)=azero plus the sum from k=1 to k=kmax of
c
c                  a(k)*cos(k*(i-1)*2*pi/n)+b(k)*sin(k*(i-1)*2*pi/n)
c
c     ********************* complex notation **************************
c
c             for j=1,...,n
c
c             r(j) equals the sum from k=-kmax to k=kmax of
c
c                  c(k)*exp(i*k*(j-1)*2*pi/n)
c
c             where
c
c                  c(k) = .5*cmplx(a(k),-b(k))   for k=1,...,kmax
c
c                  c(-k) = conjg(c(k))
c
c                  c(0) = azero
c
c                       and i=sqrt(-1)
c
c     *************** amplitude - phase notation ***********************
c
c             for i=1,...,n
c
c             r(i) equals azero plus the sum from k=1 to k=kmax of
c
c                  alpha(k)*cos(k*(i-1)*2*pi/n+beta(k))
c
c             where
c
c                  alpha(k) = sqrt(a(k)*a(k)+b(k)*b(k))
c
c                  cos(beta(k))=a(k)/alpha(k)
c
c                  sin(beta(k))=-b(k)/alpha(k)
c
      subroutine ezfftb(n, r, azero, a, b, wsave)
      dimension r(*), a(*), b(*), wsave(*)
c
      if (n - 2 .le. 0) then
         if (n - 2 .ne. 0) then
            r(1) = azero
            return 
         endif
         r(1) = azero + a(1)
         r(2) = azero - a(1)
         return 
      endif
      ns2 = (n - 1)/2
      do i = 1, ns2
         r(2*i) = .5*a(i)
         r(2*i+1) = -.5*b(i)
      end do
      r(1) = azero
      if (mod(n,2) .eq. 0) r(n) = a(ns2+1)
      call rfftb (n, r, wsave(n+1))
      return 
      end 
c
c =====================================================================
c     subroutine rfftb(n,r,wsave)
c
c     subroutine rfftb computes the real perodic sequence from its
c     fourier coefficients (fourier synthesis). the transform is defined
c     below at output parameter r.
c
c     input parameters
c
c     n       the length of the array r to be transformed.  the method
c             is most efficient when n is a product of small primes.
c             n may change so long as different work arrays are provided
c
c     r       a real array of length n which contains the sequence
c             to be transformed
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             in the program that calls rfftb. the wsave array must be
c             initialized by calling subroutine rffti(n,wsave) and a
c             different wsave array must be used for each different
c             value of n. this initialization does not have to be
c             repeated so long as n remains unchanged thus subsequent
c             transforms can be obtained faster than the first.
c             the same wsave array can be used by rfftf and rfftb.
c
c
c     output parameters
c
c     r       for n even and for i = 1,...,n
c
c                  r(i) = r(1)+(-1)**(i-1)*r(n)
c
c                       plus the sum from k=2 to k=n/2 of
c
c                        2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                       -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c             for n odd and for i = 1,...,n
c
c                  r(i) = r(1) plus the sum from k=2 to k=(n+1)/2 of
c
c                       2.*r(2*k-2)*cos((k-1)*(i-1)*2*pi/n)
c
c                      -2.*r(2*k-1)*sin((k-1)*(i-1)*2*pi/n)
c
c      *****  note
c                  this transform is unnormalized since a call of rfftf
c                  followed by a call of rfftb will multiply the input
c                  sequence by n.
c
c     wsave   contains results which must not be destroyed between
c             calls of rfftb or rfftf.
c
c
      subroutine rfftb(n, r, wsave)
      dimension r(*), wsave(*)
c
      if (n .eq. 1) return 
      call rfftb1 (n, r, wsave, wsave(n+1), wsave(2*n+1))
      return 
      end 
c
c =====================================================================
      subroutine rfftb1(n, c, ch, wa, ifac)
      dimension ch(*), c(*), wa(*), ifac(*)
      nf = ifac(2)
      na = 0
      l1 = 1
      iw = 1
      do k1 = 1, nf
         ip = ifac(k1+2)
         l2 = ip*l1
         ido = n/l2
         idl1 = ido*l1
         if (ip .eq. 4) then
            ix2 = iw + ido
            ix3 = ix2 + ido
            if (na .eq. 0) then
               call radb4 (ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3))
            else
               call radb4 (ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3))
            endif
            na = 1 - na
         else
            if (ip .eq. 2) then
               if (na .eq. 0) then
                  call radb2 (ido, l1, c, ch, wa(iw))
               else
                  call radb2 (ido, l1, ch, c, wa(iw))
               endif
               na = 1 - na
            else
               if (ip .eq. 3) then
                  ix2 = iw + ido
                  if (na .eq. 0) then
                     call radb3 (ido, l1, c, ch, wa(iw), wa(ix2))
                  else
                     call radb3 (ido, l1, ch, c, wa(iw), wa(ix2))
                  endif
                  na = 1 - na
               else
                  if (ip .eq. 5) then
                     ix2 = iw + ido
                     ix3 = ix2 + ido
                     ix4 = ix3 + ido
                     if (na .eq. 0) then
                        call radb5 (ido, l1, c, ch, wa(iw), wa(ix2), wa(
     1                     ix3), wa(ix4))
                     else
                        call radb5 (ido, l1, ch, c, wa(iw), wa(ix2), wa(
     1                     ix3), wa(ix4))
                     endif
                     na = 1 - na
                  else
                     if (na .eq. 0) then
                        call radbg(ido,ip,l1,idl1,c,c,c,ch,ch,wa(iw))
                     else
                        call radbg(ido,ip,l1,idl1,ch,ch,ch,c,c,wa(iw))
                     endif
                     if (ido .eq. 1) na = 1 - na
                  endif
               endif
            endif
         endif
         l1 = l2
         iw = iw + (ip - 1)*ido
      end do
      if (na .eq. 0) return 
      do i = 1, n
         c(i) = ch(i)
      end do
      return 
      end 
c ==============================================================
c     subroutine ezffti(n,wsave)
c
c     subroutine ezffti initializes the array wsave which is used in
c     both ezfftf and ezfftb. the prime factorization of n together with
c     a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 3*n+15.
c             the same work array can be used for both ezfftf and ezfftb
c             as long as n remains unchanged. different wsave arrays
c             are required for different values of n.
c
      subroutine ezffti(n, wsave)
      dimension wsave(*)
c
      if (n .eq. 1) return 
      call ezfft1 (n, wsave(2*n+1), wsave(3*n+1))
      return 
      end 
c
      subroutine ezfft1(n, wa, ifac)
      dimension wa(*), ifac(*), ntryh(4)
      data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/ 4, 2, 3, 5/ 
      tpi = 8.0*atan(1.0)
      nl = n
      nf = 0
      j = 0
  101 continue
      j = j + 1
      if (j - 4 .le. 0) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      endif
  104 continue
      nq = nl/ntry
      nr = nl - ntry*nq
      if (nr .ne. 0) go to 101
      nf = nf + 1
      ifac(nf+2) = ntry
      nl = nq
      if (ntry .eq. 2) then
         if (nf .ne. 1) then
            do i = 2, nf
               ib = nf - i + 2
               ifac(ib+2) = ifac(ib+1)
            end do
            ifac(3) = 2
         endif
      endif
      if (nl .ne. 1) go to 104
      ifac(1) = n
      ifac(2) = nf
      argh = tpi/float(n)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      if (nfm1 .eq. 0) return 
      do k1 = 1, nfm1
         ip = ifac(k1+2)
         l2 = l1*ip
         ido = n/l2
         ipm = ip - 1
         arg1 = float(l1)*argh
         ch1 = 1.
         sh1 = 0.
         dch1 = cos(arg1)
         dsh1 = sin(arg1)
         do j = 1, ipm
            ch1h = dch1*ch1 - dsh1*sh1
            sh1 = dch1*sh1 + dsh1*ch1
            ch1 = ch1h
            i = is + 2
            wa(i-1) = ch1
            wa(i) = sh1
            if (ido .ge. 5) then
               do ii = 5, ido, 2
                  i = i + 2
                  wa(i-1) = ch1*wa(i-3) - sh1*wa(i-2)
                  wa(i) = ch1*wa(i-2) + sh1*wa(i-3)
               end do
            endif
            is = is + ido
         end do
         l1 = l2
      end do
      return 
      end 
c ==========================================================
c     subroutine rffti(n,wsave)
c
c     subroutine rffti initializes the array wsave which is used in
c     both rfftf and rfftb. the prime factorization of n together with
c     a tabulation of the trigonometric functions are computed and
c     stored in wsave.
c
c     input parameter
c
c     n       the length of the sequence to be transformed.
c
c     output parameter
c
c     wsave   a work array which must be dimensioned at least 2*n+15.
c             the same work array can be used for both rfftf and rfftb
c             as long as n remains unchanged. different wsave arrays
c             are required for different values of n. the contents of
c             wsave must not be changed between calls of rfftf or rfftb.
c
      subroutine rffti(n, wsave)
      dimension wsave(*)
c
      if (n .eq. 1) return 
      call rffti1 (n, wsave(n+1), wsave(2*n+1))
      return 
      end 
c
c ==========================================================
c      real function pimach (dum)
c     pi=3.1415926535897932384626433832795028841971693993751058209749446
c
c      pimach = 4.*atan(1.0)
c      return 
c      end 
c
      subroutine rffti1(n, wa, ifac)
      dimension wa(*), ifac(*), ntryh(4)
      data ntryh(1), ntryh(2), ntryh(3), ntryh(4)/ 4, 2, 3, 5/ 
      nl = n
      nf = 0
      j = 0
  101 continue
      j = j + 1
      if (j - 4 .le. 0) then
         ntry = ntryh(j)
      else
         ntry = ntry + 2
      endif
  104 continue
      nq = nl/ntry
      nr = nl - ntry*nq
      if (nr .ne. 0) go to 101
      nf = nf + 1
      ifac(nf+2) = ntry
      nl = nq
      if (ntry .eq. 2) then
         if (nf .ne. 1) then
            do i = 2, nf
               ib = nf - i + 2
               ifac(ib+2) = ifac(ib+1)
            end do
            ifac(3) = 2
         endif
      endif
      if (nl .ne. 1) go to 104
      ifac(1) = n
      ifac(2) = nf
      tpi = 2.0*pimach(dum)
      argh = tpi/float(n)
      is = 0
      nfm1 = nf - 1
      l1 = 1
      if (nfm1 .eq. 0) return 
      do k1 = 1, nfm1
         ip = ifac(k1+2)
         ld = 0
         l2 = l1*ip
         ido = n/l2
         ipm = ip - 1
         do j = 1, ipm
            ld = ld + l1
            i = is
            argld = float(ld)*argh
            fi = 0.
            do ii = 3, ido, 2
               i = i + 2
               fi = fi + 1.
               arg = fi*argld
               wa(i-1) = cos(arg)
               wa(i) = sin(arg)
            end do
            is = is + ido
         end do
         l1 = l2
      end do
      return 
      end 
c
      subroutine radf2(ido, l1, cc, ch, wa1)
      dimension ch(ido,2,l1), cc(ido,l1,2), wa1(*)
      do k = 1, l1
         ch(1,1,k) = cc(1,k,1) + cc(1,k,2)
         ch(ido,2,k) = cc(1,k,1) - cc(1,k,2)
      end do
      if (ido - 2 .ge. 0) then
         if (ido - 2 .ne. 0) then
            idp2 = ido + 2
            do k = 1, l1
               do i = 3, ido, 2
                  ic = idp2 - i
                  tr2 = wa1(i-2)*cc(i-1,k,2) + wa1(i-1)*cc(i,k,2)
                  ti2 = wa1(i-2)*cc(i,k,2) - wa1(i-1)*cc(i-1,k,2)
                  ch(i,1,k) = cc(i,k,1) + ti2
                  ch(ic,2,k) = ti2 - cc(i,k,1)
                  ch(i-1,1,k) = cc(i-1,k,1) + tr2
                  ch(ic-1,2,k) = cc(i-1,k,1) - tr2
               end do
            end do
            if (mod(ido,2) .eq. 1) return 
         endif
         do k = 1, l1
            ch(1,2,k) = -cc(ido,k,2)
            ch(ido,1,k) = cc(ido,k,1)
         end do
      endif
      return 
      end 
c
      subroutine radf3(ido, l1, cc, ch, wa1, wa2)
      dimension ch(ido,3,l1), cc(ido,l1,3), wa1(*), wa2(*)
      data taur, taui/ -.5, .866025403784439/ 
      do k = 1, l1
         cr2 = cc(1,k,2) + cc(1,k,3)
         ch(1,1,k) = cc(1,k,1) + cr2
         ch(1,3,k) = taui*(cc(1,k,3)-cc(1,k,2))
         ch(ido,2,k) = cc(1,k,1) + taur*cr2
      end do
      if (ido .eq. 1) return 
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            dr2 = wa1(i-2)*cc(i-1,k,2) + wa1(i-1)*cc(i,k,2)
            di2 = wa1(i-2)*cc(i,k,2) - wa1(i-1)*cc(i-1,k,2)
            dr3 = wa2(i-2)*cc(i-1,k,3) + wa2(i-1)*cc(i,k,3)
            di3 = wa2(i-2)*cc(i,k,3) - wa2(i-1)*cc(i-1,k,3)
            cr2 = dr2 + dr3
            ci2 = di2 + di3
            ch(i-1,1,k) = cc(i-1,k,1) + cr2
            ch(i,1,k) = cc(i,k,1) + ci2
            tr2 = cc(i-1,k,1) + taur*cr2
            ti2 = cc(i,k,1) + taur*ci2
            tr3 = taui*(di2 - di3)
            ti3 = taui*(dr3 - dr2)
            ch(i-1,3,k) = tr2 + tr3
            ch(ic-1,2,k) = tr2 - tr3
            ch(i,3,k) = ti2 + ti3
            ch(ic,2,k) = ti3 - ti2
         end do
      end do
      return 
      end 
c
      subroutine radf4(ido, l1, cc, ch, wa1, wa2, wa3)
      dimension cc(ido,l1,4), ch(ido,4,l1), wa1(*), wa2(*), wa3(*)
      data hsqt2/ .7071067811865475/ 
      do k = 1, l1
         tr1 = cc(1,k,2) + cc(1,k,4)
         tr2 = cc(1,k,1) + cc(1,k,3)
         ch(1,1,k) = tr1 + tr2
         ch(ido,4,k) = tr2 - tr1
         ch(ido,2,k) = cc(1,k,1) - cc(1,k,3)
         ch(1,3,k) = cc(1,k,4) - cc(1,k,2)
      end do
      if (ido - 2 .ge. 0) then
         if (ido - 2 .ne. 0) then
            idp2 = ido + 2
            do k = 1, l1
               do i = 3, ido, 2
                  ic = idp2 - i
                  cr2 = wa1(i-2)*cc(i-1,k,2) + wa1(i-1)*cc(i,k,2)
                  ci2 = wa1(i-2)*cc(i,k,2) - wa1(i-1)*cc(i-1,k,2)
                  cr3 = wa2(i-2)*cc(i-1,k,3) + wa2(i-1)*cc(i,k,3)
                  ci3 = wa2(i-2)*cc(i,k,3) - wa2(i-1)*cc(i-1,k,3)
                  cr4 = wa3(i-2)*cc(i-1,k,4) + wa3(i-1)*cc(i,k,4)
                  ci4 = wa3(i-2)*cc(i,k,4) - wa3(i-1)*cc(i-1,k,4)
                  tr1 = cr2 + cr4
                  tr4 = cr4 - cr2
                  ti1 = ci2 + ci4
                  ti4 = ci2 - ci4
                  ti2 = cc(i,k,1) + ci3
                  ti3 = cc(i,k,1) - ci3
                  tr2 = cc(i-1,k,1) + cr3
                  tr3 = cc(i-1,k,1) - cr3
                  ch(i-1,1,k) = tr1 + tr2
                  ch(ic-1,4,k) = tr2 - tr1
                  ch(i,1,k) = ti1 + ti2
                  ch(ic,4,k) = ti1 - ti2
                  ch(i-1,3,k) = ti4 + tr3
                  ch(ic-1,2,k) = tr3 - ti4
                  ch(i,3,k) = tr4 + ti3
                  ch(ic,2,k) = tr4 - ti3
               end do
            end do
            if (mod(ido,2) .eq. 1) return 
         endif
         do k = 1, l1
            ti1 = -hsqt2*(cc(ido,k,2)+cc(ido,k,4))
            tr1 = hsqt2*(cc(ido,k,2)-cc(ido,k,4))
            ch(ido,1,k) = tr1 + cc(ido,k,1)
            ch(ido,3,k) = cc(ido,k,1) - tr1
            ch(1,2,k) = ti1 - cc(ido,k,3)
            ch(1,4,k) = ti1 + cc(ido,k,3)
         end do
      endif
      return 
      end 
c
      subroutine radf5(ido, l1, cc, ch, wa1, wa2, wa3, wa4)
      dimension cc(ido,l1,5),ch(ido,5,l1),wa1(*),wa2(*),wa3(*),wa4(*)
      data tr11, ti11, tr12, ti12/ .309016994374947, .951056516295154, 
     1   -.809016994374947, .587785252292473/ 
      do k = 1, l1
         cr2 = cc(1,k,5) + cc(1,k,2)
         ci5 = cc(1,k,5) - cc(1,k,2)
         cr3 = cc(1,k,4) + cc(1,k,3)
         ci4 = cc(1,k,4) - cc(1,k,3)
         ch(1,1,k) = cc(1,k,1) + cr2 + cr3
         ch(ido,2,k) = cc(1,k,1) + tr11*cr2 + tr12*cr3
         ch(1,3,k) = ti11*ci5 + ti12*ci4
         ch(ido,4,k) = cc(1,k,1) + tr12*cr2 + tr11*cr3
         ch(1,5,k) = ti12*ci5 - ti11*ci4
      end do
      if (ido .eq. 1) return 
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            dr2 = wa1(i-2)*cc(i-1,k,2) + wa1(i-1)*cc(i,k,2)
            di2 = wa1(i-2)*cc(i,k,2) - wa1(i-1)*cc(i-1,k,2)
            dr3 = wa2(i-2)*cc(i-1,k,3) + wa2(i-1)*cc(i,k,3)
            di3 = wa2(i-2)*cc(i,k,3) - wa2(i-1)*cc(i-1,k,3)
            dr4 = wa3(i-2)*cc(i-1,k,4) + wa3(i-1)*cc(i,k,4)
            di4 = wa3(i-2)*cc(i,k,4) - wa3(i-1)*cc(i-1,k,4)
            dr5 = wa4(i-2)*cc(i-1,k,5) + wa4(i-1)*cc(i,k,5)
            di5 = wa4(i-2)*cc(i,k,5) - wa4(i-1)*cc(i-1,k,5)
            cr2 = dr2 + dr5
            ci5 = dr5 - dr2
            cr5 = di2 - di5
            ci2 = di2 + di5
            cr3 = dr3 + dr4
            ci4 = dr4 - dr3
            cr4 = di3 - di4
            ci3 = di3 + di4
            ch(i-1,1,k) = cc(i-1,k,1) + cr2 + cr3
            ch(i,1,k) = cc(i,k,1) + ci2 + ci3
            tr2 = cc(i-1,k,1) + tr11*cr2 + tr12*cr3
            ti2 = cc(i,k,1) + tr11*ci2 + tr12*ci3
            tr3 = cc(i-1,k,1) + tr12*cr2 + tr11*cr3
            ti3 = cc(i,k,1) + tr12*ci2 + tr11*ci3
            tr5 = ti11*cr5 + ti12*cr4
            ti5 = ti11*ci5 + ti12*ci4
            tr4 = ti12*cr5 - ti11*cr4
            ti4 = ti12*ci5 - ti11*ci4
            ch(i-1,3,k) = tr2 + tr5
            ch(ic-1,2,k) = tr2 - tr5
            ch(i,3,k) = ti2 + ti5
            ch(ic,2,k) = ti5 - ti2
            ch(i-1,5,k) = tr3 + tr4
            ch(ic-1,4,k) = tr3 - tr4
            ch(i,5,k) = ti3 + ti4
            ch(ic,4,k) = ti4 - ti3
         end do
      end do
      return 
      end 
c
      subroutine radfg(ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
      dimension ch(ido,l1,ip), cc(ido,ip,l1), c1(ido,l1,ip), c2(idl1,ip)
     1   , ch2(idl1,ip), wa(*)
      tpi = 2.0*pimach(dum)
      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      ipph = (ip + 1)/2
      ipp2 = ip + 2
      idp2 = ido + 2
      nbd = (ido - 1)/2
      if (ido .ne. 1) then
         do ik = 1, idl1
            ch2(ik,1) = c2(ik,1)
         end do
         do j = 2, ip
            do k = 1, l1
               ch(1,k,j) = c1(1,k,j)
            end do
         end do
         if (nbd .le. l1) then
            is = -ido
            do j = 2, ip
               is = is + ido
               idij = is
               do i = 3, ido, 2
                  idij = idij + 2
                  do k = 1, l1
                     ch(i-1,k,j) = wa(idij-1)*c1(i-1,k,j) + wa(idij)*c1(
     1                  i,k,j)
                     ch(i,k,j) = wa(idij-1)*c1(i,k,j) - wa(idij)*c1(i-1,
     1                  k,j)
                  end do
               end do
            end do
         else
            is = -ido
            do j = 2, ip
               is = is + ido
               do k = 1, l1
                  idij = is
                  do i = 3, ido, 2
                     idij = idij + 2
                     ch(i-1,k,j) = wa(idij-1)*c1(i-1,k,j) + wa(idij)*c1(
     1                  i,k,j)
                     ch(i,k,j) = wa(idij-1)*c1(i,k,j) - wa(idij)*c1(i-1,
     1                  k,j)
                  end do
               end do
            end do
         endif
         if (nbd .ge. l1) then
            do j = 2, ipph
               jc = ipp2 - j
               do k = 1, l1
                  do i = 3, ido, 2
                     c1(i-1,k,j) = ch(i-1,k,j) + ch(i-1,k,jc)
                     c1(i-1,k,jc) = ch(i,k,j) - ch(i,k,jc)
                     c1(i,k,j) = ch(i,k,j) + ch(i,k,jc)
                     c1(i,k,jc) = ch(i-1,k,jc) - ch(i-1,k,j)
                  end do
               end do
            end do
            go to 121
         endif
         do j = 2, ipph
            jc = ipp2 - j
            do i = 3, ido, 2
               do k = 1, l1
                  c1(i-1,k,j) = ch(i-1,k,j) + ch(i-1,k,jc)
                  c1(i-1,k,jc) = ch(i,k,j) - ch(i,k,jc)
                  c1(i,k,j) = ch(i,k,j) + ch(i,k,jc)
                  c1(i,k,jc) = ch(i-1,k,jc) - ch(i-1,k,j)
               end do
            end do
         end do
         go to 121
      endif
      do ik = 1, idl1
         c2(ik,1) = ch2(ik,1)
      end do
  121 continue
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            c1(1,k,j) = ch(1,k,j) + ch(1,k,jc)
            c1(1,k,jc) = ch(1,k,jc) - ch(1,k,j)
         end do
      end do
c
      ar1 = 1.
      ai1 = 0.
      do l = 2, ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         do ik = 1, idl1
            ch2(ik,l) = c2(ik,1) + ar1*c2(ik,2)
            ch2(ik,lc) = ai1*c2(ik,ip)
         end do
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3, ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            do ik = 1, idl1
               ch2(ik,l) = ch2(ik,l) + ar2*c2(ik,j)
               ch2(ik,lc) = ch2(ik,lc) + ai2*c2(ik,jc)
            end do
         end do
      end do
      do j = 2, ipph
         do ik = 1, idl1
            ch2(ik,1) = ch2(ik,1) + c2(ik,j)
         end do
      end do
c
      if (ido .ge. l1) then
         do k = 1, l1
            do i = 1, ido
               cc(i,1,k) = ch(i,k,1)
            end do
         end do
      else
         do i = 1, ido
            do k = 1, l1
               cc(i,1,k) = ch(i,k,1)
            end do
         end do
      endif
      do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1, l1
            cc(ido,j2-2,k) = ch(1,k,j)
            cc(1,j2-1,k) = ch(1,k,jc)
         end do
      end do
      if (ido .eq. 1) return 
      if (nbd .ge. l1) then
         do j = 2, ipph
            jc = ipp2 - j
            j2 = j + j
            do k = 1, l1
               do i = 3, ido, 2
                  ic = idp2 - i
                  cc(i-1,j2-1,k) = ch(i-1,k,j) + ch(i-1,k,jc)
                  cc(ic-1,j2-2,k) = ch(i-1,k,j) - ch(i-1,k,jc)
                  cc(i,j2-1,k) = ch(i,k,j) + ch(i,k,jc)
                  cc(ic,j2-2,k) = ch(i,k,jc) - ch(i,k,j)
               end do
            end do
         end do
         return 
      endif
      do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do i = 3, ido, 2
            ic = idp2 - i
            do k = 1, l1
               cc(i-1,j2-1,k) = ch(i-1,k,j) + ch(i-1,k,jc)
               cc(ic-1,j2-2,k) = ch(i-1,k,j) - ch(i-1,k,jc)
               cc(i,j2-1,k) = ch(i,k,j) + ch(i,k,jc)
               cc(ic,j2-2,k) = ch(i,k,jc) - ch(i,k,j)
            end do
         end do
      end do
      return 
      end 
c
      subroutine radb2(ido, l1, cc, ch, wa1)
      dimension cc(ido,2,l1), ch(ido,l1,2), wa1(*)
      do k = 1, l1
         ch(1,k,1) = cc(1,1,k) + cc(ido,2,k)
         ch(1,k,2) = cc(1,1,k) - cc(ido,2,k)
      end do
      if (ido - 2 .ge. 0) then
         if (ido - 2 .ne. 0) then
            idp2 = ido + 2
            do k = 1, l1
               do i = 3, ido, 2
                  ic = idp2 - i
                  ch(i-1,k,1) = cc(i-1,1,k) + cc(ic-1,2,k)
                  tr2 = cc(i-1,1,k) - cc(ic-1,2,k)
                  ch(i,k,1) = cc(i,1,k) - cc(ic,2,k)
                  ti2 = cc(i,1,k) + cc(ic,2,k)
                  ch(i-1,k,2) = wa1(i-2)*tr2 - wa1(i-1)*ti2
                  ch(i,k,2) = wa1(i-2)*ti2 + wa1(i-1)*tr2
               end do
            end do
            if (mod(ido,2) .eq. 1) return 
         endif
         do k = 1, l1
            ch(ido,k,1) = cc(ido,1,k) + cc(ido,1,k)
            ch(ido,k,2) = -(cc(1,2,k)+cc(1,2,k))
         end do
      endif
      return 
      end 
c
      subroutine radb3(ido, l1, cc, ch, wa1, wa2)
      dimension cc(ido,3,l1), ch(ido,l1,3), wa1(*), wa2(*)
      data taur, taui/ -.5, .866025403784439/ 
      do k = 1, l1
         tr2 = cc(ido,2,k) + cc(ido,2,k)
         cr2 = cc(1,1,k) + taur*tr2
         ch(1,k,1) = cc(1,1,k) + tr2
         ci3 = taui*(cc(1,3,k)+cc(1,3,k))
         ch(1,k,2) = cr2 - ci3
         ch(1,k,3) = cr2 + ci3
      end do
      if (ido .eq. 1) return 
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
            cr2 = cc(i-1,1,k) + taur*tr2
            ch(i-1,k,1) = cc(i-1,1,k) + tr2
            ti2 = cc(i,3,k) - cc(ic,2,k)
            ci2 = cc(i,1,k) + taur*ti2
            ch(i,k,1) = cc(i,1,k) + ti2
            cr3 = taui*(cc(i-1,3,k)-cc(ic-1,2,k))
            ci3 = taui*(cc(i,3,k)+cc(ic,2,k))
            dr2 = cr2 - ci3
            dr3 = cr2 + ci3
            di2 = ci2 + cr3
            di3 = ci2 - cr3
            ch(i-1,k,2) = wa1(i-2)*dr2 - wa1(i-1)*di2
            ch(i,k,2) = wa1(i-2)*di2 + wa1(i-1)*dr2
            ch(i-1,k,3) = wa2(i-2)*dr3 - wa2(i-1)*di3
            ch(i,k,3) = wa2(i-2)*di3 + wa2(i-1)*dr3
         end do
      end do
      return 
      end 
c
      subroutine radb4(ido, l1, cc, ch, wa1, wa2, wa3)
      dimension cc(ido,4,l1), ch(ido,l1,4), wa1(*), wa2(*), wa3(*)
      data sqrt2/ 1.414213562373095/ 
      do k = 1, l1
         tr1 = cc(1,1,k) - cc(ido,4,k)
         tr2 = cc(1,1,k) + cc(ido,4,k)
         tr3 = cc(ido,2,k) + cc(ido,2,k)
         tr4 = cc(1,3,k) + cc(1,3,k)
         ch(1,k,1) = tr2 + tr3
         ch(1,k,2) = tr1 - tr4
         ch(1,k,3) = tr2 - tr3
         ch(1,k,4) = tr1 + tr4
      end do
      if (ido - 2 .ge. 0) then
         if (ido - 2 .ne. 0) then
            idp2 = ido + 2
            do k = 1, l1
               do i = 3, ido, 2
                  ic = idp2 - i
                  ti1 = cc(i,1,k) + cc(ic,4,k)
                  ti2 = cc(i,1,k) - cc(ic,4,k)
                  ti3 = cc(i,3,k) - cc(ic,2,k)
                  tr4 = cc(i,3,k) + cc(ic,2,k)
                  tr1 = cc(i-1,1,k) - cc(ic-1,4,k)
                  tr2 = cc(i-1,1,k) + cc(ic-1,4,k)
                  ti4 = cc(i-1,3,k) - cc(ic-1,2,k)
                  tr3 = cc(i-1,3,k) + cc(ic-1,2,k)
                  ch(i-1,k,1) = tr2 + tr3
                  cr3 = tr2 - tr3
                  ch(i,k,1) = ti2 + ti3
                  ci3 = ti2 - ti3
                  cr2 = tr1 - tr4
                  cr4 = tr1 + tr4
                  ci2 = ti1 + ti4
                  ci4 = ti1 - ti4
                  ch(i-1,k,2) = wa1(i-2)*cr2 - wa1(i-1)*ci2
                  ch(i,k,2) = wa1(i-2)*ci2 + wa1(i-1)*cr2
                  ch(i-1,k,3) = wa2(i-2)*cr3 - wa2(i-1)*ci3
                  ch(i,k,3) = wa2(i-2)*ci3 + wa2(i-1)*cr3
                  ch(i-1,k,4) = wa3(i-2)*cr4 - wa3(i-1)*ci4
                  ch(i,k,4) = wa3(i-2)*ci4 + wa3(i-1)*cr4
               end do
            end do
            if (mod(ido,2) .eq. 1) return 
         endif
         do k = 1, l1
            ti1 = cc(1,2,k) + cc(1,4,k)
            ti2 = cc(1,4,k) - cc(1,2,k)
            tr1 = cc(ido,1,k) - cc(ido,3,k)
            tr2 = cc(ido,1,k) + cc(ido,3,k)
            ch(ido,k,1) = tr2 + tr2
            ch(ido,k,2) = sqrt2*(tr1 - ti1)
            ch(ido,k,3) = ti2 + ti2
            ch(ido,k,4) = -sqrt2*(tr1 + ti1)
         end do
      endif
      return 
      end 
c
      subroutine radb5(ido, l1, cc, ch, wa1, wa2, wa3, wa4)
      dimension cc(ido,5,l1),ch(ido,l1,5),wa1(*),wa2(*),wa3(*),wa4(*)
      data tr11, ti11, tr12, ti12/ .309016994374947, .951056516295154, 
     1   -.809016994374947, .587785252292473/ 
      do k = 1, l1
         ti5 = cc(1,3,k) + cc(1,3,k)
         ti4 = cc(1,5,k) + cc(1,5,k)
         tr2 = cc(ido,2,k) + cc(ido,2,k)
         tr3 = cc(ido,4,k) + cc(ido,4,k)
         ch(1,k,1) = cc(1,1,k) + tr2 + tr3
         cr2 = cc(1,1,k) + tr11*tr2 + tr12*tr3
         cr3 = cc(1,1,k) + tr12*tr2 + tr11*tr3
         ci5 = ti11*ti5 + ti12*ti4
         ci4 = ti12*ti5 - ti11*ti4
         ch(1,k,2) = cr2 - ci5
         ch(1,k,3) = cr3 - ci4
         ch(1,k,4) = cr3 + ci4
         ch(1,k,5) = cr2 + ci5
      end do
      if (ido .eq. 1) return 
      idp2 = ido + 2
      do k = 1, l1
         do i = 3, ido, 2
            ic = idp2 - i
            ti5 = cc(i,3,k) + cc(ic,2,k)
            ti2 = cc(i,3,k) - cc(ic,2,k)
            ti4 = cc(i,5,k) + cc(ic,4,k)
            ti3 = cc(i,5,k) - cc(ic,4,k)
            tr5 = cc(i-1,3,k) - cc(ic-1,2,k)
            tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
            tr4 = cc(i-1,5,k) - cc(ic-1,4,k)
            tr3 = cc(i-1,5,k) + cc(ic-1,4,k)
            ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
            ch(i,k,1) = cc(i,1,k) + ti2 + ti3
            cr2 = cc(i-1,1,k) + tr11*tr2 + tr12*tr3
            ci2 = cc(i,1,k) + tr11*ti2 + tr12*ti3
            cr3 = cc(i-1,1,k) + tr12*tr2 + tr11*tr3
            ci3 = cc(i,1,k) + tr12*ti2 + tr11*ti3
            cr5 = ti11*tr5 + ti12*tr4
            ci5 = ti11*ti5 + ti12*ti4
            cr4 = ti12*tr5 - ti11*tr4
            ci4 = ti12*ti5 - ti11*ti4
            dr3 = cr3 - ci4
            dr4 = cr3 + ci4
            di3 = ci3 + cr4
            di4 = ci3 - cr4
            dr5 = cr2 + ci5
            dr2 = cr2 - ci5
            di5 = ci2 - cr5
            di2 = ci2 + cr5
            ch(i-1,k,2) = wa1(i-2)*dr2 - wa1(i-1)*di2
            ch(i,k,2) = wa1(i-2)*di2 + wa1(i-1)*dr2
            ch(i-1,k,3) = wa2(i-2)*dr3 - wa2(i-1)*di3
            ch(i,k,3) = wa2(i-2)*di3 + wa2(i-1)*dr3
            ch(i-1,k,4) = wa3(i-2)*dr4 - wa3(i-1)*di4
            ch(i,k,4) = wa3(i-2)*di4 + wa3(i-1)*dr4
            ch(i-1,k,5) = wa4(i-2)*dr5 - wa4(i-1)*di5
            ch(i,k,5) = wa4(i-2)*di5 + wa4(i-1)*dr5
         end do
      end do
      return 
      end 
c
      subroutine radbg(ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa)
      dimension ch(ido,l1,ip), cc(ido,ip,l1), c1(ido,l1,ip), c2(idl1,ip)
     1   , ch2(idl1,ip), wa(*)
      tpi = 2.0*pimach(dum)
      arg = tpi/float(ip)
      dcp = cos(arg)
      dsp = sin(arg)
      idp2 = ido + 2
      nbd = (ido - 1)/2
      ipp2 = ip + 2
      ipph = (ip + 1)/2
      if (ido .ge. l1) then
         do k = 1, l1
            do i = 1, ido
               ch(i,k,1) = cc(i,1,k)
            end do
         end do
      else
         do i = 1, ido
            do k = 1, l1
               ch(i,k,1) = cc(i,1,k)
            end do
         end do
      endif
      do j = 2, ipph
         jc = ipp2 - j
         j2 = j + j
         do k = 1, l1
            ch(1,k,j) = cc(ido,j2-2,k) + cc(ido,j2-2,k)
            ch(1,k,jc) = cc(1,j2-1,k) + cc(1,j2-1,k)
         end do
      end do
      if (ido .ne. 1) then
         if (nbd .ge. l1) then
            do j = 2, ipph
               jc = ipp2 - j
               do k = 1, l1
                  do i = 3, ido, 2
                     ic = idp2 - i
                     ch(i-1,k,j) = cc(i-1,2*j-1,k) + cc(ic-1,2*j-2,k)
                     ch(i-1,k,jc) = cc(i-1,2*j-1,k) - cc(ic-1,2*j-2,k)
                     ch(i,k,j) = cc(i,2*j-1,k) - cc(ic,2*j-2,k)
                     ch(i,k,jc) = cc(i,2*j-1,k) + cc(ic,2*j-2,k)
                  end do
               end do
            end do
         else
            do j = 2, ipph
               jc = ipp2 - j
               do i = 3, ido, 2
                  ic = idp2 - i
                  do k = 1, l1
                     ch(i-1,k,j) = cc(i-1,2*j-1,k) + cc(ic-1,2*j-2,k)
                     ch(i-1,k,jc) = cc(i-1,2*j-1,k) - cc(ic-1,2*j-2,k)
                     ch(i,k,j) = cc(i,2*j-1,k) - cc(ic,2*j-2,k)
                     ch(i,k,jc) = cc(i,2*j-1,k) + cc(ic,2*j-2,k)
                  end do
               end do
            end do
         endif
      endif
      ar1 = 1.
      ai1 = 0.
      do l = 2, ipph
         lc = ipp2 - l
         ar1h = dcp*ar1 - dsp*ai1
         ai1 = dcp*ai1 + dsp*ar1
         ar1 = ar1h
         do ik = 1, idl1
            c2(ik,l) = ch2(ik,1) + ar1*ch2(ik,2)
            c2(ik,lc) = ai1*ch2(ik,ip)
         end do
         dc2 = ar1
         ds2 = ai1
         ar2 = ar1
         ai2 = ai1
         do j = 3, ipph
            jc = ipp2 - j
            ar2h = dc2*ar2 - ds2*ai2
            ai2 = dc2*ai2 + ds2*ar2
            ar2 = ar2h
            do ik = 1, idl1
               c2(ik,l) = c2(ik,l) + ar2*ch2(ik,j)
               c2(ik,lc) = c2(ik,lc) + ai2*ch2(ik,jc)
            end do
         end do
      end do
      do j = 2, ipph
         do ik = 1, idl1
            ch2(ik,1) = ch2(ik,1) + ch2(ik,j)
         end do
      end do
      do j = 2, ipph
         jc = ipp2 - j
         do k = 1, l1
            ch(1,k,j) = c1(1,k,j) - c1(1,k,jc)
            ch(1,k,jc) = c1(1,k,j) + c1(1,k,jc)
         end do
      end do
      if (ido .ne. 1) then
         if (nbd .ge. l1) then
            do j = 2, ipph
               jc = ipp2 - j
               do k = 1, l1
                  do i = 3, ido, 2
                     ch(i-1,k,j) = c1(i-1,k,j) - c1(i,k,jc)
                     ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
                     ch(i,k,j) = c1(i,k,j) + c1(i-1,k,jc)
                     ch(i,k,jc) = c1(i,k,j) - c1(i-1,k,jc)
                  end do
               end do
            end do
         else
            do j = 2, ipph
               jc = ipp2 - j
               do i = 3, ido, 2
                  do k = 1, l1
                     ch(i-1,k,j) = c1(i-1,k,j) - c1(i,k,jc)
                     ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
                     ch(i,k,j) = c1(i,k,j) + c1(i-1,k,jc)
                     ch(i,k,jc) = c1(i,k,j) - c1(i-1,k,jc)
                  end do
               end do
            end do
         endif
      endif
      if (ido .eq. 1) return 
      do ik = 1, idl1
         c2(ik,1) = ch2(ik,1)
      end do
      do j = 2, ip
         do k = 1, l1
            c1(1,k,j) = ch(1,k,j)
         end do
      end do
      if (nbd .le. l1) then
         is = -ido
         do j = 2, ip
            is = is + ido
            idij = is
            do i = 3, ido, 2
               idij = idij + 2
               do k = 1, l1
                  c1(i-1,k,j)=wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
                  c1(i,k,j)=wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
               end do
            end do
         end do
      else
         is = -ido
         do j = 2, ip
            is = is + ido
            do k = 1, l1
               idij = is
               do i = 3, ido, 2
                  idij = idij + 2
                  c1(i-1,k,j)=wa(idij-1)*ch(i-1,k,j)-wa(idij)*ch(i,k,j)
                  c1(i,k,j)=wa(idij-1)*ch(i,k,j)+wa(idij)*ch(i-1,k,j)
               end do
            end do
         end do
      endif
      return 
      end 

