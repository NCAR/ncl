c
c    tests subroutines shpei and shpe in file shpe.f
c    also requires files  shaec.f shsec.f sphcom.f and hrfft.f
c
c    shpe is the n**2 filter with complement, odd/even
c    factorization and zero truncation on an equally spaced
c    grid. The projection is defined in the JCP paper "Generalized
c    discrete spherical harmonic transforms" by 
c    Paul N. Swarztrauber and William F. Spotz
c    J. Comp. Phys., 159(2000) pp. 213-230.
c
c                     April 2002
c
      program tshpe
      parameter (idp=32)
      parameter (kdp=idp+idp-2)
      parameter (lwshp=2*(idp+1)**2+kdp+20,
     1    liwshp=4*(idp+1),lwrk=1.25*(idp+1)**2+7*idp+8)
      parameter (lwrk1=idp*kdp)
      parameter(lwork = 5*idp*(idp-1),
     1 lwsha=idp*(idp+1)+3*(idp-2)*(idp-1)/2+kdp+15)
      double precision work(lwrk)
      dimension sx(idp,kdp),sy(idp,kdp),
     1wshp(lwshp),iwshp(liwshp),wrk1(lwrk1)
      dimension g(idp,kdp,2),ga(idp,idp,2),gb(idp,idp,2),
     1          gh(idp,kdp,2),gw(idp,kdp,2),
     2          wrk2(lwork),wshaec(lwsha),wshsec(lwsha)
      dimension t1(2)
c
      iprint = 0
      nt = 1
c
      do 100 nlat=6,8
      do 100 mtr=1,3
      mtrunc = nlat-mtr
      idimg = idp
      jdimg = kdp
      nlon = 2*(nlat-1)
      mtrunc = min(mtrunc,nlat-1,nlon/2)
      call shaeci(nlat,nlon,wshaec,lwsha,work,lwrk,ierror)
      if(ierror .ne. 0) write(6,70) ierror
   70 format('   ierror0' ,i5)
c
      lwshs = lwsha
      call shseci(nlat,nlon,wshsec,lwshs,work,lwrk,ierror)
      if(ierror .ne. 0) write(6,71) ierror
   71 format('   ierror1' ,i5)
c
      mode = 0
      if(iprint .ne. 0) write (6,5) mode,nlat,nlon
    5 format(' mode =' ,i5,'  nlat =',i5,'  nlon =',i5)
c
      do 10 k=1,nt
      do 10 i=1,nlat
      do 10 j=1,nlon
      g(i,j,k) = cos(float(i*j))
      gh(i,j,k) = g(i,j,k)
   10 continue
c
      thold = etime(t1)
      thold = t1(1)
      call shaec(nlat,nlon,mode,nt,g,idimg,jdimg,ga,gb,idimg,idimg,
     1              wshaec,lwsha,wrk2,lwork,ierror)
      if(ierror .ne. 0) write(6,72) ierror
   72 format('   ierror2' ,i5)
c
      if(mtrunc.lt.nlat-1) then
      do np1=mtrunc+2,nlat
       do mp1=1,np1
        ga(mp1,np1,1) = 0.
        gb(mp1,np1,1) = 0.
       end do
      end do
      end if
      call shsec(nlat,nlon,mode,nt,gw,idimg,jdimg,ga,gb,idimg,idimg,
     1              wshsec,lwshs,wrk2,lwork,ierror)
      tusl = etime(t1)
      tusl = t1(1)-thold
      if(ierror .ne. 0) write(6,73) ierror
   73 format('   ierror3' ,i5)
c
c     faster filter
c
      isym = 0
c      mtrunc = nlat-1
      call shpei(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,liwshp,
     1 work,lwrk,ierror)
c
      if(ierror.ne.0) write(*,429) ierror
 429  format(' ierror =',i5,' at 429')
      do j=1,nlon
      do i=1,nlat
      sx(i,j) = gh(i,j,1)
      end do
      end do
c
c      do i=1,nlat
c       write(*,427) i,(sx(i,j),j=1,nlon)      
c 427   format(i5,1p4e15.6/(5x,1p4e15.6))
c      end do
      thold = etime(t1)
      toe = t1(1)
      call shpe(nlat,nlon,isym,mtrunc,sx,sy,idp,
     1  wshp,lwshp,iwshp,liwshp,wrk1,lwrk1,ierror)
      thold = etime(t1)
      toe = t1(1)-toe
      if(ierror.ne.0) write(*,428) ierror
 428  format(' ierror =',i5,' at 428')
      if(iprint.gt.0)write(*,431)
 431  format(/' approx and exact solution'/)
      do j=1,nlon
      if(iprint.gt.0)write(*,437) j,(sy(i,j),gw(i,j,1),i=1,nlat)
 437  format(' j=',i5,1p4e15.6/(8x,1p4e15.6))
      end do
      dmax1 = 0.
      do j=1,nlon
      do i=1,nlat
      dmax1 = max(dmax1,abs(sy(i,j)-gw(i,j,1)))
      end do
      end do
      write (6,134) nlat,mtrunc
 134  format(/'case nlat =',i5,' and mtrunc =',i5/)
      write(*,135) dmax1,tusl,toe
 135  format(/' error =',1pe15.6/
     1        ' tusl  =',1pe15.6/
     2        ' toe   =',1pe15.6)
  100 continue
      end

