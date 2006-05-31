c
c    tests subroutines shpgi and shpg in file shpg.f
c    also requires files  shagc.f, shsgc.f, gaqd.f 
c    sphcom.f and hrfft.f 
c
c    shpg is the n**2 projection with complement, odd/even
c    factorization and zero truncation on a Gaussian distributed
c    grid. The projection is defined in the JCP paper "Generalized
c    discrete spherical harmonic transforms" by 
c    Paul N. Swarztrauber and William F. Spotz
c    J. Comp. Phys., 159(2000) pp. 213-230.
c
c                     April 2002
c
      program tshpg
      parameter (idp=8)
      parameter (kdp=idp+idp-2)
      parameter (lwshp=2*(idp+1)**2+kdp+20,
     1    liwshp=4*(idp+1),lwrk=1.25*(idp+1)**2+7*idp+8)
      parameter (lwrk1=idp*kdp)
      parameter(lwork = 4*idp*(idp-1),
     1 lwsha=idp*(4*idp+1)+idp+idp+15)
c     1 lwsha=idp*(idp+1)+3*(idp-2)*(idp-1)/2+kdp+15)

      double precision work(lwrk)
      dimension sx(idp,kdp),sy(idp,kdp),
     1wshp(lwshp),iwshp(liwshp),wrk1(lwrk1)
      dimension g(idp,kdp,2),ga(idp,idp,2),gb(idp,idp,2),
     1 gh(idp,kdp,2),
     2 wrk2(lwork),wshagc(lwsha),wshsgc(lwsha)
      dimension t1(2)
c
      iprint = 0
      nt = 1
      isym = 0
      mode = 0
c
      do 100 nlat=6,8
      do 200 mtr=1,2
      nlon = 2*(nlat-1)
      mtrunc = nlat-mtr
      mtrunc = min(mtrunc,nlat-1,nlon/2)
      idimg = idp
      jdimg = kdp
      call shagci(nlat,nlon,wshagc,lwsha,work,lwrk,ierror)
      if(ierror .ne. 0) write(6,70) ierror
   70 format('   ierror0' ,i5)
c
      lwshs = lwsha
      call shsgci(nlat,nlon,wshsgc,lwshs,work,lwrk,ierror)
      if(ierror .ne. 0) write(6,71) ierror
   71 format('   ierror1' ,i5)
c
c     initiate faster filter
c
      call shpgi(nlat,nlon,isym,mtrunc,wshp,lwshp,iwshp,liwshp,
     1 work,lwrk,ierror)
      if(ierror.ne.0) write(*,429) ierror
 429  format(' ierror2 =',i5,' at 429')
c
      if(iprint .ne. 0) write (6,5) mode,nlat,nlon
    5 format(' mode =' ,i5,'  nlat =',i5,'  nlon =',i5)
c
c     initialize with pseudo random field
c
      do 10 i=1,nlat
      do 10 j=1,nlon
      sx(i,j) =  cos(float(i*j))
      g(i,j,1) = sx(i,j)
   10 continue
c
      thold = etime(t1)
      thold = t1(1)
      call shagc(nlat,nlon,mode,nt,g,idimg,jdimg,ga,gb,idimg,idimg,
     1              wshagc,lwsha,wrk2,lwork,ierror)
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
      call shsgc(nlat,nlon,mode,nt,gh,idimg,jdimg,ga,gb,idimg,idimg,
     1              wshsgc,lwshs,wrk2,lwork,ierror)
      tusl = etime(t1)
      tusl = t1(1)-thold
      if(ierror .ne. 0) write(6,73) ierror
   73 format('   ierror3' ,i5)
c
      thold = etime(t1)
      toe = t1(1)
      call shpg(nlat,nlon,isym,mtrunc,sx,sy,idp,
     1  wshp,lwshp,iwshp,liwshp,wrk1,lwrk1,ierror)
      thold = etime(t1)
      toe = t1(1)-toe
      if(ierror.ne.0) write(*,428) ierror
 428  format(' ierror =',i5,' at 428')
      if(iprint.gt.0)write(*,431)
 431  format(/' approx and exact solution'/)
      do j=1,nlon
      if(iprint.gt.0)write(*,437) j,(sy(i,j),gh(i,j,1),i=1,nlat)
 437  format(' j=',i5,1p4e15.6/(8x,1p4e15.6))
      end do
      dmax1 = 0.
      do j=1,nlon
      do i=1,nlat
      dmax1 = max(dmax1,abs(sy(i,j)-gh(i,j,1)))
      end do
      end do
      write (6,134) nlat,mtrunc
 134  format(/'case nlat =',i5,' and mtrunc =',i5/)
      write(*,135) dmax1,tusl,toe
 135  format(/' error =',1pe15.6/
     1        ' tusl  =',1pe15.6/
     2        ' toe   =',1pe15.6)
c
  200 continue
  100 continue
      end

