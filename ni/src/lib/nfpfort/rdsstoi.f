      subroutine rdsstoi (nyrstrt,nyrlast,mlon,nlat,info,sst)

c read one month/week/whatever grid at a time

c     The monthly optimum interpolation (OI) fields are derived by a linear
c     interpolation of the weekly OI fields to daily fields then averaging
c     the daily values over a month. The monthly fields are in the same
c     format and spatial resolution as the weekly fields.

      integer nyrstrt, nyrlast          ! input  (used to select data)
      integer mlon, nlat                ! input  (used to error check)
      real    sst(360,180)              ! output                    
      integer info(9)                   ! output (date info + error)
                                        !         info(9) has error info

c     Program to read WEEKLY, MONTHLY compocp site, and CLIMATOLOGY grids.
c     All grids are 1x1, (360,180), (longitude,latitude).
c
c     Each global grid is preceeded by a single header record.  The table
c     below shows the meaning of the header record variables for each of
c     the three SST data products.
c
c              IYRST IMST IDST       IYREND IMEND IDEND     NDAYS    INDEX
c           -----------------------  -------------------    ------    ----
c    WEEKLY (start date, yr/mn/day) (end date, yr/mn/day)      7        ??
c    MONTHLY    (yr/mn/1)              (yr/mn/day)          days/mn     ??
c    CLIM.      (99/mn/1)              (99/mn/day)          days/mn      0
c           --------------------------------------------------------------
c           Note: numeric constants denote fixed place-holder values
c   
c
c  Following the header are integer SST values in degrees C time 100, with
c  format (20I4).
c  These data are read into integer array ISST, converted to degree C, and 
c  placed into real array SST.
c
c  The geo-location of the SST array elements are:
c    SST(1,1)     = 179.5W, 89.5S
c    SST(1,2)     = 179.5W, 88.5S
c    SST(2,1)     = 178.5W, 89.5S
c    SST(360,180) = 179.5E, 89.5N
c
c  NOTES: 
c     1) all values less than or equal to -1.78 C are ice
c     2) a land/sea mask should be used to mask out OI SST analyzed values
c        not located in the ocean, e.g. program ls.f and data file ls.dat

      integer isst(360,180)             ! local
      character*80 fil, dir

      integer igrd
      data    igrd /0/
      save    igrd

      if (igrd.eq.0) then
          open  (unit=10,file='SSTOI'
     +          ,access='sequential',form='formatted')
      endif

      if (mlon.ne.360 .or. nlat.ne.180) then
          write (*,'('' RDSSTOI: BAD MLON, NLAT='',2i8)') mlon, nlat
          stop
      endif

c     read the header and a grid
100   read(10,'(8i5)',end=200) iyrst,imst,idst,iyrend
     +                        ,imend,idend,ndays,index
      read(10,'(20i4)',end=200) isst

      if (iyrst .lt.nyrstrt) go to 100
      if (iyrend.gt.nyrlast) then      ! no sense reading more
          do i=1,9
             info(i) = -999            ! error code -999
          enddo
          return
      endif

      do i=1,360
        do j=1,180
          sst(i,j) = 0.01*float(isst(i,j))
        enddo
      enddo

      igrd = igrd + 1

c      if (igrd.le.10) print 7,igrd,
c     * iyrst,imst,idst,iyrend,imend,idend,sst(70,80)
c    7  format ('RDSSTOI: igrd =',i3,3x,'dates =',3i3,' - ',3i3,3x
c     *        ,'sst (110.5w,10.5s) =',f6.2)

      info(1) = iyrst
      info(2) = imst
      info(3) = idst
      info(4) = iyrend
      info(5) = imend
      info(6) = idend
      info(7) = ndays
      info(8) = index
      info(9) = 0           ! use for error code
      return                                                    

  200 continue
c     write (*,'('' RDSSTOI: EOF encountered'')') 

      igrd    = 0
      info(9) = 999         ! eof
      close (10)

      return
      end       
c ---------------------------------------------------------------
      subroutine wrsstoi (sstg,ilon,jlat,nmos,nyrs
     +                   ,nyrstrt,nyrlast,twave,ier)

c write the gaussian sst grids via fortran binary write

      integer ilon,jlat,nmos,nyrs,nyrstrt,nyrlast,twave
      real sstg(ilon,jlat,nmos,nyrs)
      character*80 fil, dir

      ier  = 0
      nyr1 = nyrstrt-1900
      nyr2 = nyrlast-1900
      
      dir  = '/fs/cgd/data0/shea/ncldata_output/$'
      ldir = index (dir,'$')-1
      write (fil,'(''sstoi_'',i2,i2,''_T'',i2,''$'')') nyr1,nyr2,twave  
      lfil = index (fil,'$')-1
 
      open  (unit=10,file=dir(1:ldir) // fil(1:lfil)
     +              ,access='sequential',form='unformatted', iostat=ios)
      if (ios.ne.0) then
          write (*,'(''FORTRAN: wrsstoi: open: ios='',i4)') ios
          ier = 1
          return 
      endif

      do nyr=1,nyrs
         nyear = nyr+nyrstrt-1
        do nmo=1,nmos
           write (10) ((sstg(il,jl,nmo,nyr),il=1,ilon),jl=1,jlat)
        enddo
      enddo

      close (10)

      return
      end
         
