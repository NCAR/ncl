      SUBROUTINE DRDSSTOI(NYRSTRT,NYRLAST,MLON,NLAT,INFO,SST)

c read one month/week/whatever grid at a time

C*PL*ERROR* Comment line too long
c     The monthly optimum interpolation (OI) fields are derived by a linear
C*PL*ERROR* Comment line too long
c     interpolation of the weekly OI fields to daily fields then averaging
c     the daily values over a month. The monthly fields are in the same
c     format and spatial resolution as the weekly fields.

C input  (used to select data)
      INTEGER NYRSTRT,NYRLAST
C input  (used to error check)
      INTEGER MLON,NLAT
C output
      DOUBLE PRECISION SST(360,180)
C output (date info + error)
      INTEGER INFO(9)
C         info(9) has error info


C*PL*ERROR* Comment line too long
c     Program to read WEEKLY, MONTHLY compocp site, and CLIMATOLOGY grids.
c     All grids are 1x1, (360,180), (longitude,latitude).
c
C*PL*ERROR* Comment line too long
c     Each global grid is preceeded by a single header record.  The table
c     below shows the meaning of the header record variables for each of
c     the three SST data products.
c
C*PL*ERROR* Comment line too long
c              IYRST IMST IDST       IYREND IMEND IDEND     NDAYS    INDEX
C*PL*ERROR* Comment line too long
c           -----------------------  -------------------    ------    ----
C*PL*ERROR* Comment line too long
c    WEEKLY (start date, yr/mn/day) (end date, yr/mn/day)      7        ??
C*PL*ERROR* Comment line too long
c    MONTHLY    (yr/mn/1)              (yr/mn/day)          days/mn     ??
C*PL*ERROR* Comment line too long
c    CLIM.      (99/mn/1)              (99/mn/day)          days/mn      0
C*PL*ERROR* Comment line too long
c           --------------------------------------------------------------
c           Note: numeric constants denote fixed place-holder values
c
c
C*PL*ERROR* Comment line too long
c  Following the header are integer SST values in degrees C time 100, with
c  format (20I4).
C*PL*ERROR* Comment line too long
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
C*PL*ERROR* Comment line too long
c     2) a land/sea mask should be used to mask out OI SST analyzed values
C*PL*ERROR* Comment line too long
c        not located in the ocean, e.g. program ls.f and data file ls.dat

C local
      INTEGER ISST(360,180)
      CHARACTER*80 FIL,DIR

      INTEGER IGRD
      DATA IGRD/0/
      SAVE IGRD

      IF (IGRD.EQ.0) THEN
          OPEN (UNIT=10,FILE='SSTOI',ACCESS='sequential',
     +         FORM='formatted')
      END IF

      IF (MLON.NE.360 .OR. NLAT.NE.180) THEN
          WRITE (*,FMT='('' RDSSTOI: BAD MLON, NLAT='',2i8)') MLON,NLAT
          STOP
      END IF

c     read the header and a grid
  100 READ (10,FMT='(8i5)',END=200) IYRST,IMST,IDST,IYREND,IMEND,IDEND,
     +  NDAYS,INDEX
      READ (10,FMT='(20i4)',END=200) ISST

      IF (IYRST.LT.NYRSTRT) GO TO 100
C no sense reading more
      IF (IYREND.GT.NYRLAST) THEN
          DO I = 1,9
C error code -999
              INFO(I) = -999
          END DO
          RETURN
      END IF

      DO I = 1,360
          DO J = 1,180
              SST(I,J) = 0.01D0*DBLE(ISST(I,J))
          END DO
      END DO

      IGRD = IGRD + 1

c      if (igrd.le.10) print 7,igrd,
c     * iyrst,imst,idst,iyrend,imend,idend,sst(70,80)
c    7  format ('RDSSTOI: igrd =',i3,3x,'dates =',3i3,' - ',3i3,3x
c     *        ,'sst (110.5w,10.5s) =',f6.2)

      INFO(1) = IYRST
      INFO(2) = IMST
      INFO(3) = IDST
      INFO(4) = IYREND
      INFO(5) = IMEND
      INFO(6) = IDEND
      INFO(7) = NDAYS
      INFO(8) = INDEX
C use for error code
      INFO(9) = 0
      RETURN

  200 CONTINUE
c     write (*,'('' RDSSTOI: EOF encountered'')')

      IGRD = 0
C eof
      INFO(9) = 999
      CLOSE (10)

      RETURN
      END
c ---------------------------------------------------------------
      SUBROUTINE DWRSSTOI(SSTG,ILON,JLAT,NMOS,NYRS,NYRSTRT,NYRLAST,
     +                    TWAVE,IER)

c write the gaussian sst grids via fortran binary write

      INTEGER ILON,JLAT,NMOS,NYRS,NYRSTRT,NYRLAST,TWAVE
      DOUBLE PRECISION SSTG(ILON,JLAT,NMOS,NYRS)
      CHARACTER*80 FIL,DIR

      IER = 0
      NYR1 = NYRSTRT - 1900
      NYR2 = NYRLAST - 1900

      DIR = '/fs/cgd/data0/shea/ncldata_output/$'
      LDIR = INDEX(DIR,'$') - 1
      WRITE (FIL,FMT='(''sstoi_'',i2,i2,''_T'',i2,''$'')') NYR1,NYR2,
     +  TWAVE
      LFIL = INDEX(FIL,'$') - 1

      OPEN (UNIT=10,FILE=DIR(1:LDIR)//FIL(1:LFIL),ACCESS='sequential',
     +     FORM='unformatted',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
          WRITE (*,FMT='(''FORTRAN: wrsstoi: open: ios='',i4)') IOS
          IER = 1
          RETURN
      END IF

      DO NYR = 1,NYRS
          NYEAR = NYR + NYRSTRT - 1
          DO NMO = 1,NMOS
              WRITE (10) ((SSTG(IL,JL,NMO,NYR),IL=1,ILON),JL=1,JLAT)
          END DO
      END DO

      CLOSE (10)

      RETURN
      END
