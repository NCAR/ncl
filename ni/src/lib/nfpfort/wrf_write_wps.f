C NCLFORTSTART
      SUBROUTINE write_intermediate_wps(OUTPUT_NAME,FIELDIN,UNITSIN,
     +  DESCIN,HDATEIN,DATA_SOURCE,XLVL,IPROJ,START_LOCATION,STARTLAT,
     +  STARTLON,DELTALAT,DELTALON,XLONC,TRUELAT1,TRUELAT2, NLATS,DX,DY,
     +  NX,NY,IS_WIND_EARTH_REL,IFV,XFCST,EARTH_RADIUS,SLAB)

      character*(*) OUTPUT_NAME
      character*(*) FIELDIN,UNITSIN,DESCIN,HDATEIN
      character*(*) DATA_SOURCE,START_LOCATION
      integer IPROJ
      integer NX
      integer NY
      real DELTALAT
      real DELTALON
      real DX
      real DY
      real NLATS
      real STARTLAT
      real STARTLON
      real TRUELAT1
      real TRUELAT2
      real XLONC
      real XLVL
      logical IS_WIND_EARTH_REL
      integer IFV
      real XFCST
      real EARTH_RADIUS 
      real SLAB(NX,NY)

C NCLEND


C Declarations INTERNAL

      character(len=9)  FIELD
      character(len=25) UNITS
      character(len=46) DESC
      character(len=24) HDATE
      character(len=32) MAP_SOURCE 
      character(len=8)  STARTLOC
      character(len=80) OUT_FILE 
      
      integer IUNIT 
      integer OUNIT 
      integer ierr
      logical exists


CCC Made these part of calling routine
CCC      IFV=5
CCC      XFCST=0.0
CCC      EARTH_RADIUS = 6367470. * .001
      IUNIT = 10
      OUNIT = 11

      FIELD      = FIELDIN
      UNITS      = UNITSIN
      DESC       = DESCIN
      HDATE      = HDATEIN
      MAP_SOURCE = DATA_SOURCE
      STARTLOC   = START_LOCATION


C Create the WPS IM file name and test it it is already in use
      OUT_FILE = TRIM(OUTPUT_NAME)//":"//HDATE(1:13)

      inquire(file=OUT_FILE, exist=exists)
      if (exists) then
        open(OUNIT, file=OUT_FILE, status="old", 
     +       position="append", action="write", form='unformatted', 
     +       convert='big_endian')
      else
        open(OUNIT, file=OUT_FILE, status="new", action="write", 
     +       form='unformatted', convert='big_endian')
      end if


C WRITE WPS IM identifier - we always use the newer versions so this is always 5
      write (OUNIT, IOSTAT=IERR) IFV


C WRITE the second record, common to all projections:
      write (OUNIT) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, 
     +       NX, NY, IPROJ


C WRITE the third record, which depends on the projection:
      if (IPROJ == 0) then            
C This is the Cylindrical Equidistant (lat/lon) projection:
        WRITE (OUNIT) STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, 
     +       EARTH_RADIUS

      elseif (IPROJ == 1) then        
C This is the Mercator projection:
        WRITE (OUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, 
     +       EARTH_RADIUS

      elseif (IPROJ == 3) then        
C This is the Lambert Conformal projection:
         WRITE (OUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, 
     +       TRUELAT1, TRUELAT2, EARTH_RADIUS

      elseif (IPROJ == 4) then        
C Gaussian projection                         
        WRITE (OUNIT) STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, 
     +       EARTH_RADIUS
        
      elseif (IPROJ == 5) then        
C This is the Polar Stereographic projection:
        WRITE (OUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, 
     +       TRUELAT1, EARTH_RADIUS

      endif
     
      WRITE (OUNIT) IS_WIND_EARTH_REL

      WRITE (OUNIT) slab
      WRITE(*, '("DONE WRITING ", A, " (level=", f7.0, ") for time ", 
     +       A, " to IM Format")' ) 
     +      TRIM(FIELD), XLVL, TRIM(HDATE)

      close(ounit)

      RETURN
      END
