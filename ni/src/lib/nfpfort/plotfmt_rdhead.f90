subroutine plotfmt_rdhead(istatus,head_real,field,hdate, &
                          units, map_source, desc)

  implicit none

  integer, parameter :: MAX_VAR = 14
  integer :: funit, i
  integer, intent(inout) :: istatus

  integer :: version, nx, ny, iproj
  real :: nlats, xfcst, xlvl, startlat, startlon, deltalat, deltalon
  real :: dx, dy, xlonc, truelat1, truelat2, earth_radius
  logical :: is_wind_grid_rel
  character (len = 8) :: startloc
  character (len = 9) :: field
  character (len = 24) :: hdate
  character (len = 25) :: units
  character (len = 32) :: map_source
  character (len = 46) :: desc

  real, dimension(MAX_VAR) :: head_real(MAX_VAR)

  funit = 10
  istatus = 1

  do i=1,MAX_VAR
    head_real(i) = -999.0
  end do

  read(unit=funit,end=1001) version
  read(unit=funit) hdate, xfcst, map_source, field, &
                   units, desc, xlvl, nx, ny, iproj

  if(iproj == 0) then
    read(unit=funit,err=1001,end=1001) startloc, startlat, startlon, &
                                       deltalat, deltalon, earth_radius

  else if(iproj == 1) then
    read(unit=funit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, &
                                       truelat1, earth_radius

  else if(iproj == 3) then
    read(unit=funit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, &
                                       xlonc, truelat1, truelat2, earth_radius

  else if(iproj == 4) then
    read(unit=funit,err=1001,end=1001) startloc, startlat, startlon, &
                                       nlats, deltalon, earth_radius
  else if(iproj == 5) then
    read(unit=funit,err=1001,end=1001) startloc, startlat, startlon, dx, dy, &
                                       xlonc, truelat1, earth_radius
  end if

  read(unit=funit,err=1001,end=1001) is_wind_grid_rel

  head_real(1) = version
  head_real(2) = xfcst
  head_real(3) = xlvl
  head_real(4) = nx
  head_real(5) = ny
  head_real(6) = iproj
  if(iproj == 0) then
    head_real(7) = startlat
    head_real(8) = startlon
    head_real(9) = deltalat
    head_real(10) = deltalon
    head_real(11) = earth_radius
  else if(iproj == 1) then
    head_real(7) = startlat
    head_real(8) = startlon
    head_real(9) = dx
    head_real(10) = dy
    head_real(11) = truelat1
    head_real(12) = earth_radius
  else if(iproj == 3) then
    head_real(7) = startlat
    head_real(8) = startlon
    head_real(9) = dx
    head_real(10) = dy
    head_real(11) = xlonc
    head_real(12) = truelat1
    head_real(13) = truelat2
    head_real(14) = earth_radius
  else if(iproj == 4) then
    head_real(7) = startlat
    head_real(8) = startlon
    head_real(9) = nlats
    head_real(10) = deltalon
    head_real(11) = earth_radius
  else if(iproj == 5) then
    head_real(7) = startlat
    head_real(8) = startlon
    head_real(9) = dx
    head_real(10) = dy
    head_real(11) = xlonc
    head_real(12) = truelat1
    head_real(13) = earth_radius
  end if

  istatus = 0

  return

   1001 return

  istatus = 1

end subroutine plotfmt_rdhead
