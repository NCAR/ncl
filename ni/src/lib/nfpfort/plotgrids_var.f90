subroutine plotgrids_var(fname,plotvar)

   implicit none

   ! Parameters
   integer, parameter :: MAX_DOMAINS = 21
   integer, parameter :: NVAR = 19

   ! Variables
   integer :: i, j, funit, io_form_geogrid
   integer :: interval_seconds

   real, dimension(MAX_DOMAINS) :: parent_grid_ratio, parent_id, ixdim, jydim
   real, dimension(MAX_DOMAINS) :: i_parent_start, j_parent_start, &
                        s_we, e_we, s_sn, e_sn, &
                        start_year, start_month, start_day, start_hour, &
                        end_year,   end_month,   end_day,   end_hour
   logical, dimension(MAX_DOMAINS) :: active_grid
   logical :: is_used

   real :: stand_lon, truelat1, truelat2, &
           ref_lat, ref_lon, ref_x, ref_y, pole_lat, pole_lon
   real :: dx, dy, mproj_int, max_dom

   character (len=128) :: geog_data_path, opt_output_from_geogrid_path, opt_geogrid_tbl_path
   character (len=128), dimension(MAX_DOMAINS) :: geog_data_res
   character (len=128) :: map_proj
   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
   character (len=3) :: wrf_core
   character (*) :: fname

   integer :: debug_level

   real, dimension(NVAR,MAX_DOMAINS) :: plotvar(NVAR,MAX_DOMAINS)

   namelist /share/ wrf_core, max_dom, start_date, end_date, &
                     start_year, end_year, start_month, end_month, &
                     start_day, end_day, start_hour, end_hour, &
                     interval_seconds, io_form_geogrid, opt_output_from_geogrid_path, &
                     debug_level, active_grid
   namelist /geogrid/ parent_id, parent_grid_ratio, &
                      i_parent_start, j_parent_start, s_we, e_we, s_sn, e_sn, &
                      map_proj, ref_x, ref_y, ref_lat, ref_lon, &
                      truelat1, truelat2, stand_lon, dx, dy, pole_lat, pole_lon, &
                      geog_data_res, geog_data_path, opt_geogrid_tbl_path

   !Initialize array

   ref_x = -999.0
   ref_y = -999.0
   ref_lat = -999.0
   ref_lon = -999.0
   do i=1,NVAR
    do j=1,MAX_DOMAINS
      plotvar(i,j) = -999.0
    end do
   end do

   ! Read parameters from Fortran namelist
   do funit=10,100
      inquire(unit=funit, opened=is_used)
      if (.not. is_used) exit
   end do
   open(funit,file=fname,status='old',form='formatted')
   read(funit,share)
   read(funit,geogrid)
   close(funit)

   !Assign integers to map projections

   if (index(map_proj, 'lambert') /= 0) then
      mproj_int = 1
   else if (index(map_proj, 'mercator') /= 0) then
      mproj_int = 2
   else if (index(map_proj, 'polar') /= 0) then
      mproj_int = 3
   else if (index(map_proj, 'lat-lon') /= 0) then
      mproj_int = 4
   end if

   !Put all the variables into an array

   plotvar(1,1) = max_dom
   plotvar(2,1) = dx
   plotvar(3,1) = dy
   plotvar(4,1) = ref_lat
   plotvar(5,1) = ref_lon
   plotvar(6,1) = ref_x
   plotvar(7,1) = ref_y
   plotvar(8,1) = truelat1
   plotvar(9,1) = truelat2
   plotvar(10,1) = stand_lon
   plotvar(11,1) = mproj_int
   plotvar(12,1) = pole_lat
   plotvar(13,1) = pole_lon
   do j=1,int(max_dom)
     plotvar(14,j) = parent_id(j)
     plotvar(15,j) = parent_grid_ratio(j)
     plotvar(16,j) = i_parent_start(j)
     plotvar(17,j) = j_parent_start(j)
     plotvar(18,j) = e_we(j)
     plotvar(19,j) = e_sn(j)
   end do

end subroutine plotgrids_var
