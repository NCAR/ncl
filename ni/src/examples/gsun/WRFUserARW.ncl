load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;--------------------------------------------------------------------------------
; function wrf_user_set_xy( var:numeric, xp:numeric, yp:numeric, x1:numeric, \
;                           y1:numeric,angle:numeric ,opts )
; function wrf_user_intrp3d( var3d:numeric, z:numeric, plot_type:string, \
;                            loc_param:numeric, angle:numeric, opts:logical )
; function wrf_user_intrp2d( var2d:numeric, \
;                            loc_param:numeric, angle:numeric, opts:logical )
; function wrf_user_getvar( nc_file:file, variable:string, time:integer )
; function wrf_user_list_times( nc_file:file )
; function wrf_user_latlon_to_ij( nc_file:file, latitude:numeric, 
;                                 longitude:numeric )
; function wrf_user_ll_to_ij( nc_file:file, latitude:numeric, longitude:numeric \
;                             opts:logical )
; function wrf_contour(nc_file:file,wks[1]: graphic, data[*][*]:numeric, \
;                      opt_args[1]:logical)
; function wrf_vector(nc_file:file,wks[1]: graphic, data_u[*][*]:numeric, \
;                     data_v[*][*]:numeric, opt_args[1]:logical)
; function wrf_map(wks[1]:graphic,in_file[1]:file,opt_args[1]:logical)   
; function wrf_map_overlays(in_file[1]:file,wks:graphic,base[1]:graphic, \
;                           plots[*]:graphic,opt_arg[1]:logical,mp_arg[1]:logical)
; function wrf_overlays(in_file[1]:file,wks:graphic, plots[*]:graphic, \
;                       opt_arg[1]:logical)
;
; These next three functions are obsolete as of version 5.0.1 of NCL.
; Do not use them. Use wrf_overlays and wrf_map_overlays instead.
;
; function wrf_map_zoom(wks[1]:graphic,in_file[1]:file,opt_args[1]:logical, \
;                       y1:integer,y2:integer,x1:integer,x2:integer)
; procedure wrf_map_overlay(wks:graphic,base[1]:graphic, \
;                           plots[*]:graphic, \
;                           opt_arg[1]:logical)
; procedure wrf_overlay(wks:graphic, plots[*]:graphic, \
;                           opt_arg[1]:logical)
;
; function add_white_space(str:string,maxlen:integer)
; procedure print_opts(opts_name,opts,debug)
; procedure print_header(icount:integer,debug)
;--------------------------------------------------------------------------------
 
;***********************************************************************;
; function : tointeger                                                  ;
;                x:numeric                                              ;
;                                                                       ;
; Convert input to integer.                                             ;
;                                                                       ;
;***********************************************************************;
undef("tointeger")
function tointeger(x:numeric)
local xi
begin
  if(typeof(x).eq."double")
    xi = doubletointeger(x)
  else
    if(typeof(x).eq."float")
      xi = floattointeger(x)
    else
      if(isatt(x,"_FillValue"))
        xi = new(dimsizes(x),integer,x@_FillValue)
      else
        xi = new(dimsizes(x),integer)
        delete(xi@_FillValue)
      end if
      xi = x
    end if
  end if
  return(xi)
end

undef("wrf_user_set_xy")
function wrf_user_set_xy( var:numeric, xp:numeric, yp:numeric, x1:numeric, \
                          y1:numeric, angle:numeric, opts )

;  mass coordinate version of ncl user routines

local dims,x,y,slope,intercept,distance,dx,dy,dxy,npts,xy

begin

; find intersection of line and domain boundaries

  dims = dimsizes(var)

  if (.not. opts) then        ; We have a pivot point and location and 
			      ; need to calculate the start and end points of 
			      ; the cross section

     if ((angle .gt. 315.) .or. (angle .lt. 45.) .or. \
         ((angle .gt. 135.) .and. (angle .lt. 225.)) )  then

;  x = y*slope + intercept

          slope = -(360.-angle)/45.
          if( angle .lt. 45. ) then
            slope = angle/45.
          end if
          if( angle .gt. 135.) then
            slope = (angle-180.)/45.
          end if
          intercept = xp - yp*slope

; find intersections with domain boundaries

          y0 = 0.
          x0 = y0*slope + intercept
   
          if( x0 .lt. 0.) then  ; intersect outside of left boundary
            x0 = 0.
            y0 =  (x0 - intercept)/slope
          end if
          if( x0 .gt. dims(2)-1) then  ; intersect outside of right boundary
            x0 = dims(2)-1
            y0 =  (x0 - intercept)/slope
          end if
   
          y1 = dims(1)-1.  ;  need to make sure this will be a float?
          x1 = y1*slope + intercept
   
          if( x1 .lt. 0.) then  ; intersect outside of left boundary
            x1 = 0.
            y1 =  (x1 - intercept)/slope
          end if
          if( x1 .gt. dims(2)-1) then  ; intersect outside of right boundary
            x1 = dims(2)-1
            y1 =  (x1 - intercept)/slope
          end if

     else

;  y = x*slope + intercept

          slope = (90.-angle)/45.
          if( angle .gt. 225. ) then
            slope = (270.-angle)/45.
          end if
          intercept = yp - xp*slope

; find intersections with domain boundaries

          x0 = 0.
          y0 = x0*slope + intercept
   
          if( y0 .lt. 0.) then  ; intersect outside of bottom boundary
            y0 = 0.
            x0 =  (y0 - intercept)/slope
          end if
          if( y0 .gt. dims(1)-1) then  ; intersect outside of top boundary
            y0 = dims(1)-1
            x0 =  (y0 - intercept)/slope
          end if

          x1 = dims(2)-1.  ;  need to make sure this will be a float?
          y1 = x1*slope + intercept
   
          if( y1 .lt. 0.) then  ; intersect outside of bottom boundary
            y1 = 0.
            x1 =  (y1 - intercept)/slope
          end if
          if( y1 .gt. dims(1)-1) then  ; intersect outside of top boundary
            y1 = dims(1)-1
            x1 =  (y1 - intercept)/slope
          end if
   
     end if  ; we have beginning and ending points

  end if


  if (opts) then   	; We have a specified start and end point
    x0 = xp
    y0 = yp
    if ( x1 .gt. dims(2)-1 ) then 
      x1 = dims(2)
    end if
    if ( y1 .gt. dims(1)-1 ) then 
      y1 = dims(1)
    end if
  end if

  dx = x1 - x0
  dy = y1 - y0
  distance = (dx*dx + dy*dy)^0.5
  npts = tointeger(distance)
  dxy = new(1,typeof(distance))
  dxy = distance/npts

  xy = new((/ npts, 2 /),typeof(x1))

  dx = dx/npts
  dy = dy/npts

  do i=0,npts-1
    xy(i,0) = x0 + i*dx
    xy(i,1) = y0 + i*dy
  end do

; print(xy)
  return(xy)

end

;--------------------------------------------------------------------------------
 
undef("wrf_user_intrp3d")
function wrf_user_intrp3d( var3d:numeric, z_in:numeric, \
                           plot_type:string, \
                           loc_param:numeric, angle:numeric, opts:logical )

; var3d      - 3d field to interpolate (all input fields must be unstaggered)
; z_in       - interpolate to this field (either p/z)
; plot_type  - interpolate horizontally "h", or vertically "v"
; loc_param  - level(s) for horizontal plots (eg. 500hPa ; 3000m - scalar), 
;              plane for vertical plots (2 values representing an xy point 
;              on the model domain through which the vertical plane will pass
;              OR 4 values specifying start and end values
; angle      - 0.0 for horizontal plots, and 
;              an angle for vertical plots - 90 represent a WE cross section
; opts         Used IF opts is TRUE, else use loc_param and angle to determine crosssection

begin

  if(plot_type .eq. "h" ) then   ;  horizontal cross section needed

     dim_levs = dimsizes(loc_param)
     dims = dimsizes(var3d)
     nd = dimsizes(dims)


     dimX = dims(nd-1)
     dimY = dims(nd-2)
     dimZ = dims(nd-3)
     dimL = dim_levs
     dim4 = 1
     dim5 = 1
     if ( nd .eq. 4 ) then
       dim4 = dims(nd-4)
     end if
     if ( nd .eq. 5 ) then
       dim4 = dims(nd-4)
       dim5 = dims(nd-5)
     end if
     var3  = new ( (/ dim5, dim4, dimZ, dimY, dimX /) , typeof(var3d) )
     z     = new ( (/ dim5, dim4, dimZ, dimY, dimX /) , typeof(var3d) )
     var2d = new ( (/ dim5, dim4, dimL, dimY, dimX /) , typeof(var3d) )

     if ( nd .eq. 5 ) then
       var3 = var3d
          z =  z_in
     end if
     if ( nd .eq. 4 ) then
       var3(0,:,:,:,:) = var3d(:,:,:,:)
          z(0,:,:,:,:) =  z_in(:,:,:,:)
     end if
     if ( nd .eq. 3 ) then
       var3(0,0,:,:,:) = var3d(:,:,:)
          z(0,0,:,:,:) =  z_in(:,:,:)
     end if


     if ( z(0,0,0,0,0) .gt. 500.) then
       ; We must be interpolating to pressure
       ; This routine needs input field and level in hPa - lets make sure of this
       if ( z(0,0,0,0,0) .gt. 2000. ) then
         ; looks like we have Pa as input - make this hPa
         z = z * 0.01
       end if
       if ( loc_param(0) .gt. 2000. ) then
         ; looks like the input was specified in Pa - change this
         loc_param = loc_param * 0.01
       end if
     end if



     do i5 = 0,dim5-1
       do i4 = 0,dim4-1
         do il = 0,dimL-1
           var = wrf_interp_3d_z(var3(i5,i4,:,:,:),z(i5,i4,:,:,:),loc_param(il))
           var2d(i5,i4,il,:,:) = var(:,:)
         end do
       end do
     end do


     nn = nd-1
     var2d!4 = var3d!nn
     nn = nd-2
     var2d!3 = var3d!nn
     var2d!2 = "plevs"
     if ( nd .ge. 4 ) then
       nn = nd-4
       var2d!1 = var3d!nn
     end if
     if ( nd .ge. 5 ) then
       nn = nd-5
       var2d!0 = var3d!nn
     end if

     if ( dimL .gt. 1 ) then
       if ( nd .eq. 5 ) then
         return( var2d )
       end if
       if ( nd .eq. 4 ) then
         return( var2d(0,:,:,:,:) )
       end if
       if ( nd .eq. 3 ) then
         return( var2d(0,0,:,:,:) )
       end if
     else
       if ( nd .eq. 5 ) then
         return( var2d(:,:,0,:,:) )
       end if
       if ( nd .eq. 4 ) then
         return( var2d(0,:,0,:,:) )
       end if
       if ( nd .eq. 3 ) then
         return( var2d(0,0,0,:,:) )
       end if
       if ( z(0,0,0,0,0) .gt. 500.) then
          var2d@PlotLevelID = loc_param + " hPa"
       else
          var2d@PlotLevelID = .001*loc_param + " km"
       end if
     end if


  end if




  if(plot_type .eq. "v" ) then   ;  vertical cross section needed

     dims = dimsizes(var3d)

     if ( dimsizes(dims) .eq. 4 ) then
       z = z_in(0,:,:,:)
     else
       z = z_in
     end if
       
; set vertical cross section
     if (opts) then
       xy = wrf_user_set_xy( z, loc_param(0)-1, loc_param(1)-1, \    ; the -1 is for NCL dimensions
                                loc_param(2)-1, loc_param(3)-1, \
                                angle, opts )
     else
       xy = wrf_user_set_xy( z, loc_param(0), loc_param(1), \
                                0.0, 0.0, angle, opts )
     end if
     xp = dimsizes(xy)
  
  
; first we interp z
     var2dz   = wrf_interp_2d_xy( z, xy)

;  interp to constant z grid
     z_max = max(var2dz)
     z_min = min(var2dz)
     dz = 0.01*(z_max - z_min)
     nlevels = tointeger(z_max/dz + 1.)
     z_var2d = new( (/nlevels/), typeof(var2dz))
     z_var2d(0) = z_min
  
     if(var2dz(0,0) .gt. var2dz(1,0) ) then  ; monotonically decreasing coordinate
        dz = -dz
        z_var2d(0) = z_max
     end if
  
     do i=1, nlevels-1
        z_var2d(i) = z_var2d(0)+i*dz
     end do
  

; interp the variable
     if ( dimsizes(dims) .eq. 4 ) then
       var2d = new( (/dims(0), nlevels, xp(0)/), typeof(var2dz))
       do it = 0,dims(0)-1
         var2dtmp = wrf_interp_2d_xy( var3d(it,:,:,:), xy)
         do i=0,xp(0)-1
            var2d(it,:,i) = wrf_interp_1d( var2dtmp(:,i), var2dz(:,i), z_var2d)
         end do
       end do
       var2d!0 = var3d!0
       var2d!1 = "Vertical"
       var2d!2 = "Horizontal"
     else
       var2d = new( (/nlevels, xp(0)/), typeof(var2dz))
       var2dtmp = wrf_interp_2d_xy( var3d, xy)
       do i=0,xp(0)-1
          var2d(:,i) = wrf_interp_1d( var2dtmp(:,i), var2dz(:,i), z_var2d)
       end do
       var2d!0 = "Vertical"
       var2d!1 = "Horizontal"
     end if



     st_x = tointeger(xy(0,0)) + 1
     st_y = tointeger(xy(0,1)) + 1
     ed_x = tointeger(xy(xp(0)-1,0)) + 1
     ed_y = tointeger(xy(xp(0)-1,1)) + 1
     if (opts) then
       var2d@Orientation = "Cross-Sesion: (" + \
                            st_x + "," + st_y + ") to (" + \
                            ed_x + "," + ed_y + ")"
     else
       var2d@Orientation = "Cross-Sesion: (" + \
                            st_x + "," + st_y + ") to (" + \
                            ed_x + "," + ed_y + ") ; center=(" + \
                            loc_param(0) + "," + loc_param(1) + \
                            ") ; angle=" + angle
     end if

     return(var2d)
  end if



end

;--------------------------------------------------------------------------------
 
undef("wrf_user_intrp2d")
function wrf_user_intrp2d( var2d:numeric, \
                           loc_param:numeric, angle:numeric, opts:logical )

; var2d      - 2d field to interpolate 
; loc_param  - plane for vertical plots (2 values representing an xy point 
;              on the model domain through which the vertical plane will pass
;              OR 4 values specifying start and end values
; angle      - 0.0 for horizontal plots, and 
;              an angle for vertical plots - 90 represent a WE cross section
; opts         Used IF opts is TRUE, else use loc_param and angle to determine crosssection

begin

     dims = dimsizes(var2d)
     nd = dimsizes(dims)
     dimX = dims(nd-1)
     dimY = dims(nd-2)
     dimT = 1
     if ( nd .eq. 3 ) then
       dimT = dims(nd-3)
     end if
     var2dtmp = new( (/ 1, dimY, dimX /), typeof(var2d) )
     

; set vertical cross section
     if ( nd .eq. 3 ) then
       var2dtmp(0,:,:) = var2d(0,:,:)
     else
       var2dtmp(0,:,:) = var2d(:,:)
     end if
     if (opts) then
       xy = wrf_user_set_xy( var2dtmp, \
                             loc_param(0)-1, loc_param(1)-1, \    ; the -1 is for NCL dimensions
                             loc_param(2)-1, loc_param(3)-1, \
                             angle, opts )
     else
       xy = wrf_user_set_xy( var2dtmp, \
                             loc_param(0), loc_param(1), \
                             0.0, 0.0, angle, opts )
     end if
     xp = dimsizes(xy)

     var2dout = new( (/ dimT, xp(0) /), typeof(var2d) )

     var1dtmp = wrf_interp_2d_xy( var2dtmp, xy )
     var2dout(0,:) = var1dtmp(0,:)
     if ( dimT .eq. 1 ) then
       var2dout!1 = "Horizontal"
       return ( var2dout(0,:) )
     end if

     do it = 1,dimT-1
       var2dtmp(0,:,:) = var2d(it,:,:)
       var1dtmp = wrf_interp_2d_xy( var2dtmp, xy )
       var2dout(it,:) = var1dtmp(0,:)
     end do
     var2dout!0 = "Time"
     var2dout!1 = "Horizontal"
     return(var2dout)

end

 
undef("wrf_user_getvar")
function wrf_user_getvar( nc_file:file, varin[*]:string, time:integer )

begin

  variable = varin(0)

  if( (variable .eq. "uvmet") .or. (variable .eq. "uvmet10") ) then
       ;; Calculate winds rotated to earth coord. 
       ;; Return an array for uvmet 
       ;;   uvmet(t,0,z,y,x) is umet, and 
       ;;   uvmet(t,1,z,y,x) is vmet
       ;;   IF time was not -1 on input, then t falls away, making this a 4D variable
       ;; Return an array for uvmet10 
       ;;   uvmet10(t,0,y,x) is umet10, and 
       ;;   uvmet10(t,1,y,x) is vmet10
       ;;   IF time was not -1 on input, then t falls away, making this a 3D variable

       pii = 3.14159265
       radians_per_degree = pii/180.

       if( (variable .eq. "uvmet") )  then
         getU = "U" 
         getV = "V" 
         if(.not. isfilevar(nc_file,"U")) then
           if(isfilevar(nc_file,"UU")) then
             getU = "UU"
             getV = "VV"
           end if
         end if
         if ( time .eq. -1 ) then
           u_in = nc_file->$getU$
           v_in = nc_file->$getV$
           nameX = u_in!3
           nameY = u_in!2
           nameZ = u_in!1
           nameT = u_in!0
             dd_in = dimsizes(v_in)
             num_dd = dimsizes(dd_in)
             dimX = dd_in(num_dd-1)
             delete(dd_in)
             dd_in = dimsizes(u_in)
             num_dd = dimsizes(dd_in)
             dimY = dd_in(num_dd-2)
             dimZ = dd_in(num_dd-3)
             dimT = dd_in(num_dd-4)
             delete(dd_in)
           u = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
           v = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
           u(:,:,:,:) = 0.5*(u_in(:,:,:,:dimX-1) + u_in(:,:,:,1:dimX))
           v(:,:,:,:) = 0.5*(v_in(:,:,:dimY-1,:) + v_in(:,:,1:dimY,:))
         else
           u_in   = nc_file->$getU$(time,:,:,:)
           v_in   = nc_file->$getV$(time,:,:,:)
           nameX = u_in!2
           nameY = u_in!1
           nameZ = u_in!0
           nameT = "Time"
             dd_in = dimsizes(v_in)
             num_dd = dimsizes(dd_in)
             dimX = dd_in(num_dd-1)
             delete(dd_in)
             dd_in = dimsizes(u_in)
             num_dd = dimsizes(dd_in)
             dimY = dd_in(num_dd-2)
             dimZ = dd_in(num_dd-3)
             dimT = 1
             delete(dd_in)
           u = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
           v = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
           u(0,:,:,:) = 0.5*(u_in(:,:,:dimX-1) + u_in(:,:,1:dimX))
           v(0,:,:,:) = 0.5*(v_in(:,:dimY-1,:) + v_in(:,1:dimY,:))
         end if
       end if  
       if( (variable .eq. "uvmet10") )  then
         if(isfilevar(nc_file,"U10")) then
           if ( time .eq. -1 ) then
             u_in   = nc_file->U10
             v_in   = nc_file->V10
             nameX = u_in!2
             nameY = u_in!1
             nameT = u_in!0
             nameZ = "surface"
               dd_in = dimsizes(u_in)
               num_dd = dimsizes(dd_in)
               dimX = dd_in(num_dd-1)
               dimY = dd_in(num_dd-2)
               dimZ = 1
               dimT = dd_in(num_dd-3)
               delete(dd_in)
             dd_in = dimsizes(u_in)
             u   = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
             v   = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
             u(:,0,:,:) = u_in
             v(:,0,:,:) = v_in
             delete(dd_in)
           else
             u_in  = nc_file->U10(time,:,:)
             v_in  = nc_file->V10(time,:,:)
             nameX = u_in!1
             nameY = u_in!0
             nameT = "Time"
             nameZ = "surface"
               dd_in = dimsizes(u_in)
               num_dd = dimsizes(dd_in)
               dimX = dd_in(num_dd-1)
               dimY = dd_in(num_dd-2)
               dimZ = 1
               dimT = 1
               delete(dd_in)
             u   = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
             v   = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
             u(0,0,:,:) = u_in
             v(0,0,:,:) = v_in
           end if
         else  ; may be a met file, so get lowest level of UU and VV
           if(isfilevar(nc_file,"UU")) then
             print("Assume this is a met_em file - getting lowest level from UU and VV fields")
             if ( time .eq. -1 ) then
               u_in  = nc_file->UU(:,0,:,:)
               v_in  = nc_file->VV(:,0,:,:)
               nameX = u_in!2
               nameY = u_in!1
               nameT = u_in!0
               nameZ = "surface"
                 dd_in = dimsizes(v_in)
                 num_dd = dimsizes(dd_in)
                 dimX = dd_in(num_dd-1)
                 delete(dd_in)
                 dd_in = dimsizes(u_in)
                 num_dd = dimsizes(dd_in)
                 dimY = dd_in(num_dd-2)
                 dimZ = 1
                 dimT = dd_in(num_dd-3)
                 delete(dd_in)
               u = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
               v = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
               u(:,0,:,:) = 0.5*(u_in(:,:,:dimX-1) + u_in(:,:,1:dimX))
               v(:,0,:,:) = 0.5*(v_in(:,:dimY-1,:) + v_in(:,1:dimY,:))
             else
               u_in  = nc_file->UU(time,0,:,:)
               v_in  = nc_file->VV(time,0,:,:)
               nameX = u_in!1
               nameY = u_in!0
               nameT = "Time"
               nameZ = "surface"
                 dd_in = dimsizes(v_in)
                 num_dd = dimsizes(dd_in)
                 dimX = dd_in(num_dd-1)
                 delete(dd_in)
                 dd_in = dimsizes(u_in)
                 num_dd = dimsizes(dd_in)
                 dimY = dd_in(num_dd-2)
                 dimZ = 1
                 dimT = 1
                 delete(dd_in)
               u = new ( (/ dimT, dimZ, dimY, dimX /), typeof(u_in) )
               v = new ( (/ dimT, dimZ, dimY, dimX /), typeof(v_in) )
               u(0,0,:,:) = 0.5*(u_in(:,:dimX-1) + u_in(:,1:dimX))
               v(0,0,:,:) = 0.5*(v_in(:dimY-1,:) + v_in(1:dimY,:))
             end if
           end if
         end if  
       end if  

       uvmet = new( (/ dimT, 2, dimZ, dimY, dimX /), typeof(u))
       uvmet@description = " u,v met velocity"
       uvmet@units = "m/s"

       map_projection = nc_file@MAP_PROJ


       if( any(map_projection.eq.(/0,3,6/)) ) then  ; no rotation needed
         uvmet(:,0,:,:,:) = u(:,:,:,:) 
         uvmet(:,1,:,:,:) = v(:,:,:,:)
       end if


       if( any(map_projection.eq.(/1,2/)) ) then  ; no rotation needed
         cen_lat  = nc_file@CEN_LAT
         if(isatt(nc_file,"STAND_LON")) then
             cen_long = nc_file@STAND_LON
         else
             cen_long = nc_file@CEN_LON
         end if
         true_lat1 = nc_file@TRUELAT1
         true_lat2 = nc_file@TRUELAT2
  
         getLAT = "XLAT" 
         getLON = "XLONG" 
         if(.not. isfilevar(nc_file,"XLAT")) then
           if(isfilevar(nc_file,"XLAT_M")) then
             getLAT = "XLAT_M"
             getLON = "XLONG_M"
           end if
         end if
         if ( time .eq. -1 ) then
           latitude   = nc_file->$getLAT$
           longitude  = nc_file->$getLON$
         else
           lat   = nc_file->$getLAT$(time,:,:)
           lon   = nc_file->$getLON$(time,:,:)
           dd_in = dimsizes(lat)
           latitude  = new ( (/ 1, dd_in(0), dd_in(1) /), typeof(lat) )
           longitude = new ( (/ 1, dd_in(0), dd_in(1) /), typeof(lon) )
           latitude(0,:,:)  = lat
           longitude(0,:,:) = lon
           delete(dd_in)
         end if

         cone = 1.
         if( map_projection .eq. 1) then    ; Lambert Conformal mapping
           if( (fabs(true_lat1 - true_lat2) .gt. 0.1) .and.  \
               (fabs(true_lat2 - 90. )      .gt. 0.1)       ) then
               cone = 10^(cos(true_lat1*radians_per_degree)) \
                     -10^(cos(true_lat2*radians_per_degree))
               cone = cone/(10^(tan(45. -fabs(true_lat1/2.)*radians_per_degree)) - \
                            10^(tan(45. -fabs(true_lat2/2.)*radians_per_degree))   )
           else
               cone = sin(fabs(true_lat1)*radians_per_degree)
           end if
         end if
         if(map_projection .eq. 2) then      ; polar stereographic
           cone = 1.
         end if
         if(map_projection .eq. 3) then      ; Mercator
           cone = 0.
         end if

         uvmet = wrf_uvmet( u, v, latitude, longitude, cen_long, cone )
       end if  
       uvmet!0 = nameT
       uvmet!1 = "u_and_v"
       uvmet!2 = nameZ
       uvmet!3 = nameY
       uvmet!4 = nameX

       if( (variable .eq. "uvmet") ) then
         if ( time .eq. -1 )
           return(uvmet)
         else
           return(uvmet(0,:,:,:,:))
         end if
       end if
       if( (variable .eq. "uvmet10") ) then
         if ( time .eq. -1 )
           return(uvmet(:,:,0,:,:))
         else
           return(uvmet(0,:,0,:,:))
         end if
       end if


  end if



  if( variable .eq. "ua" ) then
       ;  U interpolated to mass points
       getTHIS = "U" 
       if(.not. isfilevar(nc_file,"U")) then
         if(isfilevar(nc_file,"UU")) then
           getTHIS = "UU"
         end if
       end if

       if ( time .eq. -1 ) then
         U   = nc_file->$getTHIS$
       else
         U   = nc_file->$getTHIS$(time,:,:,:)
       end if
       dims = dimsizes(U)
       n_dims = dimsizes(dims)
       dimU = dims(n_dims-1)
       if ( n_dims .eq. 4 ) then
         ua  = 0.5*(U(:,:,:,:dimU-2) + U(:,:,:,1:dimU-1))
         ua!3 = "west_east"
         ua!2 = U!2
         ua!1 = U!1
         ua!0 = U!0
       else
         ua  = 0.5*(U(:,:,:dimU-2) + U(:,:,1:dimU-1))
         ua!2 = "west_east"
         ua!1 = U!1
         ua!0 = U!0
       end if
       ua@description = "u Velocity"
       ua@units       = "m/s"
       return(ua)
  end if


  if( variable .eq. "va" ) then
       ; V interpolated to mass points
       getTHIS = "V" 
       if(.not. isfilevar(nc_file,"V")) then
         if(isfilevar(nc_file,"VV")) then
           getTHIS = "VV"
         end if
       end if

       if ( time .eq. -1 ) then
         V   = nc_file->$getTHIS$
       else
         V   = nc_file->$getTHIS$(time,:,:,:)
        end if
       dims = dimsizes(V)
       n_dims = dimsizes(dims)
       dimV = dims(n_dims-2)
       if ( n_dims .eq. 4 ) then
         va  = 0.5*(V(:,:,:dimV-2,:)+V(:,:,1:dimV-1,:))
         va!3 = V!3
         va!2 = "south_north"
         va!1 = V!1
         va!0 = V!0
       else
         va  = 0.5*(V(:,:dimV-2,:)+V(:,1:dimV-1,:))
         va!2 = V!2
         va!1 = "south_north"
         va!0 = V!0
       end if
       va@description = "v Velocity"
       va@units       = "m/s"
       return(va)
  end if


  if( variable .eq. "wa" ) then
       ; W interpolated to mass points
       if ( time .eq. -1 ) then
         W   = nc_file->W
       else
         W   = nc_file->W(time,:,:,:)
       end if
       dims = dimsizes(W)
       n_dims = dimsizes(dims)
       dimW = dims(n_dims-3)
       if ( n_dims .eq. 4 ) then
         wa  = 0.5*(W(:,0:dimW-2,:,:)+W(:,1:dimW-1,:,:))
         wa!3 = W!3
         wa!2 = W!2
         wa!1 = "bottom_top"
         wa!0 = W!0
       else
         wa  = 0.5*(W(0:dimW-2,:,:)+W(1:dimW-1,:,:))
         wa!2 = W!2
         wa!1 = W!1
         wa!0 = "bottom_top"
       end if
       wa@description = "Vertical Velocity w"
       wa@units       = "m/s"
       return(wa)
  end if


  if( variable .eq. "pressure" ) then
       ; Full model pressure [=base pressure (PB) + pertubation pressure (P)]
       if(isfilevar(nc_file,"P")) then
         if ( time .eq. -1 ) then
           P  = nc_file->P
           PB = nc_file->PB
         else
           P  = nc_file->P(time,:,:,:)
           PB = nc_file->PB(time,:,:,:)
         end if
         pressure  = 0.01 * ( P + PB )
       else
         ;; may be a met_em file - see if we can get PRES
         if(isfilevar(nc_file,"PRES")) then
           if ( time .eq. -1 ) then
             pressure = nc_file->PRES
           else
             pressure = nc_file->PRES(time,:,:,:)
           end if
           pressure = 0.01 * pressure
         end if
       end if
       copy_VarCoords ( P , pressure )
       pressure@description = "Pressure"
       pressure@units       = "hPa"
       return(pressure)
  end if


  if( any( variable .eq. (/"geopt","z"/) ) ) then
       ; Height [=full geopotentail height / 9.81]
       if(isfilevar(nc_file,"PH")) then
         if ( time .eq. -1 ) then
           PH  = nc_file->PH
           PHB = nc_file->PHB
         else
           PH  = nc_file->PH(time,:,:,:)
           PHB = nc_file->PHB(time,:,:,:)
         end if
         var = ( PH + PHB )
         dims = dimsizes(var)
         n_dims = dimsizes(dims)
         dimW = dims(n_dims-3)
         if ( n_dims .eq. 4 ) then
           z  = 0.5*(var(:,0:dimW-2,:,:)+var(:,1:dimW-1,:,:))
           z!3 = PH!3
           z!2 = PH!2
           z!1 = "bottom_top"
           z!0 = PH!0
         else
           z  = 0.5*(var(0:dimW-2,:,:)+var(1:dimW-1,:,:))
           z!2 = PH!2
           z!1 = PH!1
           z!0 = "bottom_top"
         end if
       else
         ;; may be a met_em file - see if we can get GHT
         if(isfilevar(nc_file,"GHT")) then
           if ( time .eq. -1 ) then
             z = nc_file->GHT
           else
             z = nc_file->GHT(time,:,:,:)
           end if
         end if
       end if  

       if ( variable .eq. "z" ) then
         z = z / 9.81
         z@description = "Height"
         z@units       = "m"
       else
         z@description = "Geopotential Height"
         z@units       = "m2 s-2"
       end if
       return(z)
  end if


  if( variable .eq. "th" ) then
       ; Potentail Temperature is model output T + 300K
       if ( time .eq. -1 ) then
         T  = nc_file->T
       else
         T  = nc_file->T(time,:,:,:)
       end if  
       th = T + 300.
       copy_VarCoords ( T , th )
       th@description = "Potential Temperature (theta) "
       th@units       = "K"
       return(th)
  end if


  if( any( variable .eq. (/"tk","tc"/) ) ) then
       ;; function wrf_tk needs theta and pressure (Pa) on input and returns temperature in K on return
       if(isfilevar(nc_file,"T")) then
         if ( time .eq. -1 ) then
           T  = nc_file->T
           P  = nc_file->P
           PB = nc_file->PB
         else
           T  = nc_file->T(time,:,:,:)
           P  = nc_file->P(time,:,:,:)
           PB = nc_file->PB(time,:,:,:)
         end if  
         th = T + 300.
         p  = P + PB
         t = wrf_tk( p , th )
       else
         ;; may be a met_em file - see if we can get TT
         if(isfilevar(nc_file,"TT")) then
           if ( time .eq. -1 ) then
             t  = nc_file->TT
           else
             t  = nc_file->TT(time,:,:,:)
           end if  
         end if  
       end if  
       if( variable .eq. "tc" ) then
         t = t - 273.16
         t@units = "C"     ; Overwrite return units
       end if
       copy_VarCoords ( T , t )
       return(t)
  end if


  if( variable .eq. "td" ) then
       ;; function wrf_td needs qv and pressure (Pa) on input and returns dewpoint temperature on return
       if ( time .eq. -1 ) then
         P      = nc_file->P
         PB     = nc_file->PB
         QVAPOR = nc_file->QVAPOR
       else
         P      = nc_file->P(time,:,:,:)
         PB     = nc_file->PB(time,:,:,:)
         QVAPOR = nc_file->QVAPOR(time,:,:,:)
       end if
       p      = P + PB
       QVAPOR = QVAPOR > 0.0
       td    = wrf_td( p , QVAPOR )
       copy_VarCoords ( P , td )
       return(td)
  end if


  if( variable .eq. "td2" ) then
       ;; function wrf_td needs qv and pressure (Pa) on input and returns dewpoint temperature on return
       if ( time .eq. -1 ) then
         P   = nc_file->P(:,0,:,:)      ; get only the lowest level, to match Q2
         PB  = nc_file->PB(:,0,:,:)     ; get only the lowest level, to match Q2
         Q2  = nc_file->Q2
       else
         P   = nc_file->P(time,0,:,:)      ; get only the lowest level, to match Q2
         PB  = nc_file->PB(time,0,:,:)     ; get only the lowest level, to match Q2
         Q2  = nc_file->Q2(time,:,:)
       end if
       p   = P + PB
       Q2  = Q2 > 0.0
       td2 = wrf_td( p , Q2 )
       td2@description = "2m Dewpoint Temperature"   ; Overwrite return description
       copy_VarCoords ( P , td2 )
       return(td2)
  end if


  if( variable .eq. "slp" ) then
       if(isfilevar(nc_file,"T")) then
         ;; first compute theta - function wrf_tk needs theta and pressure (Pa) on input
         ;; THEN compute sea level pressure, from qv, p (Pa), tk, z
         if ( time .eq. -1 ) then
           T      = nc_file->T
           P      = nc_file->P
           PB     = nc_file->PB
           QVAPOR = nc_file->QVAPOR
           PH     = nc_file->PH
           PHB    = nc_file->PHB
         else
           T      = nc_file->T(time,:,:,:)
           P      = nc_file->P(time,:,:,:)
           PB     = nc_file->PB(time,:,:,:)
           QVAPOR = nc_file->QVAPOR(time,:,:,:)
           PH     = nc_file->PH(time,:,:,:)
           PHB    = nc_file->PHB(time,:,:,:)
         end if
         th = T + 300.
         p  = P + PB
         QVAPOR = QVAPOR > 0.000
         var    = ( PH + PHB ) / 9.81
         dims = dimsizes(var)
         n_dims = dimsizes(dims)
         dimW = dims(n_dims-3)
         if ( n_dims .eq. 4 ) then
           z  = 0.5*(var(:,0:dimW-2,:,:)+var(:,1:dimW-1,:,:))
         else
           z  = 0.5*(var(0:dimW-2,:,:)+var(1:dimW-1,:,:))
         end if
         tk = wrf_tk( p , th )    ; calculate TK
         slp   = wrf_slp( z, tk, p, QVAPOR )  ; calculate slp
         if ( n_dims .eq. 4 ) then
            slp!2 = T!3
            slp!1 = T!2
            slp!0 = T!0
         else
            slp!1 = T!2
            slp!0 = T!1
         end if
       else
         ;; may be a met_em file - see if we can get PMSL
         if(isfilevar(nc_file,"PMSL")) then
           if ( time .eq. -1 ) then
             slp = nc_file->PMSL
           else
             slp = nc_file->PMSL(time,:,:)
           end if
         end if
       end if  

       return(slp)
  end if


  if( variable .eq. "rh" ) then
       if(isfilevar(nc_file,"T")) then
         ;; first compute theta - function wrf_tk needs theta and pressure (Pa) on input
         ;; THEN compute rh, using tk, p (Pa), QVAPOR
         if ( time .eq. -1 ) then
           T      = nc_file->T
           P      = nc_file->P
           PB     = nc_file->PB
           QVAPOR = nc_file->QVAPOR
         else
           T      = nc_file->T(time,:,:,:)
           P      = nc_file->P(time,:,:,:)
           PB     = nc_file->PB(time,:,:,:)
           QVAPOR = nc_file->QVAPOR(time,:,:,:)
         end if  
         th = T + 300.
         p  = P + PB
         QVAPOR = QVAPOR > 0.000
         tk = wrf_tk( p , th )
         rh = wrf_rh( QVAPOR, p, tk )
       else
         ;; may be a met_em file - see if we can get RH
         if(isfilevar(nc_file,"RH")) then
           if ( time .eq. -1 ) then
             rh = nc_file->RH
           else
             rh = nc_file->RH(time,:,:)
           end if
         end if
       end if  
       copy_VarCoords ( T , rh )
       return(rh)
  end if


  if( variable .eq. "rh2" ) then
       if(isfilevar(nc_file,"T2")) then
         ;; THEN compute rh2, using T2, PSFC, Q2
         if ( time .eq. -1 ) then
           T2   = nc_file->T2
           PSFC = nc_file->PSFC
           Q2   = nc_file->Q2
           dims = dimsizes(T2)
           nd = dimsizes(dims)
           t = new ( (/ dims(0), 1, dims(nd-2), dims(nd-1) /) , typeof(T2) )
           p = new ( (/ dims(0), 1, dims(nd-2), dims(nd-1) /) , typeof(PSFC) )
           q = new ( (/ dims(0), 1, dims(nd-2), dims(nd-1) /) , typeof(Q2) )
           t(:,0,:,:) = T2(:,:,:)
           p(:,0,:,:) = PSFC(:,:,:)
           q(:,0,:,:) = Q2(:,:,:)
           rh2 = new ( (/ dims(0), dims(nd-2), dims(nd-1) /) , typeof(T2) )
         else
           T2   = nc_file->T2(time,:,:)
           PSFC = nc_file->PSFC(time,:,:)
           Q2   = nc_file->Q2(time,:,:)
           dims = dimsizes(T2)
           nd = dimsizes(dims)
           t = new ( (/ 1, dims(nd-2), dims(nd-1) /) , typeof(T2) )
           p = new ( (/ 1, dims(nd-2), dims(nd-1) /) , typeof(PSFC) )
           q = new ( (/ 1, dims(nd-2), dims(nd-1) /) , typeof(Q2) )
           t(0,:,:) = T2(:,:)
           p(0,:,:) = PSFC(:,:)
           q(0,:,:) = Q2(:,:)
           rh2 = new ( (/ dims(nd-2), dims(nd-1) /) , typeof(T2) )
         end if  
         q = q > 0.000
         rh = wrf_rh( q, p, t )
         if ( time .eq. -1 ) then
           rh2(:,:,:) = rh(:,0,:,:)
         else
           rh2(:,:) = rh(0,:,:)
         end if
         copy_VarCoords ( T2 , rh2 )
       else
         ;; may be a met_em file - see if we can get RH
         if(isfilevar(nc_file,"RH")) then
           print("Probably a met_em file - get lowerst level from RH field")
           if ( time .eq. -1 ) then
             rh2 = nc_file->RH(:,0,:,:)
           else
             rh2 = nc_file->RH(time,0,:,:)
           end if
         end if
       end if  
       return(rh2)
  end if


  if( variable .eq. "dbz" .or. variable .eq. "mdbz" ) then
       ; calculate dbz
       if ( time .eq. -1 ) then
         T  = nc_file->T
         P  = nc_file->P
         PB = nc_file->PB
         qv = nc_file->QVAPOR
         qr = nc_file->QRAIN
         if(isfilevar(nc_file,"QSNOW"))
           qs = nc_file->QSNOW
         else
           qs = qv
           qs = 0.0
         end if
         if(isfilevar(nc_file,"QGRAUP"))
           qg = nc_file->QGRAUP
         else
           qg = qv
           qg = 0.0
         end if
       else
         T  = nc_file->T(time,:,:,:)
         P  = nc_file->P(time,:,:,:)
         PB = nc_file->PB(time,:,:,:)
         qv = nc_file->QVAPOR(time,:,:,:)
         qr = nc_file->QRAIN(time,:,:,:)
         if(isfilevar(nc_file,"QSNOW"))
           qs = nc_file->QSNOW(time,:,:,:)
         else
           qs = qv
           qs = 0.0
         end if
         if(isfilevar(nc_file,"QGRAUP"))
           qg = nc_file->QGRAUP(time,:,:,:)
         else
           qg = qv
           qg = 0.0
         end if
       end if
       th = T + 300.
       p  = P + PB
       tk = wrf_tk( p , th )

       ivarint  = 0 
       iliqskin = 0
       dim_vars = dimsizes(varin)
       do idims = 1,dim_vars-1
         if ( idims .eq. 1 ) then
           if ( varin(idims) .eq. "1" ) then
             ivarint = 1
           end if
         end if
         if ( idims .eq. 2 ) then
           if ( varin(idims) .eq. "1" ) then
             iliqskin = 1
           end if
         end if
       end do

       dim = dimsizes(tk)
       dbz = wrf_dbz ( p, tk, qv, qr, qs, qg, ivarint, iliqskin)

       if ( variable .eq. "mdbz") then
         mdbz = new ( (/ dim(1), dim(2) /), typeof(tk))
         mdbz = 0
         do jj = 0,dim(1)-1
         do ii = 0,dim(2)-1
            mdbz(jj,ii) = max(dbz(:,jj,ii))
         end do
         end do
         mdbz!1 = T!2
         mdbz!0 = T!1
         mdbz@description = "Max Reflectivity"
         mdbz@units       = "dBZ"
         return(mdbz)
       else
         copy_VarCoords ( T , dbz )
         dbz@description = "Reflectivity"
         dbz@units       = "dBZ"
         return(dbz)
       end if

  end if



  if( any( variable .eq. (/"cape_3d"/) ) ) then
       ;; function wrf_tk needs theta and pressure (Pa) on input and returns temperature in K on return
       if ( time .eq. -1 ) then
         T  = nc_file->T
         P  = nc_file->P
         PB = nc_file->PB
         QV = nc_file->QVAPOR
         PH  = nc_file->PH
         PHB = nc_file->PHB
         HGT = nc_file->HGT
         PSFC = nc_file->PSFC
       else
         T  = nc_file->T(time,:,:,:)
         P  = nc_file->P(time,:,:,:)
         PB = nc_file->PB(time,:,:,:)
         QV = nc_file->QVAPOR(time,:,:,:)
         PH  = nc_file->PH(time,:,:,:)
         PHB = nc_file->PHB(time,:,:,:)
         HGT = nc_file->HGT(time,:,:)
         PSFC = nc_file->PSFC(time,:,:)
       end if
       th = T + 300.
       p  = (P + PB) 
       tk = wrf_tk( p , th )

       var = ( PH + PHB )
       dims = dimsizes(var)
       n_dims = dimsizes(dims)
       dimW = dims(n_dims-3)
       p = p/100.
       if ( n_dims .eq. 4 ) then
         z  = 0.5*(var(:,0:dimW-2,:,:)+var(:,1:dimW-1,:,:))
         z = z/9.81
         p  = p(:,::-1,:,:)
         tk = tk(:,::-1,:,:)
         QV = QV(:,::-1,:,:)
         z  = z(:,::-1,:,:)
       else
         z  = 0.5*(var(0:dimW-2,:,:)+var(1:dimW-1,:,:))
         z = z/9.81
         p  = p(::-1,:,:)
         tk = tk(::-1,:,:)
         QV = QV(::-1,:,:)
         z  = z(::-1,:,:)
       end if

       cape3d = rip_cape_3d( p, tk, QV, z, HGT, PSFC , True )
       if ( n_dims .eq. 4 ) then
         cape3d!3 = T!3
         cape3d!2 = T!2
         cape3d!1 = T!0
         cape3d!0 = "cape_cin"
       else
         cape3d!2 = T!2
         cape3d!1 = T!1
         cape3d!0 = "cape_cin"
       end if

       return(cape3d)
  end if



  if( any( variable .eq. (/"cape_2d"/) ) ) then
       ;; function wrf_tk needs theta and pressure (Pa) on input and returns temperature in K on return
       if ( time .eq. -1 ) then
         T  = nc_file->T
         P  = nc_file->P
         PB = nc_file->PB
         QV = nc_file->QVAPOR
         PH  = nc_file->PH
         PHB = nc_file->PHB
         HGT = nc_file->HGT
         PSFC = nc_file->PSFC
       else
         T  = nc_file->T(time,:,:,:)
         P  = nc_file->P(time,:,:,:)
         PB = nc_file->PB(time,:,:,:)
         QV = nc_file->QVAPOR(time,:,:,:)
         PH  = nc_file->PH(time,:,:,:)
         PHB = nc_file->PHB(time,:,:,:)
         HGT = nc_file->HGT(time,:,:)
         PSFC = nc_file->PSFC(time,:,:)
       end if
       th = T + 300.
       p  = (P + PB) 
       tk = wrf_tk( p , th )

       var = ( PH + PHB )
       dims = dimsizes(var)
       n_dims = dimsizes(dims)
       dimW = dims(n_dims-3)
       p = p/100.
       if ( n_dims .eq. 4 ) then
         z  = 0.5*(var(:,0:dimW-2,:,:)+var(:,1:dimW-1,:,:))
         z = z/9.81
         p  = p(:,::-1,:,:)
         tk = tk(:,::-1,:,:)
         QV = QV(:,::-1,:,:)
         z  = z(:,::-1,:,:)
       else
         z  = 0.5*(var(0:dimW-2,:,:)+var(1:dimW-1,:,:))
         z = z/9.81
         p  = p(::-1,:,:)
         tk = tk(::-1,:,:)
         QV = QV(::-1,:,:)
         z  = z(::-1,:,:)
       end if

       cape2d = rip_cape_2d( p, tk, QV, z, HGT, PSFC , True )
       if ( n_dims .eq. 4 ) then
         cape2d!3 = T!3
         cape2d!2 = T!2
         cape2d!1 = T!0
         cape2d!0 = "mcape_mcin_lcl_lfc"
       else
         cape2d!2 = T!2
         cape2d!1 = T!1
         cape2d!0 = "mcape_mcin_lcl_lfc"
       end if

       return(cape2d)
  end if




  if( any( variable .eq. (/"ter","HGT","HGT_M"/) ) ) then
    variable = "HGT" 
    if(.not. isfilevar(nc_file,"HGT")) then
      variable = "HGT_M"
    end if
  end if


  if( any( variable .eq. (/"lat","XLAT","XLAT_M"/) ) ) then
    variable = "XLAT" 
    if(.not. isfilevar(nc_file,"XLAT")) then
      variable = "XLAT_M"
    end if
  end if


  if( any( variable .eq. (/"lon","XLONG","XLONG_M"/) ) ) then
    variable = "XLONG" 
    if(.not. isfilevar(nc_file,"XLONG")) then
      variable = "XLONG_M"
    end if
  end if



;  end of diagnostic variable list - we must want a variable already in the file.  

  if ( time .eq. -1 ) then
    var = nc_file->$variable$
  else
    ;  check variable dimensionality and pull proper time  out of file
    ndims = dimsizes(filevardimsizes(nc_file,variable))
    if( ndims .eq. 4) then
      var = nc_file->$variable$(time,:,:,:)
    end if
    if( ndims .eq. 3) then
      var = nc_file->$variable$(time,:,:)
    end if
    if( ndims .eq. 2) then
      var = nc_file->$variable$(time,:)
    end if
    if( ndims .eq. 1) then
      var = nc_file->$variable$(time)
    end if
  end if

  return(var)

end

;--------------------------------------------------------------------------------

undef("wrf_user_list_times")
function wrf_user_list_times( nc_file:file )

local times, times_in_file, dims, i
begin

  times_in_file = nc_file->Times
  dims = dimsizes(times_in_file)
  times = new(dims(0),string)
  do i=0,dims(0)-1
    times(i) = chartostring(times_in_file(i,:))
  end do
  times@description = "times in file"
  print(times)
  return(times)

end

;--------------------------------------------------------------------------------

undef("wrf_user_latlon_to_ij")
function wrf_user_latlon_to_ij( nc_file:file, latitude:numeric, \
                                longitude:numeric )

begin

  WE       = "WEST-EAST_GRID_DIMENSION"
  SN       = "SOUTH-NORTH_GRID_DIMENSION"
  wedim    = nc_file@$WE$
  sndim    = nc_file@$SN$

  if(isfilevar(nc_file,"XLAT"))
    XLAT  = nc_file->XLAT(0,:,:)
    XLONG = nc_file->XLONG(0,:,:)
  else
    XLAT  = nc_file->XLAT_M(0,:,:)
    XLONG = nc_file->XLONG_M(0,:,:)
  end if

  loc = wrf_latlon_to_ij( XLAT, XLONG, latitude, longitude )
  if ( loc(0) .le. 0 .or. loc(0) .ge. sndim ) then
       loc = -999
  else
       if ( loc(1) .le. 0 .or. loc(1) .ge. wedim ) then
            loc = -999
       end if
  end if

  loc!0 = "j & i locations"
  return(loc)

end

;--------------------------------------------------------------------------------

undef("wrf_user_ll_to_ij")
function wrf_user_ll_to_ij( nc_file:file, latitude:numeric, longitude:numeric, \
                            opts:logical )

begin

  useT  = get_res_value(opts,"useTime",0)
  returnI= get_res_value(opts,"returnInt",True)

  WE       = "WEST-EAST_GRID_DIMENSION"
  SN       = "SOUTH-NORTH_GRID_DIMENSION"
  wedim    = nc_file@$WE$
  sndim    = nc_file@$SN$

  llres = True
  llres@MAP_PROJ = nc_file@MAP_PROJ
  llres@TRUELAT1 = nc_file@TRUELAT1
  llres@TRUELAT2 = nc_file@TRUELAT2
  llres@STAND_LON   = nc_file@STAND_LON
  llres@DX       = nc_file@DX
  hemi     = 1.0
  if ( llres@TRUELAT1 .lt. 0.0 ) then
     hemi  = -1.0
  end if


  if(isfilevar(nc_file,"XLAT"))
    XLAT  = nc_file->XLAT(useT,:,:)
    XLONG = nc_file->XLONG(useT,:,:)
  else
    XLAT  = nc_file->XLAT_M(useT,:,:)
    XLONG = nc_file->XLONG_M(useT,:,:)
  end if


  llres@LAT1 = XLAT(0,0)
  llres@LON1 = XLONG(0,0)
  llres@KNOWNI   = 1.0
  llres@KNOWNJ   = 1.0

  dummy_stdlon = llres@STAND_LON
  dummy_lon1 = llres@LON1
  if (dummy_stdlon .lt. -180.) then
    dummy_stdlon = dummy_stdlon + 360.
  end if
  if (dummy_stdlon .gt. 180.) then
    dummy_stdlon = dummy_stdlon - 360.
  end if
  if (dummy_lon1 .lt. -180.) then
    dummy_lon1 = dummy_lon1 + 360.
  end if
  if (dummy_lon1 .gt.  180.) then
    dummy_lon1 = dummy_lon1 - 360.
  end if
  llres@STAND_LON = dummy_stdlon
  llres@LON1 = dummy_lon1

  loc = wrf_ll_to_ij (latitude,longitude,llres)
  loc@_FillValue = -999

  if ( loc(0) .lt. 0.5 .or. loc(0) .ge. sndim ) then
       loc = -999
  else
       loc(0) = loc(0) - 1.
       if ( loc(1) .lt. 0.5 .or. loc(1) .ge. wedim ) then
            loc = -999
       else
            loc(1) = loc(1) - 1.
       end if
  end if

  if ( returnI  ) then
    loci = new(2,integer)
    loci@_FillValue = -999
    loci(0) = tointeger(loc(0)+.5)
    loci(1) = tointeger(loc(1)+.5)
    loci!0 = "j & i locations"
    return(loci)
  else
    loc!0 = "j & i locations"
    return(loc)
  end if


end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

undef("delete_attrs")
procedure delete_attrs(opts:logical)

; This procedure does some cleanup by removing unneeded attributes
; so they don't get passed to other routines by accident.

begin
  list_attrs = (/"MainTitle","MainTitlePos","MainTitlePosF", \
                 "InitTime","ValidTime","TimePos","TimePosF", \
                 "NoHeaderFooter","TimeLabel","LevelLabel", \
                 "FieldTitle","UnitLabel","NumVectors","AspectRatio", \
                 "SubFieldTitle","PlotOrientation","PlotLevelID", \
                 "mpNestTime", \
                 "ContourParameters","FontHeightF","Footer"/)

  do i=0,dimsizes(list_attrs)-1
    if(isatt(opts,list_attrs(i))) then
      delete(opts@$list_attrs(i)$)
    end if
  end do
end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_cn_resources")
function set_cn_resources (data[*][*]:numeric, res:logical)

begin

  opts = res

; The ContourParameters resource can either be a scalar that
; represents the contour level spacing, or it can be an array
; of three elements that represent the minimum level, the maximum
; level, and the level spacing.
;
  mx = max(data)
  mn = min(data)

  if(mn.ne.mx.and.opts.and.isatt(opts,"ContourParameters")) then
    if(dimsizes(opts@ContourParameters) .eq. 1) then

; Only the contour interval is specified.
      nlev   = tointeger((mx-mn)/opts@ContourParameters)+1
      levels = nice_mnmxintvl(mn,mx,nlev,True)
      if(levels(0) .lt. 0.) then
        ; Set a zero contour.
        nlev = tointeger(levels(0)/opts@ContourParameters) - 1
        levels(0) = nlev*opts@ContourParameters
      end if
      nlev = tointeger((levels(1)-levels(0))/opts@ContourParameters)+1
      levels(1) = levels(0) + nlev*opts@ContourParameters
      levels(2) = opts@ContourParameters

; Min level, max level, and level spacing are specified by user.
    else
      if(dimsizes(opts@ContourParameters) .eq. 3) then
        levels = opts@ContourParameters
      else
        print("wrf_contour: Warning: illegal setting for ContourParameters attribute")
      end if
    end if

  end if

; Contour levels
  if(isvar("levels")) then
    opts@cnLevelSelectionMode = get_res_value_keep(opts, "cnLevelSelectionMode", "ManualLevels")
    opts@cnMinLevelValF       = get_res_value_keep(opts, "cnMinLevelValF", levels(0))
    opts@cnMaxLevelValF       = get_res_value_keep(opts, "cnMaxLevelValF", levels(1))
    opts@cnLevelSpacingF      = get_res_value_keep(opts, "cnLevelSpacingF",levels(2))
    delete(levels)
  end if


; Set the default zero line thickness to 2, and the negative contour
; line dash pattern to 1 (0 is solid).
  opts@gsnContourZeroLineThicknessF = get_res_value_keep(opts, "gsnContourZeroLineThicknessF",2.0)
  opts@gsnContourNegLineDashPattern = get_res_value_keep(opts, "gsnContourNegLineDashPattern",1)


; Set resources that apply for both filled and line contour plots.
  opts@cnFillDrawOrder            = get_res_value_keep(opts,"cnFillDrawOrder", "PreDraw")


  opts@cnLineLabelAngleF          = get_res_value_keep(opts,"cnLineLabelAngleF", 0.0)
  opts@cnLineLabelFontHeightF     = get_res_value_keep(opts,"cnLineLabelFontHeightF", 0.015)
  opts@cnInfoLabelFontHeightF     = get_res_value_keep(opts,"cnInfoLabelFontHeightF", 0.015)
  opts@cnLineLabelPerimOn         = get_res_value_keep(opts,"cnLineLabelPerimOn", True)
  opts@cnInfoLabelPerimOn         = get_res_value_keep(opts,"cnInfoLabelPerimOn", False)
  opts@cnLineLabelBackgroundColor = get_res_value_keep(opts,"cnLineLabelBackgroundColor", -1)
  opts@cnHighLabelBackgroundColor = get_res_value_keep(opts,"cnHighLabelBackgroundColor", -1)
  opts@cnLowLabelBackgroundColor  = get_res_value_keep(opts,"cnLowLabelBackgroundColor", -1)
  opts@cnLineColor                = get_res_value_keep(opts,"cnLineColor", "Black")
  opts@cnLineLabelFontColor       = opts@cnLineColor
  opts@cnLineLabelPerimColor      = opts@cnLineColor
  opts@cnInfoLabelFontColor       = opts@cnLineColor
  opts@cnHighLabelFontColor       = opts@cnLineColor
  opts@cnLowLabelFontColor        = opts@cnLineColor


; Set field Title and levels if available
  if(isatt(opts,"FieldTitle")) then
    opts@cnInfoLabelString     = opts@FieldTitle + " Contours: $CMN$ to $CMX$ by $CIU$"
  else
    if(isatt(data,"description")) then
      opts@cnInfoLabelString     = data@description + " Contours: $CMN$ to $CMX$ by $CIU$"
    else
      opts@cnInfoLabelString     = " Contours: $CMN$ to $CMX$ by $CIU$"
    end if
  end if


  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_lb_resources")
function set_lb_resources (data[*][*]:numeric, res:logical)

begin

  opts = res


; Somewhat convoluted way to see if a labelbar is not desired.
  if(check_attr(opts,"pmTickMarkDisplayMode","Never",True).or.\
     check_attr(opts,"pmTickMarkDisplayMode",-1,False).or.\
     check_attr(opts,"pmTickMarkDisplayMode",0,False).or. \
     check_attr(opts,"lbLabelBarOn",False,False).or.\
     check_attr(opts,"lbLabelBarOn",0,False)) then
    lbar_on = False
  else
    lbar_on = True
  end if
  atmp = get_res_value(opts,"lbLabelBarOn",True) ; Remove this resource
  delete(atmp)                                   ; just in case.


; Possible title for the labelbar
  if(isatt(opts,"FieldTitle")) then
    lb_desc = opts@FieldTitle 
  else
    if(isatt(data,"description")) then
      lb_desc = data@description 
    else
      lb_desc = ""  
    end if
  end if

  if(isatt(opts,"UnitLabel") ) then
    lb_desc = lb_desc + "  (" + opts@UnitLabel + ")"
  else
    if(isatt(data,"units") .and. .not.(data@units.eq."")) then
      lb_desc = lb_desc + "  (" + data@units + ")"
    end if
  end if


  if(.not.isatt(opts,"cnFillColors")) then
    opts@gsnSpreadColors = get_res_value_keep(opts, "gsnSpreadColors", True)
  end if
  opts@cnInfoLabelOn   = get_res_value_keep(opts,"cnInfoLabelOn", False)
  opts@cnLinesOn       = get_res_value_keep(opts,"cnLinesOn", False)
  opts@cnLineLabelsOn  = get_res_value_keep(opts,"cnLineLabelsOn", False)

; Labelbar resources
  if(lbar_on) then
    opts@pmLabelBarDisplayMode    = get_res_value_keep(opts,"pmLabelBarDisplayMode", "Always")
    opts@pmLabelBarSide           = get_res_value_keep(opts,"pmLabelBarSide", "Bottom")
    opts@lbAutoManage             = get_res_value_keep(opts,"lbAutoManage",False)
    opts@lbOrientation            = get_res_value_keep(opts,"lbOrientation", "Horizontal")
    opts@lbPerimOn                = get_res_value_keep(opts,"lbPerimOn", False)
    opts@lbLabelJust              = get_res_value_keep(opts,"lbLabelJust", "BottomCenter")
    opts@lbLabelAutoStride        = get_res_value_keep(opts,"lbLabelAutoStride",True)
    opts@lbBoxMinorExtentF        = get_res_value_keep(opts,"lbBoxMinorExtentF", 0.13)
    opts@lbTitleFontHeightF       = get_res_value_keep(opts,"lbTitleFontHeightF", 0.015)
    opts@lbLabelFontHeightF       = get_res_value_keep(opts,"lbLabelFontHeightF", 0.015)
    opts@pmLabelBarOrthogonalPosF = get_res_value_keep(opts,"pmLabelBarOrthogonalPosF", -0.1)

    opts@lbTitleOn                = get_res_value_keep(opts,"lbTitleOn", True)
    if(lb_desc.ne."" .and. opts@lbTitleOn) then
      opts@lbTitleOn              = get_res_value_keep(opts,"lbTitleOn", True)
      opts@lbTitleString          = get_res_value_keep(opts,"lbTitleString", lb_desc)
      opts@lbTitleJust            = get_res_value_keep(opts,"lbTitleJust", "BottomCenter")
      opts@lbTitleOffsetF         = get_res_value_keep(opts,"lbTitleOffsetF", -0.5)
    else
      opts@lbTitleOn = False
    end if
  end if


  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_title_resources")
function set_title_resources (data[*][*]:numeric, res:logical)

begin

  opts = res

; Set field Title and levels if available
  if(isatt(opts,"FieldTitle")) then
    SubTitles = opts@FieldTitle
  else
    if(isatt(data,"description")) then
      SubTitles = data@description
    else
      SubTitles = "UnKnown"
    end if
  end if

  if(isatt(opts,"SubFieldTitle")) then
    SubTitles = SubTitles + " " +  opts@SubFieldTitle
  end if
  if(isatt(opts,"UnitLabel")) then
    SubTitles = SubTitles + "   (" +  opts@UnitLabel + ")"
  else
     if(isatt(data,"units") .and. .not.(data@units.eq."")) then
      SubTitles = SubTitles + "   (" +  data@units + ")"
    end if
  end if
  if (isatt(opts,"PlotLevelID")) then
    SubTitles = SubTitles + "     at   " +  opts@PlotLevelID 
  else
    if (isatt(data,"PlotLevelID")) then
       SubTitles = SubTitles + "     at   " +  data@PlotLevelID 
    end if
  end if
  opts@tiMainString = SubTitles      

  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
undef("set_vc_resources")
function set_vc_resources (res:logical)

begin

  opts = res

  if ( isatt(opts,"vpWidthF") ) then
  ; num_vectors is used for vcMinDistanceF and vcRefLengthF
    width = opts@vpWidthF
    num_vectors = get_res_value(opts,"NumVectors",25.0)
    opts@vcMinDistanceF   = get_res_value_keep(opts,"vcMinDistanceF", width/num_vectors)
    opts@vcRefLengthF     = get_res_value_keep(opts,"vcRefLengthF", width/num_vectors)
  else
    opts@vcMinDistanceF   = get_res_value_keep(opts,"vcMinDistanceF", 0.02)
    opts@vcRefLengthF     = get_res_value_keep(opts,"vcRefLengthF", 0.02)
  end if


  opts@vcGlyphStyle     = get_res_value_keep(opts,"vcGlyphStyle", "WindBarb")
  opts@vcWindBarbColor  = get_res_value_keep(opts,"vcWindBarbColor", "Black")
  opts@vcRefAnnoOn      = get_res_value_keep(opts,"vcRefAnnoOn", False)
  opts@vcMinFracLengthF = get_res_value_keep(opts,"vcMinFracLengthF", .2)
 
  return(opts)
end
;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------

procedure _SetMainTitle(nc_file:file,wks[1]:graphic,cn[1]:graphic,opts)
 
; This procedure checks the input data for certain attributes, and
; based on those, sets MainTitle, InitTime and ValidTime
;
;  Attributes recognized by this procedure:
;     MainTitle                 (main title - top left)
;                               (with Init time top right)
;     TimeLabel                 (valid time  - right  under init time)
;     NoHeaderFooter            (switch all headers and footers off - mainly for panels)
;
; If the "NoHeaderFooter" attribute exists and is set True, then
; don't create any titles.

begin
;
  if(opts.and.isatt(opts,"NoHeaderFooter").and.opts@NoHeaderFooter) then
    return
  end if

;
; Set basic plot font
;
  font_height = get_res_value_keep(opts,"FontHeightF",0.01)
;
;
; If a MainTitle attribute hasn't been set, then set to "WRF"
; Also set an Initial time
;
; MAIN Header of plot
  opts@MainTitle    = get_res_value_keep(opts,"MainTitle", " ") 
  opts@MainTitlePos = get_res_value_keep(opts,"MainTitlePos", "Left") 
  opts@InitTime     = get_res_value_keep(opts,"InitTime", True) 
  opts@ValidTime    = get_res_value_keep(opts,"ValidTime", True) 
  opts@TimePos      = get_res_value_keep(opts,"TimePos", "Right") 
  opts@Footer       = get_res_value_keep(opts,"Footer", True) 


  if (opts@MainTitlePos .eq. "Left")
    opts@MainTitlePos  = "CenterLeft"
    opts@MainTitlePosF = 0.0
  end if
  if (opts@MainTitlePos .eq. "Center")
    opts@MainTitlePos  = "CenterCenter"
    opts@MainTitlePosF = 0.5
  end if
  if (opts@MainTitlePos .eq. "Right")
    opts@MainTitlePos  = "CenterRight"
    opts@MainTitlePosF = 1.0
  end if

  if (opts@TimePos .eq. "Left")
    MTOPosF = 0.30
  else
    MTOPosF = 0.20
  end if

  txt0 = create "MainPlotTilte" textItemClass wks
    "txString"      : opts@MainTitle
    "txFontHeightF" : font_height*1.5
  end create
  anno = NhlAddAnnotation(cn,txt0)
  setvalues anno
    "amZone"           : 3
    "amSide"           : "Top"
    "amJust"           : opts@MainTitlePos
    "amParallelPosF"   : opts@MainTitlePosF
    "amOrthogonalPosF" : MTOPosF
    "amResizeNotify"   : False
  end setvalues

; Time information on plot
  if (opts@TimePos .eq. "Left")
    opts@TimePos  = "CenterLeft"
    opts@TimePosF = 0.0
    if (opts@MainTitlePos .eq. "CenterLeft")
     MTOPosF = MTOPosF - 0.05
    end if
  end if
  if (opts@TimePos .eq. "Right")
    opts@TimePos  = "CenterRight"
    opts@TimePosF = 1.0
    if (opts@MainTitlePos .eq. "CenterRight")
     MTOPosF = MTOPosF - 0.05
    end if
  end if

  if( isatt(nc_file,"START_DATE") .and. opts@InitTime ) then
    InitTime = "Init: " + nc_file@START_DATE
    txt1 = create "InitTime" textItemClass wks
      "txString"      : InitTime
      "txFontHeightF" : font_height
    end create
    anno = NhlAddAnnotation(cn,txt1)
    setvalues anno
      "amZone"           : 3
      "amSide"           : "Top"
      "amJust"           : opts@TimePos
      "amParallelPosF"   : opts@TimePosF        
      "amOrthogonalPosF" : MTOPosF
      "amResizeNotify"   : False
    end setvalues
  end if

  plot_narrow = False
  if((opts).and.(isatt(opts,"vpWidthF")).and.(isatt(opts,"vpHeightF"))) then
     ph = opts@vpHeightF  
     pw = opts@vpWidthF  
     phw = ph/pw
     if ( phw .gt. 1.8 ) then
       plot_narrow = True
     end if
  end if

  if( opts@ValidTime .and. isatt(opts,"TimeLabel") ) then

    ValidTime = "Valid: " + opts@TimeLabel

    MTOPosF = MTOPosF - 0.03
    txt2 = create "ValidTime" textItemClass wks
      "txString"      : ValidTime
      "txFontHeightF" : font_height
    end create
    anno = NhlAddAnnotation(cn,txt2)
    setvalues anno
      "amZone"           : 3
      "amSide"           : "Top"
      "amJust"           : opts@TimePos
      "amParallelPosF"   : opts@TimePosF        
      "amOrthogonalPosF" : MTOPosF           
      "amResizeNotify"   : False
    end setvalues
  end if


; Add Footer if called for
  if( opts@Footer ) then
    footer1 = nc_file@TITLE
    dis =  nc_file@DX / 1000.0
    if(isatt(nc_file,"WEST-EAST_PATCH_END_UNSTAG"))
      WE = "WEST-EAST_GRID_DIMENSION"
      SN = "SOUTH-NORTH_GRID_DIMENSION"
      BT = "BOTTOM-TOP_GRID_DIMENSION"
      footer2 =  " Phys Opt = " + nc_file@MP_PHYSICS + \
                 " ; PBL Opt = " + nc_file@BL_PBL_PHYSICS + \
                 " ; Cu Opt = " + nc_file@CU_PHYSICS + \
                 " ; WE = " + nc_file@$WE$ + \
                 " ; SN = " + nc_file@$SN$ + \
                 " ; Levels = " + nc_file@$BT$ + \
                 " ; Dis = " + dis + "km"
    else
      WE = "WEST-EAST_GRID_DIMENSION"
      SN = "SOUTH-NORTH_GRID_DIMENSION"
      BT = "BOTTOM-TOP_GRID_DIMENSION"
      footer2 =  " WE = " + nc_file@$WE$ + \
                 " ; SN = " + nc_file@$SN$ + \
                 " ; Levels = " + nc_file@$BT$ + \
                 " ; Dis = " + dis + "km"
    end if
    Footer = footer1 + "~C~" + footer2
   else
     Footer = " "
   end if
    txt3 = create "Footer" textItemClass wks
      "txString"      : Footer 
      "txFontHeightF" : font_height*.9
    end create
    anno = NhlAddAnnotation(cn,txt3)
    setvalues anno
      "amZone"           : 1
;      "amZone"           : 7
      "amJust"           : "TopLeft"
      "amSide"           : "Bottom"
      "amParallelPosF"   : 0.0
      "amOrthogonalPosF" : -0.55
      "amResizeNotify"   : False
    end setvalues


; Add X-setion information if needed
  if(opts.and.isatt(opts,"PlotOrientation")) then
    ;Xsection = "Cross-Section Orientation : " + opts@PlotOrientation
    Xsection =  opts@PlotOrientation
    txt4 = create "Xsection" textItemClass wks
      "txString"      : Xsection 
      "txFontHeightF" : font_height*.9
    end create
    anno = NhlAddAnnotation(cn,txt4)
    setvalues anno
      "amZone"           : 3
      "amSide"           : "Top"
      "amJust"           : "CenterRight"
      "amParallelPosF"   : 1.0
      "amOrthogonalPosF" : 0.005
      "amResizeNotify"   : False
    end setvalues
  end if

end

;--------------------------------------------------------------------------------
;--------------------------------------------------------------------------------
function wrf_contour_ps(nc_file:file,wks[1]: graphic, data[*][*]:numeric, \
                     opt_args[1]:logical)

begin

  callFrame = True
  if ( isatt(opt_args,"FrameIT") ) then
    if ( .not.opt_args@FrameIT ) then
      callFrame = False
    end if
    delete (opt_args@FrameIT)
  end if

  lat2U = nc_file->XLAT_U(0,:,:)
  lon2U = nc_file->XLONG_U(0,:,:)

  opts = opt_args
  opts@sfXArray = lon2U
  opts@sfYArray = lat2U
  opts@sfDataArray = data
  opts@mpProjection         = "Stereographic"
  opts@mpEllipticalBoundary = True
  opts@mpFillOn = False
 
  opts@mpGeophysicalLineColor      = get_res_value_keep(opts, "mpGeophysicalLineColor","Gray")
  opts@mpGeophysicalLineThicknessF = get_res_value_keep(opts, "mpGeophysicalLineThicknessF",2.0)


  ; Set the contour resources
  opts = set_cn_resources(data,opts)
  opts@cnInfoLabelFontHeightF = 0.012
  opts@cnLineLabelPerimOn = False
  opts@cnInfoLabelPerimOn = True


  ; Find out if we are working with a contour or a shaded plot
  ; fill_on = False : line contour plot
  ; fill_on = True  : filled contour plot
  fill_on = get_res_value_keep(opts,"cnFillOn",False)
  if(fill_on) then      ; set lb resources if needed
    opts@pmLabelBarDisplayMode    = get_res_value_keep(opts,"pmLabelBarDisplayMode", "Never")
    opts = set_lb_resources(data,opts)
    opts@pmLabelBarOrthogonalPosF = 0.0
    opts@lbTitleJust = "BottomLeft"
  end if
  


  opts@gsnDraw = False   
  opts@gsnFrame = False
  opts@gsnMaximize = False
  delete_attrs(opts)
  cn = gsn_csm_contour_map_polar(wks,data,opts)          ; Create the plot.

  draw(cn)


  if ( callFrame ) then
    frame(wks)
  end if

return (cn)
end
;--------------------------------------------------------------------------------
function wrf_vector_ps(nc_file:file,wks[1]: graphic, \
                       data_u[*][*]:numeric, data_v[*][*]:numeric, \
                       opt_args[1]:logical)

begin

  callFrame = True
  if ( isatt(opt_args,"FrameIT") ) then
    if ( .not.opt_args@FrameIT ) then
      callFrame = False
    end if
    delete (opt_args@FrameIT)
  end if

  if(isfilevar(nc_file,"XLAT"))
    lat2T = nc_file->XLAT(0,:,:)
    lon2T = nc_file->XLONG(0,:,:)
  else
    lat2T = nc_file->XLAT_M(0,:,:)
    lon2T = nc_file->XLONG_M(0,:,:)
  end if

  opts = opt_args
  opts@vfXArray = lon2T
  opts@vfYArray = lat2T
  opts@vfUDataArray = data_u
  opts@vfVDataArray = data_v
  opts@mpProjection         = "Stereographic"
  opts@mpEllipticalBoundary = True
  opts@mpFillOn = False

  opts@mpGeophysicalLineColor      = get_res_value_keep(opts, "mpGeophysicalLineColor","Gray")
  opts@mpGeophysicalLineThicknessF = get_res_value_keep(opts, "mpGeophysicalLineThicknessF",2.0)


; Set vector resources
  opts = set_vc_resources(opts) 


 opts@gsnDraw = False   
 opts@gsnFrame = False
 opts@gsnMaximize = False
 delete_attrs(opts)
 cn = gsn_csm_vector_map_polar(wks,data_u,data_v,opts)          ; Create the plot.

  draw(cn)


  if ( callFrame ) then
    frame(wks)
  end if

return (cn)
end
;--------------------------------------------------------------------------------

undef("wrf_contour")
function wrf_contour(nc_file:file,wks[1]: graphic, data[*][*]:numeric, \
                     opt_args[1]:logical)
 
; This function creates a contour plot and adds some titles to it.
;
;   1. Determine width to height ratio of plot.
;
;   2. First determine if this is to be a filled or line
;      contour plot (fill_on)
;
;   3. If the ContourParameters attribute is set, then calculate
;      the contour levels.
;
;   4. Set two resources for setting the zero contour line to 
;      a larger thickness, and for changing the negative contour
;      lines to a dashed pattern.
;
;   5. If doing a filled contour plot, set a title for the labelbar
;      based on whether a units attribute is set.
;
;   6. Make a copy of the resource list, and set some additional
;      resources for filled contour plots.
;
;   7. Create the contour plot, attach the titles, and draw
;      and advance the frame (if requested).
 
local dims
begin
  opts = opt_args      ; Make a copy of the resource list.

  if(opts.and.isatt(opts,"mpOutlineBoundarySets")) then
    delete(opts@mpOutlineBoundarySets)
  end if


; Calculate ratio of plot width and height. Note that this doesn't
; affect the setting of gsnMaximize to True, because gsnMaximize will
; retain the aspect ratio of the plot.

  if(opts.and.isatt(opts,"AspectRatio")) then
    ratio = opts@AspectRatio
  else
    dims  = dimsizes(data)
    ratio = 1.*dims(0)/dims(1)
    if(ratio .gt. 1.2) then
      ratio = 1.2
    end if
    if(ratio .lt. 0.6667) then
      ratio = 0.6667
    end if
  end if

  if(ratio .gt. 1) 
    width  = 0.65 * 1.0/ratio
    height = 0.65
  else
    width  = 0.85 
    height = 0.85 * ratio
  end if

  opts@vpWidthF = get_res_value_keep(opts,"vpWidthF",  width)
  opts@vpHeightF = get_res_value_keep(opts,"vpHeightF", height)


; Set some basic contour resources 
  opts = set_cn_resources(data,opts)


; Find out if we are working with a contour or a shaded plot
; fill_on = False : line contour plot
; fill_on = True  : filled contour plot
  fill_on = get_res_value_keep(opts,"cnFillOn",False)
  if(fill_on) then      ; set lb resources if needed
    opts = set_lb_resources(data,opts)
    atmp = get_res_value(opts,"lbLabelBarOn",True) ; Remove this resource
    delete(atmp)                                   ; just in case.
  end if


; Set Title resources
  opts = set_title_resources(data,opts)


; Setting gsnScale to True ensures that the tickmark lengths and labels
; will be the same size on both axes.
  opts@gsnScale    = get_res_value_keep(opts,"gsnScale",    True)


; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.
  opts@gsnDraw          = False    ; Make sure don't draw or frame or,
  opts@gsnFrame         = False    ; maximize, b/c we'll do this later.
  opts@gsnMaximize      = False


  opts2 = opts
  delete_attrs(opts2)                       ; Clean up.
  cn = gsn_contour(wks,data,opts2)          ; Create the plot.
  _SetMainTitle(nc_file,wks,cn,opts)        ; Set some titles


  opts2@gsnDraw     = get_res_value_keep(opts2,"gsnDraw",     False)
  opts2@gsnFrame    = get_res_value_keep(opts2,"gsnFrame",    False)
  opts2@gsnMaximize = get_res_value_keep(opts2,"gsnMaximize", True)
  draw_and_frame(wks,cn,opts2@gsnDraw,opts2@gsnFrame,False,opts2@gsnMaximize)

  return(cn)                                ; Return

end

;--------------------------------------------------------------------------------

undef("wrf_vector")
function wrf_vector(nc_file:file,wks[1]: graphic, data_u[*][*]:numeric, \
                    data_v[*][*]:numeric, opt_args[1]:logical)
;
; This function creates a vector plot and adds some titles to it.
;
;   1. Determine width to height ratio of plot. Will also be use
;      to calculate values for vector resources later.
;
;   2. Make a copy of the resource list, and set some additional
;      resources.
;
;   3. Create the vector plot, attach the titles, and draw
;      and advance the frame (if requested).
 
local dims
begin
  opts = opt_args      ; Make a copy of the resource list.

  if(opts.and.isatt(opts,"mpOutlineBoundarySets")) then
    delete(opts@mpOutlineBoundarySets)
  end if
;
; The ratio is used to determine the width and height of the
; plot, and also to determine the value for the vcMinDistanceF
; resource.
;
  if(opts.and.isatt(opts,"AspectRatio")) then
    ratio = get_res_value(opts,"AspectRatio",0.)
  else
    dims  = dimsizes(data_u)
    ratio = 1.*dims(0)/dims(1)
    if(ratio .gt. 1.2) then
      ratio = 1.2
    end if
    if(ratio .lt. 0.6667) then
      ratio = 0.6667
    end if
  end if

  if(ratio .gt. 1) 
    width  = 0.65/ratio
    height = 0.65
  else
    width  = 0.95 
    height = 0.95 * ratio
  end if

  opts@vpWidthF  = get_res_value_keep(opts,"vpWidthF",  width)
  opts@vpHeightF = get_res_value_keep(opts,"vpHeightF", height)


; Set Title resources
  opts = set_title_resources(data_u,opts)


; Set vector resources
  opts = set_vc_resources(opts)


; Setting gsnScale to True ensures that the tickmark lengths and labels
; will be the same size on both axes.
  opts@gsnScale = get_res_value_keep(opts,"gsnScale",    True)


; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.
  opts@gsnDraw          = False    ; Make sure don't draw or frame or,
  opts@gsnFrame         = False    ; maximize, b/c we'll do this later.
  opts@gsnMaximize      = False

  opts2 = opts
  delete_attrs(opts2)                             ; Clean up.
  vct = gsn_vector(wks,data_u,data_v,opts2)       ; Create vector plot.
  _SetMainTitle(nc_file,wks,vct,opts) 

  opts2@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     False)
  opts2@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    False)
  opts2@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)
  draw_and_frame(wks,vct,opts2@gsnDraw,opts2@gsnFrame,False, \
                 opts2@gsnMaximize)

  return(vct)                                     ; Return.
end


;--------------------------------------------------------------------------------

undef("wrf_map")
function wrf_map(wks[1]:graphic,in_file[1]:file,opt_args[1]:logical)   

begin
;
; This function creates a map plot, and bases the projection on
; the MAP_PROJ attribute in the given file.
;
;   1. Make a copy of the resource list, and set some resources
;      common to all map projections.
;
;   2. Determine the projection being used, and set resources based
;      on that projection.
;
;   3. Create the map plot, and draw and advance the frame
;      (if requested).
;
  if(isatt(in_file,"MAP_PROJ"))
;    if(in_file@MAP_PROJ .eq. 0)
;      print("wrf_map: Error: attribute MAP_PROJ=0.")
;      print("         Data does not have a valid map projection")
;      return(new(1,graphic))
;    end if
;
; There's a bug with map tickmarks that you can't set the length of
; the tickmark to 0.0 in a setvalues call. You have to do it
; during the create call.  Double-check, though, b/c the bug may have
; been fixed by now.
;
    opts = opt_args      ; Make a copy of the resource list
    opts  = True
;
; Set some resources depending on what kind of map projection is 
; chosen.
;
;   MAP_PROJ = 1 : "LambertConformal"
;   MAP_PROJ = 2 : "Stereographic"
;   MAP_PROJ = 3 : "Mercator"
;   MAP_PROJ = 6 : "Lat/Lon"
;
; Set some resources common to all three map projections.
;
    opts@mpNestTime = get_res_value_keep(opts, "mpNestTime",0)
    if(any(in_file@MAP_PROJ.eq.(/0,1,2,3,6/))) then
      if(isfilevar(in_file,"XLAT"))
        lat = in_file->XLAT(opts@mpNestTime,:,:)
        lon = in_file->XLONG(opts@mpNestTime,:,:)
      else
        lat = in_file->XLAT_M(opts@mpNestTime,:,:)
        lon = in_file->XLONG_M(opts@mpNestTime,:,:)
      end if
      dims = dimsizes(lat)

      do ii = 0, dims(0)-1
      do jj = 0, dims(1)-1
        if ( lon(ii,jj) .lt. 0.0) then
          lon(ii,jj) = lon(ii,jj) + 360.
        end if
      end do
      end do
;
; "LowRes" is the default that NCL uses, so you don't need to
; set it here. However, if you want a higher resolution, use 
; "MediumRes". If you want higher resolution for the coastlines,
; then set it to "HighRes", but then you also need to download
; the RANGS-GSHHS database.  Higher resolutions take longer to
; draw.
;
      opts@mpDataBaseVersion     = get_res_value_keep(opts, "mpDataBaseVersion","MediumRes")
     ;opts@mpOutlineBoundarySets = get_res_value_keep(opts, "mpOutlineBoundarySets", "AllBoundaries")
      opts@mpOutlineBoundarySets = get_res_value_keep(opts, "mpOutlineBoundarySets", "GeophysicalAndUSStates")
      opts@mpPerimLineThicknessF = get_res_value_keep(opts, "mpPerimLineThicknessF", 1.0)
      opts@tmXBLabelFontHeightF  = get_res_value_keep(opts, "tmXBLabelFontHeightF", 0.01)
      opts@tmYLLabelFontHeightF  = get_res_value_keep(opts, "tmYLLabelFontHeightF", 0.01)
;
; Select portion of the map to view.
;
    ;if(any(in_file@MAP_PROJ.eq.(/1,2,3/))) then
      opts@mpLimitMode           = get_res_value_keep(opts, "mpLimitMode","Corners")
      opts@mpLeftCornerLatF      = get_res_value_keep(opts, "mpLeftCornerLatF",lat(0,0))
      opts@mpLeftCornerLonF      = get_res_value_keep(opts, "mpLeftCornerLonF",lon(0,0))
      opts@mpRightCornerLatF     = get_res_value_keep(opts, "mpRightCornerLatF", lat(dims(0)-1,dims(1)-1))
      opts@mpRightCornerLonF     = get_res_value_keep(opts, "mpRightCornerLonF", lon(dims(0)-1,dims(1)-1))
      if ( isatt(opts,"ZoomIn") .and. opts@ZoomIn ) then

        y1 = 0
        x1 = 0
        y2 = dims(0)-1
        x2 = dims(1)-1
        if ( isatt(opts,"Ystart") ) then
          y1 = opts@Ystart
          delete(opts@Ystart)
        end if
        if ( isatt(opts,"Xstart") ) then
          x1 = opts@Xstart
          delete(opts@Xstart)
        end if
        if ( isatt(opts,"Yend") ) then
          if ( opts@Yend .le. y2 ) then
            y2 = opts@Yend
          end if
          delete(opts@Yend)
        end if
        if ( isatt(opts,"Xend") ) then
          if ( opts@Xend .le. x2 ) then
            x2 = opts@Xend
          end if
          delete(opts@Xend)
        end if

        opts@mpLeftCornerLatF      = lat(y1,x1)
        opts@mpLeftCornerLonF      = lon(y1,x1)
        opts@mpRightCornerLatF     = lat(y2,x2)
        opts@mpRightCornerLonF     = lon(y2,x2)

        delete(opts@ZoomIn)
      end if

      if ( opts@mpRightCornerLonF .lt. 0.0 ) then
        opts@mpRightCornerLonF  = opts@mpRightCornerLonF + 360.0
        ;print(opts@mpLeftCornerLonF)
        ;print(opts@mpRightCornerLonF)
      end if 
    ;end if
;
; Set some other resources for line colors and grid spacing.
;
      opts@mpGeophysicalLineColor      = get_res_value_keep(opts, "mpGeophysicalLineColor","Gray")
      opts@mpGeophysicalLineThicknessF = get_res_value_keep(opts, "mpGeophysicalLineThicknessF",0.5)
      opts@mpGridLineColor             = get_res_value_keep(opts, "mpGridLineColor","Gray")
      opts@mpGridLineThicknessF        = get_res_value_keep(opts, "mpGridLineThicknessF",0.5)
      ;opts@mpGridMaskMode              = get_res_value_keep(opts, "mpGridMaskMode",3)
      opts@mpGridSpacingF              = get_res_value_keep(opts, "mpGridSpacingF",5)
      opts@mpLimbLineColor             = get_res_value_keep(opts, "mpLimbLineColor","Gray")
      opts@mpLimbLineThicknessF        = get_res_value_keep(opts, "mpLimbLineThicknessF",0.5)
      opts@mpNationalLineColor         = get_res_value_keep(opts, "mpNationalLineColor","Gray")
      opts@mpNationalLineThicknessF    = get_res_value_keep(opts, "mpNationalLineThicknessF",0.5)
      opts@mpPerimLineColor            = get_res_value_keep(opts, "mpPerimLineColor","Gray")
      opts@mpPerimOn                   = get_res_value_keep(opts, "mpPerimOn",True)
      opts@mpUSStateLineColor          = get_res_value_keep(opts, "mpUSStateLineColor","Gray")
      opts@mpUSStateLineThicknessF     = get_res_value_keep(opts, "mpUSStateLineThicknessF",0.5)
      opts@pmTickMarkDisplayMode       = get_res_value_keep(opts, "pmTickMarkDisplayMode","Always")

;
; Tick mark resources
; 
     ;opts@tmXBMajorLengthF      = get_res_value(opts, "tmXBMajorLengthF",-0.03)
     ;opts@tmYLMajorLengthF      = get_res_value(opts, "tmYLMajorLengthF",-0.03)
      opts@tmXTOn                = get_res_value(opts,"tmXTOn",False)
      opts@tmYROn                = get_res_value(opts,"tmYROn",False)
      opts@tmYRLabelsOn          = get_res_value(opts,"tmYRLabelsOn",True)
      opts@tmXBBorderOn          = get_res_value(opts,"tmXBBorderOn",True)
      opts@tmXTBorderOn          = get_res_value(opts,"tmXTBorderOn",True)
      opts@tmYLBorderOn          = get_res_value(opts,"tmYLBorderOn",True)
      opts@tmYRBorderOn          = get_res_value(opts,"tmYRBorderOn",True)
;
    end if

;
; LambertConformal projection
;
    if(in_file@MAP_PROJ .eq. 1)
      projection               = "LambertConformal"

      opts@mpLambertParallel1F = get_res_value_keep(opts, "mpLambertParallel1F",in_file@TRUELAT1)
      opts@mpLambertParallel2F = get_res_value_keep(opts, "mpLambertParallel2F",in_file@TRUELAT2)
      if(isatt(in_file,"STAND_LON"))
        opts@mpLambertMeridianF  = get_res_value_keep(opts, "mpLambertMeridianF",in_file@STAND_LON)
      else
        if(isatt(in_file,"CEN_LON"))
          opts@mpLambertMeridianF  = get_res_value_keep(opts, "mpLambertMeridianF",in_file@CEN_LON)
        else
         print("ERROR: Found neither STAND_LON or CEN_LON in file")
        end if
      end if
    end if

;
; Stereographic projection
;
    if(in_file@MAP_PROJ .eq. 2)
      projection          = "Stereographic"
      opts@mpCenterLatF   = get_res_value_keep(opts, "mpCenterLatF", in_file@CEN_LAT)
      if(isatt(in_file,"STAND_LON"))
        opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@STAND_LON)
      else
        if(isatt(in_file,"CEN_LON"))
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@CEN_LON)
        else
         print("ERROR: Found neither STAND_LON or CEN_LON in file")
        end if
      end if
    end if

;
; Mercator projection
;
    if(in_file@MAP_PROJ .eq. 3)
      projection          = "Mercator"
      opts@mpCenterLatF   = get_res_value_keep(opts, "mpCenterLatF", 0.0)
      if(isatt(in_file,"STAND_LON"))
        opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@STAND_LON)
      else
        if(isatt(in_file,"CEN_LON"))
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@CEN_LON)
        else
         print("ERROR: Found neither STAND_LON or CEN_LON in file")
        end if
      end if
    end if

;
;  global WRF CylindricalEquidistant
;
    if(in_file@MAP_PROJ .eq. 6)
      projection          = "CylindricalEquidistant"
      opts@mpGridSpacingF = 45
      opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@CEN_LON)
      if( isatt(in_file,"POLE_LAT") ) then
        opts@mpCenterRotF = get_res_value_keep(opts, "mpCenterRotF", 90.0 - in_file@POLE_LAT) 
      end if
    end if

;
; CylindricalEquidistant
;
    if(in_file@MAP_PROJ .eq. 0)
      projection          = "CylindricalEquidistant"
      opts@mpGridSpacingF = 45
      opts@mpCenterLatF   = get_res_value_keep(opts, "mpCenterLatF", 0.0)
      if(isatt(in_file,"STAND_LON"))
        opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@STAND_LON)
      else
        if(isatt(in_file,"CEN_LON"))
          opts@mpCenterLonF  = get_res_value_keep(opts, "mpCenterLonF",in_file@CEN_LON)
        else
         print("ERROR: Found neither STAND_LON or CEN_LON in file")
        end if
      end if
    end if
  else
    print("wrf_map: Error: no MAP_PROJ attribute in input file")
    return(new(1,graphic))
  end if

;
; The default is not to draw the plot or advance the frame, and
; to maximize the plot in the frame.
;
  opts@gsnDraw       = get_res_value_keep(opts,"gsnDraw",     False)
  opts@gsnFrame      = get_res_value_keep(opts,"gsnFrame",    False)
  opts@gsnMaximize   = get_res_value_keep(opts,"gsnMaximize", True)

  delete_attrs(opts)                             ; Clean up.
  mp = gsn_map(wks,projection,opts)              ; Create map plot.
  return(mp)                                     ; Return.

end

;--------------------------------------------------------------------------------

undef("wrf_map_overlays")
function wrf_map_overlays(in_file[1]:file, \
                          wks:graphic, \
                          plots[*]:graphic, \
                          opt_arg[1]:logical, \
                          opt_mp[1]:logical) 
 
; This procedure takes an array of plots and overlays them on a
; base plot - map background.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
;
;  Attributes recognized by this procedure:
;     FramePlot
;     PanelPlot
;     NoTitles                  (don't do any titles) 
;     CommonTitle & PlotTile is used to overwrite field titles
;        CommonTitle will super-seed NoTitles
;
; If FramePlot False, then Draw the plot but do not Frame.
; In this case a user want to add to the drawing, and will
; have to advance the Frame manually in the script.
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to wrf_map_overlays
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
begin

  ; Let's make the map first
  base = wrf_map(wks,in_file,opt_mp)

  opts = opt_arg      ; Make a copy of the resource list

  no_titles  = get_res_value(opts,"NoTitles",False)     ; Do we want field titles?
  com_title  = get_res_value(opts,"CommonTitle",False)     ; Do we have a common title?
  if ( com_title ) then
    plot_title = get_res_value(opts,"PlotTitle","  ")
    no_titles = True
  end if
  
  call_draw  = True
  call_frame = get_res_value(opts,"FramePlot",True)     ; Do we want to frame the plot?
  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  nplots = dimsizes(plots)
;  font_color = "Black"

  do i=0,nplots-1
    if(.not.ismissing(plots(i))) then
;      class_name = NhlClassName(plots(i))
;      print(class_name)
;      if(class_name.eq."contourPlotClass") then
;        getvalues plots(i)
;          "cnFillOn"    : fill_on
;          "cnLineColor" : line_color
;        end getvalues
;        if (.not.fill_on) then
;          font_color = line_color      
;        end if       
;      end if
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        if(i.eq.0) then
          SubTitles = SubTitle
        else
          SubTitles = SubTitles + "~C~" + SubTitle
        end if
      end if
      if(com_title .and. i .eq. nplots-1) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        SubTitles = plot_title
      end if
      setvalues plots(i)
        "tfDoNDCOverlay" : True
        "tiMainOn"       : False
      end setvalues
      overlay(base,plots(i))
    else
      print("wrf_map_overlays: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles .or. com_title) then
    font_height = get_res_value_keep(opts,"FontHeightF",0.01)
    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
     ;"txFontColor"   : font_color
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 3
      "amJust"           : "BottomLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      "amOrthogonalPosF" : 0.03
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if
;
; gsnDraw and gsnFrame default to False if panel plot.
;
  if(panel_plot) then
    call_draw = False
    call_frame= False
  end if


  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.panel_plot) then
    do i=0,nplots-1
      if(.not.ismissing(plots(i))) then
        NhlRemoveOverlay(base,plots(i),False)
      else
        print("wrf_remove_map_overlays: Warning: overlay plot #" + i + " is not valid.")
        print("                        Nothing to remove.")
      end if
    end do
  end if

  if(.not.no_titles.and..not.panel_plot) then
    if(isatt(base,"map_titles")) then
      NhlRemoveAnnotation(base,base@map_titles)
      delete(base@map_titles)
    end if
  end if

return(base)
end

;--------------------------------------------------------------------------------

undef("wrf_overlays")
function wrf_overlays(in_file[1]:file, \
                     wks:graphic, plots[*]:graphic, \
                     opt_arg[1]:logical)
 
; This procedure takes an array of plots and overlays them.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
; 
;  Attributes recognized by this procedure:
;     FramePlot
;     PanelPlot
;     NoTitles                  (don't do any titles) 
;     CommonTitle & PlotTile is used to overwrite field titles
;        CommonTitle will super-seed NoTitles
;
; If FramePlot False, then Draw the plot but do not Frame.
; In this case a user want to add to the drawing, and will
; have to advance the Frame manually in the script.
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to wrf_overlays
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
begin
  opts = opt_arg      ; Make a copy of the resource list.

  no_titles  = get_res_value(opts,"NoTitles",False)     ; Do we want field titles?
  com_title  = get_res_value(opts,"CommonTitle",False)     ; Do we have a common title?
  if ( com_title ) then
    plot_title = get_res_value(opts,"PlotTitle","  ")
    no_titles = True
  end if

  call_draw  = True
  call_frame = get_res_value(opts,"FramePlot",True)     ; Do we want to frame the plot?
  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  nplots = dimsizes(plots)
    
  base = plots(0)
  if(.not.no_titles) then
    getvalues plots(0)
      "tiMainString" : SubTitle
    end getvalues
    SubTitles = SubTitle
    setvalues plots(0)
      "tfDoNDCOverlay" : True
      "tiMainOn"       : False
    end setvalues
  else
    setvalues plots(0)
      "tfDoNDCOverlay" : True
    end setvalues
  end if

  if (nplots.eq.1) then 
    blank = create "BlankPlot" logLinPlotClass wks
      ;"cnConstFLabelOn" : False
    end create
    overlay(base,blank)
  end if

  do i=1,nplots-1
    if(.not.ismissing(plots(i))) then
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        if(i.eq.0) then
          SubTitles = SubTitle
        else
          SubTitles = SubTitles + "~C~" + SubTitle
        end if
      end if
      if(com_title .and. i .eq. nplots-1) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        SubTitles = plot_title
      end if
      setvalues plots(i)
        "tfDoNDCOverlay" : True
        "tiMainOn"       : False
      end setvalues
      overlay(base,plots(i))
    else
      print("wrf_overlays: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles .or. com_title) then
    font_height = get_res_value_keep(opt_arg,"FontHeightF",0.01)

    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 3
      "amJust"           : "BottomLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      "amOrthogonalPosF" : 0.03
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if

;
; gsnDraw and gsnFrame should default to True if not a panel plot.
;  
  if(panel_plot) then
    call_draw = False
    call_frame= False
  end if

  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.no_titles.and..not.panel_plot) then
    NhlRemoveAnnotation(base,base@map_titles)
    delete(base@map_titles)
  end if

  if(.not.panel_plot) then
    if ( nplots .ge. 2 ) then
      do i=1,nplots-1
        if(.not.ismissing(plots(i))) then
          NhlRemoveOverlay(base,plots(i),False)
        else
          print("wrf_remove_overlays: Warning: overlay plot #" + i + " is not valid.")
          print("                        Nothing to remove.")
        end if
      end do
    end if
  end if

return(base)
end

;--------------------------------------------------------------------------------

undef("wrf_map_zoom")
function wrf_map_zoom(wks[1]:graphic,in_file[1]:file,opt_args[1]:logical, \
                      y1:integer,y2:integer,x1:integer,x2:integer)

; As of version 5.0.1, this routine is redundant. Use the special "ZoomIn"
; resource in wrf_map to accomplish the same thing. This function is
; being kept for backwards capability. There should be no need for it
; except to run old WRF-NCL codes.  Do not make any changes to it except
; possibly to fix bugs.
;
begin
      print("wrf_map_zoom: Warning: This function is obsolete. Consider using")
      print("                       the 'ZoomIn' resource in wrf_map instead.")
      if(isfilevar(in_file,"XLAT"))
        lat = in_file->XLAT(0,:,:)
        lon = in_file->XLONG(0,:,:)
      else
        lat = in_file->XLAT_M(0,:,:)
        lon = in_file->XLONG_M(0,:,:)
      end if
      opts  = opt_args      ; Make a copy of the resource list
      opts  = True
      opts@mpLeftCornerLatF      = lat(y1,x1)
      opts@mpLeftCornerLonF      = lon(y1,x1)
      opts@mpRightCornerLatF     = lat(y2,x2)
      opts@mpRightCornerLonF     = lon(y2,x2)
      mz = wrf_map(wks,in_file,opts)
      return(mz)
end

;--------------------------------------------------------------------------------

undef("wrf_map_overlay")
procedure wrf_map_overlay(wks:graphic,base[1]:graphic, \
                          plots[*]:graphic, \
                          opt_arg[1]:logical)
 
; As of version 5.0.1, this procedure is obsolete. Use wrf_map_overlays
; instead. It is being kept for backwards capability. Do not make any
; changes to it except possibly to fix bugs.
;
; This procedure takes an array of plots and overlays them on a
; base plot - map background.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
;
;  Attributes recognized by this procedure:
;     NoTitles                  (don't do any titles) 
;     PanelPlot
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to wrf_map_overlay
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
begin
  print("wrf_map_overlay: Warning: This procedure is obsolete. Consider" )
  print("                          using wrf_map_overlays instead.")

  opts = opt_arg      ; Make a copy of the resource list
  opts = True

  if(opts.and.isatt(opts,"NoTitles").and.opts@NoTitles) then
    no_titles = True
  else
    no_titles = False
  end if

  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?

  nplots = dimsizes(plots)
;  font_color = "Black"

  do i=0,nplots-1
    if(.not.ismissing(plots(i))) then
;      class_name = NhlClassName(plots(i))
;      print(class_name)
;      if(class_name.eq."contourPlotClass") then
;        getvalues plots(i)
;          "cnFillOn"    : fill_on
;          "cnLineColor" : line_color
;        end getvalues
;        if (.not.fill_on) then
;          font_color = line_color      
;        end if       
;      end if
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        if(i.eq.0) then
          SubTitles = SubTitle
        else
          SubTitles = SubTitles + "~C~" + SubTitle
        end if
	setvalues plots(i)
          "tfDoNDCOverlay" : True
          "tiMainOn"       : False
        end setvalues
      else
	setvalues plots(i)
          "tfDoNDCOverlay" : True
        end setvalues
      end if
      overlay(base,plots(i))
    else
      print("wrf_map_overlay: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles) then
    font_height = get_res_value_keep(opt_arg,"FontHeightF",0.01)
    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
     ;"txFontColor"   : font_color
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 3
      "amJust"           : "BottomLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      "amOrthogonalPosF" : 0.03
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if
;
; gsnDraw and gsnFrame should default to True if not a panel plot.
; gsnFrame will default to False if opt_arg is False.
;
  if(panel_plot.or..not.opt_arg) then
    call_frame = False
  else
    call_frame = True
  end if
  if(panel_plot) then
    call_draw = False
  else
    call_draw = True
  end if
  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.panel_plot) then
    do i=0,nplots-1
      if(.not.ismissing(plots(i))) then
        NhlRemoveOverlay(base,plots(i),False)
      else
        print("wrf_remove_map_overlay: Warning: overlay plot #" + i + " is not valid.")
        print("                        Nothing to remove.")
      end if
    end do
  end if

  if(.not.no_titles.and..not.panel_plot) then
    if(isatt(base,"map_titles")) then
      NhlRemoveAnnotation(base,base@map_titles)
      delete(base@map_titles)
    end if
  end if
end

;--------------------------------------------------------------------------------

undef("wrf_overlay")
procedure wrf_overlay(wks:graphic, plots[*]:graphic, \
                          opt_arg[1]:logical)
 
; As of version 5.0.1, this procedure is obsolete. Use wrf_overlays
; instead. It is being kept for backwards capability. Do not make any
; changes to it except possibly to fix bugs.
;
; This procedure takes an array of plots and overlays them.
;
; It will advance the plot and cleanup, unless you set the
; PanelPlot resource to True.
;
;  Attributes recognized by this procedure:
;     NoTitles                  (don't do any titles) 
;     PanelPlot
;
; If the "NoTitles" attribute exists and is set True, then
; don't create the top-left titles, and leave the main titles alone.
; This resource can be useful if you are planning to panel
; the plots.
;
; If PanelPlot is set to True, then this flags to wrf_overlay
; that these plots are going to be eventually paneled (likely
; by gsn_panel), and hence 1) draw and frame should not be called
; (unless gsnDraw and/or gsnFrame are explicitly set to True),
; and 2) the overlays and titles should not be removed with
; NhlRemoveOverlay and NhlRemoveAnnotation.
;
begin
  print("wrf_overlay: Warning: This procedure is obsolete. Consider using")
  print("                      wrf_overlays instead.")

  opts = opt_arg      ; Make a copy of the resource list.
  opts = True

  if(opts.and.isatt(opts,"NoTitles").and.opts@NoTitles) then
    no_titles = True
  else
    no_titles = False
  end if

  panel_plot = get_res_value(opts,"PanelPlot",False)    ; Are we paneling?

  nplots = dimsizes(plots)
    
  base = plots(0)
  if(.not.no_titles) then
    getvalues plots(0)
      "tiMainString" : SubTitle
    end getvalues
    SubTitles = SubTitle
    setvalues plots(0)
      "tfDoNDCOverlay" : True
      "tiMainOn"       : False
    end setvalues
  else
    setvalues plots(0)
      "tfDoNDCOverlay" : True
    end setvalues
  end if

  if (nplots.eq.1) then 
    blank = create "BlankPlot" logLinPlotClass wks
      ;"cnConstFLabelOn" : False
    end create
    overlay(base,blank)
  end if

  do i=1,nplots-1
    if(.not.ismissing(plots(i))) then
      if(.not.no_titles) then
        getvalues plots(i)
          "tiMainString" : SubTitle
        end getvalues
        SubTitles = SubTitles + "~C~" + SubTitle
        setvalues plots(i)
          "tfDoNDCOverlay" : True
          "tiMainOn"       : False
        end setvalues
      else
        setvalues plots(i)
          "tfDoNDCOverlay" : True
        end setvalues
      end if
      overlay(base,plots(i))
    else
      print("wrf_overlay: Warning: overlay plot #" + i + " is not valid.")
    end if
  end do

  if(.not.no_titles) then
    font_height = get_res_value_keep(opt_arg,"FontHeightF",0.01)

    txt = create "map_titles" textItemClass wks
      "txString"      : SubTitles
      "txFontHeightF" : font_height
    end create
    anno = NhlAddAnnotation(base,txt)
    setvalues anno
      "amZone"           : 3
      "amJust"           : "BottomLeft"
      "amSide"           : "Top"
      "amParallelPosF"   : 0.005
      "amOrthogonalPosF" : 0.03
      "amResizeNotify"   : False
    end setvalues
    base@map_titles = anno
  end if

;
; gsnDraw and gsnFrame should default to True if not a panel plot.
; gsnFrame will default to False if opt_arg is False.
;
  if(panel_plot.or..not.opt_arg) then
    call_frame = False
  else
    call_frame = True
  end if
  if(panel_plot) then
    call_draw = False
  else
    call_draw = True
  end if
  opts@gsnDraw     = get_res_value_keep(opts,"gsnDraw",     call_draw)
  opts@gsnFrame    = get_res_value_keep(opts,"gsnFrame",    call_frame)
  opts@gsnMaximize = get_res_value_keep(opts,"gsnMaximize", True)

  draw_and_frame(wks,base,opts@gsnDraw,opts@gsnFrame,False, \
                 opts@gsnMaximize)

  if(.not.no_titles.and..not.panel_plot) then
    NhlRemoveAnnotation(base,base@map_titles)
    delete(base@map_titles)
  end if

  if(.not.panel_plot) then
    if ( nplots .ge. 2 ) then
      do i=1,nplots-1
        if(.not.ismissing(plots(i))) then
          NhlRemoveOverlay(base,plots(i),False)
        else
          print("wrf_remove_overlay: Warning: overlay plot #" + i + " is not valid.")
          print("                        Nothing to remove.")
        end if
      end do
    end if
  end if
end

;--------------------------------------------------------------------------------

undef("add_white_space")
function add_white_space(str:string,maxlen:integer)

begin
  cstr = stringtochar(str)
  len  = dimsizes(cstr)-1
  ws   = ""
  if(len.lt.maxlen) then
    do i=1,maxlen-len
      ws = ws + " "
    end do
  end if
  return(ws)

end

;--------------------------------------------------------------------------------

undef("print_opts")
procedure print_opts(opts_name,opts,debug)

begin
  if(.not.debug) then
    return
  end if
  varatts = getvaratts(opts)
;
; Sort attributes alphabetically/
;
  sqsort(varatts)
;
; Get length of longest attribute name.
;
  cvaratts = stringtochar(varatts)
  cmaxlen  = dimsizes(cvaratts(0,:))-1

  print("------------------------------------------------------------")
  print(opts_name + "...")    ; Print name of option variable.
;
; Loop through each attribute in the list. If its value is an array,
; then print out the array with '(/' and '/)' around it.
;
; If the value is a string, then put ticks (') around each string.
;
  do i=0,dimsizes(varatts)-1
    x = opts@$varatts(i)$
;
; Use add_white_space to make sure all the equal signs line up.
;
    tmp_str = "    " + varatts(i) + \
              add_white_space(varatts(i),cmaxlen) + " = "
;
; Check if attribute is an array.
;    
    if(dimsizes(x).gt.1) then
      tmp_str = tmp_str + "(/"
      do j=0,dimsizes(x)-1
        if(typeof(x(j)).eq."string") then
          tmp_str = tmp_str + "'" + x(j) + "'"
        else
          tmp_str = tmp_str + x(j)
        end if
        if(j.lt.dimsizes(x)-1) then
          tmp_str = tmp_str + ","
        else
          tmp_str = tmp_str + "/)"
        end if
      end do
    else      if(typeof(x).eq."string") then
        tmp_str = tmp_str + "'" + x + "'"
      else
        tmp_str = tmp_str + x
      end if
    end if
    print("" + tmp_str)
    delete(x)
  end do

end

;--------------------------------------------------------------------------------

undef("print_header")
procedure print_header(icount:integer,debug)
begin
  icount = icount + 1
  if(.not.debug) then
    return
  end if
  print("END plot #" + icount)
  print("------------------------------------------------------------")

end

;--------------------------------------------------------------------------------
