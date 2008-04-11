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
function wrf_user_intrp3d( var3d:numeric, z:numeric, \
                           plot_type:string, \
                           loc_param:numeric, angle:numeric, opts:logical )

; var3d      - field to inerpolate (all input fields must be unstaggered
; z          - interpolate to this field (either p/z)
; plot_type  - interpolate horizontally "h", or vertically "v"
; loc_param  - level for horizontal plots (eg. 500hPa ; 3000m - scalar), 
;              plane for vertical plots (2 values representing an xy point 
;              on the model domain through which the vertical plane will pass
;              OR 4 values specifying start and end values
; angle      - 0.0 for horizontal plots, and 
;              an angle for vertical plots - 90 represent a WE cross section
; opts         Used IF opts is TRUE, else use loc_param and angle to determine crosssection

begin

  if(plot_type .eq. "h" ) then   ;  horizontal cross section needed

     var2d = wrf_interp_3d_z(var3d,z,loc_param)

     if ( z(1,1,1) .gt. 500.) then
        var2d@PlotLevelID = loc_param + " hPa"
     else
        var2d@PlotLevelID = .001*loc_param + " km"
     end if

  end if




  if(plot_type .eq. "v" ) then   ;  vertical cross section needed

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


; first we interp the variable, then z
     var2dtmp = wrf_interp_2d_xy( var3d, xy)
     var2dz   = wrf_interp_2d_xy( z, xy)


;  interp to constant z grid
     z_max = max(var2dz)
     z_min = min(var2dz)
     dz = 0.01*(z_max - z_min)
     nlevels = 101
     z_var2d = new( (/nlevels/), typeof(var2dz))
     z_var2d(0) = z_min

     var2d = new( (/nlevels, xp(0)/), typeof(var2dz))
 
     if(var2dz(0,0) .gt. var2dz(1,0) ) then  ; monotonically decreasing coordinate
        dz = -dz
        z_var2d(0) = z_max
     end if

   
     do i=1, nlevels-1
        z_var2d(i) = z_var2d(0)+i*dz
     end do

     do i=0,xp(0)-1
        var2d(:,i) = wrf_interp_1d( var2dtmp(:,i), var2dz(:,i), z_var2d)
     end do


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

  end if



  return(var2d)

end

;--------------------------------------------------------------------------------
 
undef("wrf_user_intrp2d")
function wrf_user_intrp2d( var2d:numeric, \
                           loc_param:numeric, angle:numeric, opts:logical )

; var2d      - field to inerpolate 
; loc_param  - plane for vertical plots (2 values representing an xy point 
;              on the model domain through which the vertical plane will pass
;              OR 4 values specifying start and end values
; angle      - 0.0 for horizontal plots, and 
;              an angle for vertical plots - 90 represent a WE cross section
; opts         Used IF opts is TRUE, else use loc_param and angle to determine crosssection

begin

     dims = dimsizes(var2d)
     var2dtmp = new( (/1,dims(0),dims(1)/), typeof(var2d))
     var2dtmp(0,:,:) = var2d(:,:)

; set vertical cross section
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

     var1dtmp = wrf_interp_2d_xy( var2dtmp, xy)

     dims = dimsizes(var1dtmp)
     var = new( (/dims(1)/), typeof(var1dtmp))
     var(:) = var1dtmp(0,:)

  return(var)

end

 
undef("wrf_user_getvar")
function wrf_user_getvar( nc_file:file, variable:string, time:integer )

begin

  if( (variable .eq. "uvmet") ) then
       ;; Calculate winds rotated to earth coord. 
       ;; Return a 4D array where uvmet(0,:,:,:) is umet and 
       ;; uvmet(1,:,:,:) is vmet

       pii = 3.14159265
       radians_per_degree = pii/180.

       v = nc_file->V(time,:,:,:)
       dimv = dimsizes(v)
       u = nc_file->U(time,:,:,:)
       dimu = dimsizes(u)

       uvmet = new( (/ 2, dimu(0), dimu(1), dimv(2)/), typeof(u))

       map_projection = nc_file@MAP_PROJ


       if(map_projection .eq. 0) then  ; no projection
           uvmet(0,:,:,:) = 0.5*(u(:,:,:dimv(2)-2) + u(:,:,1:dimv(2)-1))
           uvmet(1,:,:,:) = 0.5*(v(:,:dimv(1)-2,:) + v(:,1:dimv(1)-1,:))
           uvmet@description = " u,v met velocity"
           uvmet@units = "m/s"
           return(uvmet)
       end if


       cen_lat  = nc_file@CEN_LAT
       if(isatt(nc_file,"STAND_LON")) then
           cen_long = nc_file@STAND_LON
       else
           cen_long = nc_file@CEN_LON
       end if
       true_lat1 = nc_file@TRUELAT1
       true_lat2 = nc_file@TRUELAT2
       if(isfilevar(nc_file,"XLAT"))
         latitude  = nc_file->XLAT(0,:,:)
         longitude = nc_file->XLONG(0,:,:)
       else
         latitude  = nc_file->XLAT_M(0,:,:)
         longitude = nc_file->XLONG_M(0,:,:)
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
       if(map_projection .eq. 2) then      ; polar steraographic
         cone = 1.
       end if
       if(map_projection .eq. 3) then      ; Mercator
         cone = 0.
       end if

       uvmet = wrf_uvmet( u,v, latitude, longitude, cen_long, cone )

     return(uvmet)

  end if


  if( variable .eq. "ua" ) then
       ; U interpolated to mass points
       U   = nc_file->U(time,:,:,:)
       dim = dimsizes(U)
       ua  = 0.5*(U(:,:,:dim(2)-2) + U(:,:,1:dim(2)-1))
       ua@description = "u Velocity"
       ua@units       = "m/s"
       return(ua)
  end if


  if( variable .eq. "va" ) then
       ; V interpolated to mass points
       V   = nc_file->V(time,:,:,:)
       dim = dimsizes(V)
       va  = 0.5*(V(:,:dim(1)-2,:)+V(:,1:dim(1)-1,:))
       va@description = "v Velocity"
       va@units       = "m/s"
       return(va)
  end if


  if( variable .eq. "wa" ) then
       ; W interpolated to mass points
       W   = nc_file->W(time,:,:,:)
       dim = dimsizes(W)
       wa  = 0.5*(W(0:dim(0)-2,:,:)+W(1:dim(0)-1,:,:))
       wa@description = "Vertical Velocity w"
       wa@units       = "m/s"
       return(wa)
  end if


  if( variable .eq. "pressure" ) then
       ; Full model pressure [=base pressure (PB) + pertubation pressure (P)]
       P  = nc_file->P(time,:,:,:)
       PB = nc_file->PB(time,:,:,:)
       pressure  = 0.01 * ( P + PB )
       pressure@description = "Pressure"
       pressure@units       = "hPa"
       return(pressure)
  end if


  if( variable .eq. "z" ) then
       ; Height [=full geopotentail height / 9.81]
       PH  = nc_file->PH(time,:,:,:)
       PHB = nc_file->PHB(time,:,:,:)
       var = ( PH + PHB ) / 9.81
       dim = dimsizes(var)
       z   = 0.5*(var(0:dim(0)-2,:,:)+var(1:dim(0)-1,:,:))
       z@description = "Height"
       z@units       = "m"
       return(z)
  end if


  if( variable .eq. "th" ) then
       ; Potentail Temperature is model output T + 300K
       T  = nc_file->T(time,:,:,:)
       th = T + 300.
       th@description = "Potential Temperature (theta) "
       th@units       = "K"
       return(th)
  end if


  if( variable .eq. "tk" ) then
       ;; function wrf_tk needs theta and pressure (Pa) on input and returns temperature in K on return
       T  = nc_file->T(time,:,:,:)
       th = T + 300.
       P  = nc_file->P(time,:,:,:)
       PB = nc_file->PB(time,:,:,:)
       p  = P + PB
       tk = wrf_tk( p , th )
       return(tk)
  end if


  if( variable .eq. "tc" ) then
       ;; function wrf_tk needs theta and pressure (Pa) on input and returns temperature in K on return
       T  = nc_file->T(time,:,:,:)
       th = T + 300.
       P  = nc_file->P(time,:,:,:)
       PB = nc_file->PB(time,:,:,:)
       p  = P + PB
       tc = wrf_tk( p , th )
       tc = tc - 273.16
       tc@units = "C"     ; Overwrite return units
       return(tc)
  end if


  if( variable .eq. "td" ) then
       ;; function wrf_td needs qv and pressure (Pa) on input and returns dewpoint temperature on return
       QVAPOR = nc_file->QVAPOR(time,:,:,:)
       QVAPOR = QVAPOR > 0.0
       P      = nc_file->P(time,:,:,:)
       PB     = nc_file->PB(time,:,:,:)
       p      = P + PB
       td    = wrf_td( p , QVAPOR )
       return(td)
  end if


  if( variable .eq. "td2" ) then
       ;; function wrf_td needs qv and pressure (Pa) on input and returns dewpoint temperature on return
       Q2  = nc_file->Q2(time,:,:)
       Q2  = Q2 > 0.0
       P   = nc_file->P(time,0,:,:)      ; get only the lowest level, to match Q2
       PB  = nc_file->PB(time,0,:,:)     ; get only the lowest level, to match Q2
       p   = P + PB
       td2 = wrf_td( p , Q2 )
       td2@description = "2m Dewpoint Temperature"   ; Overwrite return description
       return(td2)
  end if


  if( variable .eq. "slp" ) then
       ;; first compute theta
       ;; function wrf_tk needs theta and pressure (Pa) on input
       T  = nc_file->T(time,:,:,:)
       th = T + 300.
       P  = nc_file->P(time,:,:,:)
       PB = nc_file->PB(time,:,:,:)
       p  = P + PB
       tk = wrf_tk( p , th )

       ;; now compute sea level pressure, from qv, p (Pa), tk, z
       QVAPOR = nc_file->QVAPOR(time,:,:,:)
       QVAPOR = QVAPOR > 0.000
       PH     = nc_file->PH(time,:,:,:)
       PHB    = nc_file->PHB(time,:,:,:)
       var    = ( PH + PHB ) / 9.81
       dim    = dimsizes(var)
       z      = 0.5 * ( var(0:dim(0)-2,:,:) + var(1:dim(0)-1,:,:) )

       ;; get slp
       slp   = wrf_slp( z, tk, p, QVAPOR )
       return(slp)
  end if


  if( variable .eq. "rh" ) then
       ;; first compuet tk
       ;; function wrf_tk needs theta and pressure (Pa) on input
       T  = nc_file->T(time,:,:,:)
       th = T + 300.
       P  = nc_file->P(time,:,:,:)
       PB = nc_file->PB(time,:,:,:)
       p  = P + PB
       tk = wrf_tk( p , th )

       ;; now compute rh, using tk, p (Pa), QVAPOR
       QVAPOR = nc_file->QVAPOR(time,:,:,:)
       QVAPOR = QVAPOR > 0.000
       rh     = wrf_rh( QVAPOR, p, tk )
       return(rh)
  end if



;  end of diagnostic variable list - we must want a variable already in
;  the file.  check variable dimensionality and pull proper time  out of file

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

  if(isfilevar(nc_file,"XLAT"))
    XLAT  = nc_file->XLAT(0,:,:)
    XLONG = nc_file->XLONG(0,:,:)
  else
    XLAT  = nc_file->XLAT_M(0,:,:)
    XLONG = nc_file->XLONG_M(0,:,:)
  end if

  loc = wrf_latlon_to_ij( XLAT, XLONG, latitude, longitude )

  return(loc)

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
    if(any(in_file@MAP_PROJ.eq.(/1,2,3,6/))) then
      if(isfilevar(in_file,"XLAT"))
        lat = in_file->XLAT(0,:,:)
        lon = in_file->XLONG(0,:,:)
      else
        lat = in_file->XLAT_M(0,:,:)
        lon = in_file->XLONG_M(0,:,:)
      end if
      dims = dimsizes(lat)
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
      opts@mpGridMaskMode              = get_res_value_keep(opts, "mpGridMaskMode",3)
      opts@mpGridSpacingF              = get_res_value_keep(opts, "mpGridSpacingF",10)
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
    font_height = get_res_value_keep(opts,"FontHeightF",0.01)

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
    font_height = get_res_value_keep(opts,"FontHeightF",0.01)

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
