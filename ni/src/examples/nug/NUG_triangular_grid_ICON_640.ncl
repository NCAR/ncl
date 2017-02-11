;---------------------------------------------------------------
;  NCL User Guide Example: NUG_triangular_grid_ICON.ncl
;
;  Grid type:         ICON - Unstructured grid
;
;  Settings:          sub-region,
;                     manual-levels,
;                     draw colored triangles with outlines,
;                     don't draw missing values
;  KMF 31.10.14
;---------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
  start_code_time = get_cpu_time()
  
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "triangular_grid_ICON.nc"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
  f    = addfile(diri+fili, "r")
  var  = f->S(0,2,:)

  var@_FillValue = getVarFillValue(var)   ;-- retrieve missing value of var

  var_min  =  34.5                        ;-- minimum contour value to be displayed
  var_max  =  36.1                        ;-- maximum contour value to be displayed
  var_inc  =   0.1                        ;-- increment
  
  lon_min  = -85.0                        ;-- minimum longitude
  lon_max  = -30.0                        ;-- maximum longitude
  lat_min  =  15.0                        ;-- minimum latitude
  lat_max  =  70.0                        ;-- maximum latitude

  rad2deg   = 45./atan(1.)                ;-- radians to degrees
  var@lon1d = f->clon * rad2deg                ;-- cell center, lon
  var@lat1d = f->clat * rad2deg                ;-- cell center, lat

;-- if variable wet_c exist: create missing values with it:
  if (isfilevar(f,"wet_c")) then
     wet = f->wet_c(2,:)                          ;--  time=0, depth=0; cells
     var = where ( wet .ge. 0.01, var, -999.)
     var@_FillValue = -999.                       ;--  missing value
  else
     print("WARNING: variable wet_c doesn't exist  -->  no masking")
  end if

;-- calculate the latitude and longitude values
  vlon = f->clon_vertices * rad2deg
  vlon = where(vlon.lt.0, vlon + 360, vlon)       ;-- lon: 0 - 360
  vlat = f->clat_vertices * rad2deg

  wks = gsn_open_wks("png", "NUG_triangular_grid_ICON_640")   ;-- open a workstation

  res                        =  True
  res@gsnDraw                =  False             ;-- don't draw the plot yet
  res@gsnFrame               =  False             ;-- don't advance the frame
  res@gsnLeftString          = ""                 ;-- don't add variable name to plot
  res@gsnRightString         = ""                 ;-- don't add units to plot
  res@gsnMaximize            =  True              ;-- maximize plot output
  
  res@tiMainString           = "NCL Doc Example: ICON  -  Salinity [psu]" ;-- title
  res@tiMainFontHeightF      = 0.020
  res@pmTitleZone            = 2                  ;-- move title closer to plot

  res@mpFillOn               =  True              ;-- fill map grey
  res@mpFillDrawOrder        = "PostDraw"         ;-- draw map outline at last
  res@mpDataBaseVersion      = "MediumRes"        ;-- map resolution
  res@mpMinLonF              =  lon_min           ;-- sub-region minimum longitude
  res@mpMaxLonF              =  lon_max           ;-- sub-region maximum longitude
  res@mpMinLatF              =  lat_min           ;-- sub-region minimum latitude
  res@mpMaxLatF              =  lat_max           ;-- sub-region maximum latitude
  res@mpGreatCircleLinesOn   =  True              ;-- important
  res@pmTickMarkDisplayMode  = "Always"           ;-- set nicer map tickmarks
  
  res@cnFillOn               =  True              ;-- contour fill
  res@cnFillPalette          = "BlueWhiteOrangeRed" ;-- choose color map
  res@cnLinesOn              =  False             ;-- don't draw contour lines
  res@cnFillMode             = "RasterFill"       ;-- contour fill mode
  res@cnLevelSelectionMode   = "ManualLevels"     ;-- set manual contour levels
  res@cnMinLevelValF         =  var_min           ;-- set min contour level
  res@cnMaxLevelValF         =  var_max           ;-- set max contour level
  res@cnLevelSpacingF        =  var_inc           ;-- set increment

  plot = gsn_csm_contour_map(wks,var,res)         ;-- create the plot, but don't draw it

;-- retrieve contour level and color informations
  getvalues plot@contour
     "cnLevels"     : levels                      ;-- # 26 (n)
     "cnFillColors" : colors                      ;-- # 27 (n+1)
  end getvalues

  plot = setColorContourClear(plot,min(var),max(var)) ;-- clear plot, but keep all the information

;-- create color array for triangles
  ntri     = dimsizes(var@lon1d)                  ;-- Number of triangles
  gscolors = new(ntri,integer)
  gscolors = -1                                   ;-- Initialize to transparent

;-- set resources for the triangles (polygons)
  pres                       =  True
  pres@gsEdgesOn             =  True              ;-- turn on edges
  pres@gsFillIndex           =  0                 ;-- solid fill

;-- set color for data less than given minimum value var_min
  vlow = ind(var .lt. levels(0))                  ;-- get the indices of values less levels(0)
  gscolors(vlow) = colors(0)                      ;-- choose color
  ntri_calc = dimsizes(vlow)                      ;-- number of triangles

;-- set colors for all cells in between var_min and var_max
  do i = 1, dimsizes(levels) - 1
     vind := ind(var .ge. levels(i-1) .and. var .lt. levels(i))  ;-- get the indices of 'middle' values
     gscolors(vind) = colors(i)                   ;-- choose the colors
     ntri_calc = ntri_calc + dimsizes(vind)       ;-- number of triangles
  end do

;-- set color for data greater than given maximum var_max
  nc=dimsizes(colors)-1                           ;-- get the number of colors minus one
  nl=dimsizes(levels)-1                           ;-- get the number of levels minues one
  vhgh := ind(var .gt. levels(nl))                ;-- get indices of values greater levels(nl)
  gscolors(vhgh) = colors(nc)                     ;-- choose color
  ntri_calc = ntri_calc + dimsizes(vhgh)          ;-- number of triangles

  print("--> triangles calculated:  "+ ntri_calc)

;-- Attach all the triangles using the list of colors
  pres@gsColors   = gscolors
  pres@gsSegments = ispan(0,dimsizes(var) * 3,3)  ;-- assign segments array
  polygon = gsn_add_polygon(wks,plot,ndtooned(vlon),ndtooned(vlat),pres)  ;-- draw all triangles

  draw(plot)           ;-- draw plot and attached filled triangles
  frame(wks)           ;-- advance the frame

  end_code_time = get_cpu_time()
  print("--> Elapsed time in CPU seconds: " + (end_code_time-start_code_time))
end
