;-------------------------------------------------------------------
;  NCL User Guide Example:  NCL_ICON_triangles_colored.ncl  
;
;  Read ICON data and plot variable clt (data from Renate Brokopf).
;  Input data file does not contain the grid information, a second
;  file r2b4_amip.nc with the grid information must be opened.
;  Grid files:  /pool/data/ICON/grids/grids/private/r2b4_amip
;
;  Script based on the NCL example icon_5.ncl from
;           http://ncl.ucar.edu/Applications/Scripts/icon_5.ncl
;
;  modified:
;     - plot only one frame
;     - don't wrap lines around for 'low' and 'high' levels as well
;          (original script regards only to 'middle' levels which  
;           caused polygon line wrapping)
;
;  KMF 30.10.14
;-------------------------------------------------------------------
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;---------------------------------------
;-- function attach_labelbar
;---------------------------------------
undef("attach_labelbar")
function attach_labelbar(wks,map,labels,colors)
local lbres, nlevels, amres
begin
  nlevels = dimsizes(labels)                        ; number of labels

  lbres                      =  True
  lbres@lbPerimOn            =  False               ; no label bar box
  lbres@lbOrientation        = "Horizontal"         ; orientation
  lbres@vpWidthF             =  0.7                 ; width of labelbar
  lbres@vpHeightF            =  0.10                ; height of labelbar
  lbres@lbLabelFontHeightF   =  0.015               ; label font height
  lbres@lbLabelAlignment     = "InteriorEdges"      ; where to label
  lbres@lbMonoFillPattern    =  True                ; fill sold
  lbres@lbFillColors         =  colors              ; use colors
  lbid = gsn_create_labelbar (wks,nlevels+1,labels,lbres)

  amres                      =  True
  amres@amJust               = "TopCenter"          ; annotation alignment
  amres@amOrthogonalPosF     =  0.6                 ; move annotation downward
  amres@gsnMaximize          =  True                ; be sure to remaxmize output
  map@annoid                 =  gsn_add_annotation(map,lbid,amres)

  return(map)
end

;==================================
;-- main
;==================================
begin
  DataFileName = "atm_phy_mag0004_1985.nc"           ;-- data file
  gridinfofile = "r2b4_amip.nc"                      ;-- grid info file
  VarName      = "clt"                               ;-- variable name       

  colormap     = "WhiteBlue"                         ;-- color map
  scale        =  100                                ;-- multiply by

  varMin       =   10.                               ;-- minimum contour level
  varMax       =   90.                               ;-- maximum contour level
  varInt       =   10.                               ;-- interval between contours

  lonMin       = -180                                ;-- longitude minimum
  lonMax       =  180                                ;-- longitude maximum
  latMin       =  -90                                ;-- latitude minimum
  latMax       =   90                                ;-- latitude maximum

;-------------------------------------------------------------------
;-- add data and grid file
;-------------------------------------------------------------------
  File         = addfile( DataFileName, "r" )        ;-- add data file
  GridInfoFile = addfile( gridinfofile, "r" )        ;-- add grid file (not contained in data file!!!)
  var          = File->$VarName$(time|0,ncells|:)    ;-- set variable with dims: (time,ncell)
  var          = var*scale                           ;-- scale factor (here 100 to get %)

;-------------------------------------------------------------------
;-- define the x-, y-values and the polygon points
;-------------------------------------------------------------------
  rad2deg =  45./atan(1.)                            ;-- radians to degrees
 
  x    = GridInfoFile->clon *rad2deg                 ;-- cell center, lon
  y    = GridInfoFile->clat *rad2deg                 ;-- cell center, lat

  vlon = GridInfoFile->clon_vertices * rad2deg       ;-- cell longitude vertices
  vlon = where(vlon.lt.0, vlon + 360, vlon)          ;-- longitude: 0-360
  vlat = GridInfoFile->clat_vertices * rad2deg       ;-- cell latitude vertices

  nv   = dimsizes(vlon(0,:))                         ;-- number of points in polygon

  print("")
  print("Data longitude min/max: " + min(vlon) + "   " + max(vlon) + "   cell points = " + nv)
  print("")
  print("Plot area: "+lonMin+","+lonMax+" "+latMin+","+latMax)
  print("")

;-------------------------------------------------------------------
;-- open workstation
;-------------------------------------------------------------------
  wks = gsn_open_wks("png","plot_ICON_triangles_col")
  gsn_define_colormap(wks,colormap)

;-------------------------------------------------------------------
;-- set map resources
;-------------------------------------------------------------------
  mpres                      =  True
  mpres@gsnDraw              =  False                 ;-- don't draw the plot
  mpres@gsnFrame             =  False                 ;-- don't advance the frame
  mpres@gsnMaximize          =  True                  ;-- maximize plot output
  mpres@mpLimitMode          = "LatLon"               ;-- map limits mode
  mpres@mpMinLatF            =  latMin                ;-- latitude minimum
  mpres@mpMaxLatF            =  latMax                ;-- latitude maximum
  mpres@mpMinLonF            =  lonMin                ;-- longitude minimum
  mpres@mpMaxLonF            =  lonMax                ;-- longitude maximum
  mpres@mpCenterLonF         =  180                   ;-- map center lon
  mpres@mpFillOn             =  False                 ;-- don't draw filled map

;-------------------------------------------------------------------
;-- set contour resources
;-------------------------------------------------------------------
  cnres                      =  mpres                 ;-- based on mpres
  cnres@gsnDraw              =  False                 ;-- don't draw the plot
  cnres@gsnFrame             =  False                 ;-- don't advance the frame

  cnres@sfXArray             =  x                     ;-- transform x to mesh scalar field
  cnres@sfYArray             =  y                     ;-- transform y to mesh scalar field

  cnres@cnLinesOn            =  False                 ;-- don't draw contour lines
  cnres@cnInfoLabelOn        =  False                 ;-- switch off contour info label
  cnres@cnFillOn             =  True                  ;-- contour fill on
  cnres@cnFillMode           = "RasterFill"           ;-- set fill mode
  cnres@cnRasterSmoothingOn  =  True                  ;-- smooth contouring
  cnres@cnLevelSelectionMode = "ManualLevels"         ;-- set manual contouring levels on
  cnres@cnLevelSpacingF      =  varInt                ;-- contouring interval
  cnres@cnMinLevelValF       =  varMin                ;-- contouring minimum
  cnres@cnMaxLevelValF       =  varMax                ;-- contouring maximum
  cnres@gsnSpreadColors      =  True                  ;-- spread color across whole color map
  cnres@gsnSpreadColorStart  =  15                    ;-- start color index
  cnres@gsnSpreadColorEnd    =  220                   ;-- end color index

  ;-- create the contour plot, but don't draw it
  plot = gsn_csm_contour_map(wks,var,cnres)

;-------------------------------------------------------------------
;-- to plot the filled polygons we need the data levels and their 
;-- colors
;-------------------------------------------------------------------
  getvalues plot@contour
     "cnLevels"     :  levels                         ;-- retrieve the levels from plot@contour
     "cnFillColors" :  colors                         ;-- retrieve the colors from plot@contour
  end getvalues
  nlevels = dimsizes(levels)                          ;-- number of levels

;-------------------------------------------------------------------
;-- create the map, but don't draw it yet
;-------------------------------------------------------------------
  mpres@gsnLeftString        =  var@long_name         ;-- set left string
  mpres@gsnRightString       =  var@units             ;-- set right string

  ;-- create the map, but don't draw it
  map = gsn_csm_map(wks,mpres)

  ;-- attach a labelbar to the plot
  map = attach_labelbar(wks,map,levels+"",colors)

;-------------------------------------------------------------------
;-- set polygon resources
;-------------------------------------------------------------------
  pres                       =  True
  pres@gsEdgesOn             =  True                 ;-- draw edges
  pres@gsFillIndex           =  0                    ;-- solid fill

;-------------------------------------------------------------------
;-- start drawing the triangles
;-------------------------------------------------------------------
  eps = 300.

;-------------------------------------------------------------------
;-- first attach the polygons associated with the lowest level; don't wrap lines around
;-------------------------------------------------------------------
  vlow = ind(var .lt. levels(0))
  do i = 0, dimsizes(vlow)-1
     ;=== This is a special test to make sure we don't get a polygon that wraps around.
     f = fabs(vlon(vlow(i),1:nv-1) - vlon(vlow(i),0:nv-2))
     if (any(f.gt.eps)) then
        ;=== Fix vlon(vlow(i),:) so it doesn't wrap around
        vlon(vlow(i),:) = where(vlon(vlow(i),:).gt.180, vlon(vlow(i),:)-360, vlon(vlow(i),:))
     end if
     pres@gsFillColor = colors(0)
     varstring = unique_string("gon")
     plot@$varstring$ = gsn_add_polygon(wks,map,vlon(vlow(i),:), vlat(vlow(i),:),pres)
  end do
  print ("finished level  0  -- " + sprinti("%6.0i",dimsizes(vlow)) + " polygons considered")
;-------------------------------------------------------------------
;-- attach the polygons associated with the middle levels; don't wrap lines around
;-------------------------------------------------------------------
  do i = 0, dimsizes(levels) -2
     vind = ind(var .ge. levels(i) .and. var .lt. levels(i+1))
     do j = 0, dimsizes(vind)-1
        ;=== This is a special test to make sure we don't get a polygon that wraps around.
        f = fabs(vlon(vind(j),1:nv-1) - vlon(vind(j),0:nv-2))
        if (any(f.gt.eps)) then
           ;=== Fix vlon(vind(j),:) so it doesn't wrap around
           vlon(vind(j),:) = where(vlon(vind(j),:).gt.180, vlon(vind(j),:)-360, vlon(vind(j),:))
        end if
        pres@gsFillColor = colors(i+1)
        varstring = unique_string("gon")
        map@$varstring$ = gsn_add_polygon(wks,map,vlon(vind(j),:), vlat(vind(j),:),pres)
     end do
     print ("finished level " + sprinti("%2.0i",(i+1)) + "  -- " + sprinti("%6.0i",dimsizes(vind)) + \
            " polygons considered")
     delete(vind)
  end do
;-------------------------------------------------------------------
;-- attach the polygons associated with the highest level; don't wrap lines around
;-------------------------------------------------------------------
  vhgh = ind(var .ge. levels(nlevels-1))
  do i = 0, dimsizes(vhgh)-1
     ;=== This is a special test to make sure we don't get a polygon that wraps around.
     f = fabs(vlon(vhgh(i),1:nv-1) - vlon(vhgh(i),0:nv-2))
     if (any(f.gt.eps)) then
        ;=== Fix vlon(vhgh(i),:) so it doesn't wrap around
        vlon(vhgh(i),:) = where(vlon(vhgh(i),:).gt.180, vlon(vhgh(i),:)-360, vlon(vhgh(i),:))
     end if
     pres@gsFillColor = colors(nlevels)
     varstring = unique_string("gon")
     map@$varstring$ = gsn_add_polygon(wks,map,vlon(vhgh(i),:), vlat(vhgh(i),:),pres)
  end do
  print ("finished level " + sprinti("%2.0i",nlevels) + "  -- " + sprinti("%6.0i",dimsizes(vhgh)) + \
         " polygons considered")
  delete(vhgh)

;-------------------------------------------------------------------
  draw(map)             ;-- draw the map
  frame(wks)            ;-- advance the frame
;-------------------------------------------------------------------
end


