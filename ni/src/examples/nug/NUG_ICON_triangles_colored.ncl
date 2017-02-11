;-------------------------------------------------------------------
;  NCL User Guide Example:  NUG_ICON_triangles_colored.ncl  
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
;          (original script checks only 'middle' levels which  
;           caused polygon line wrapping)
;
;  KMF 30.10.14
;-------------------------------------------------------------------
; Not required in NCL versions 6.2.0 and later.
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;-------------------------------------------
;-- Check for existence of file and open it
;-------------------------------------------
undef("open_file")
function open_file(dir[1]:string,name[1]:string)
begin
   if (fileexists(dir+name)) then
      f = addfile(dir+name, "r")
      return(f)
   end if
   print("")
   print("You don't have the necessary data for this script. You can download it from:​")
   print("")
   print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   print("or use the wget command:")
   print("")
   print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   exit
end

;---------------------------------------
;-- function create_map
;---------------------------------------
undef("create_map")
function create_map(wks,var,latmin,latmax,lonmin,lonmax)
local mpres
begin
;-------------------------------------------------------------------
;-- set map resources
;-------------------------------------------------------------------
  mpres                      =  True
  mpres@gsnDraw              =  False                 ;-- don't draw the plot
  mpres@gsnFrame             =  False                 ;-- don't advance the frame
  mpres@gsnMaximize          =  True                  ;-- maximize plot output
  mpres@mpLimitMode          = "LatLon"               ;-- map limits mode
  mpres@mpMinLatF            =  latmin                ;-- latitude minimum
  mpres@mpMaxLatF            =  latmax                ;-- latitude maximum
  mpres@mpMinLonF            =  lonmin                ;-- longitude minimum
  mpres@mpMaxLonF            =  lonmax                ;-- longitude maximum
  mpres@mpCenterLonF         =  180                   ;-- map center lon
  mpres@mpFillOn             =  False                 ;-- don't draw filled map

  mpres@gsnLeftString        =  var@long_name         ;-- set left string
  mpres@gsnRightString       =  "%"                   ;-- set right string (var@units)

  ;-- create the map, but don't draw it; will use this to add filled polygons to
  map = gsn_csm_map(wks,mpres)

  return(map)
end

;---------------------------------------
;-- function attach_labelbar
;---------------------------------------
undef("attach_labelbar")
procedure attach_labelbar(wks,map,labels,colors)
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
  lbres@lbMonoFillPattern    =  True                ; fill solid
  lbres@lbFillColors         =  colors              ; use colors
  lbid = gsn_create_labelbar (wks,nlevels+1,labels,lbres)

  amres                      =  True
  amres@amJust               = "TopCenter"          ; annotation alignment
  amres@amOrthogonalPosF     =  0.6                 ; move annotation downward
  map@annoid                 =  gsn_add_annotation(map,lbid,amres)

  return(map)
end

;-------------------------------------------------------------------
; Given a map, triangular mesh, levels, and colors, add filled 
; triangles to map depending on what levels they fall between. 
;-------------------------------------------------------------------
undef("add_filled_triangles")
procedure add_filled_triangles(wks,plot,var[*]:numeric,vlat[*][*],\
                               vlon[*][*],levels[*]:numeric,\
                               colors[*][*]:numeric)
local pres, eps, vlow, vhgh, vind, varstring, i, j, nlevels, nv
begin
  nv = dimsizes(vlon(0,:))                         ;-- number of vertices in polygon

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
     pres@gsFillColor = colors(0,:)
     varstring = unique_string("gon")
     plot@$varstring$ = gsn_add_polygon(wks,plot,vlon(vlow(i),:), vlat(vlow(i),:),pres)
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
        pres@gsFillColor = colors(i+1,:)
        varstring = unique_string("gon")
        plot@$varstring$ = gsn_add_polygon(wks,plot,vlon(vind(j),:), vlat(vind(j),:),pres)
     end do
     print ("finished level " + sprinti("%2.0i",(i+1)) + "  -- " + sprinti("%6.0i",dimsizes(vind)) + \
            " polygons considered")
     delete(vind)
  end do
;-------------------------------------------------------------------
;-- attach the polygons associated with the highest level; don't wrap lines around
;-------------------------------------------------------------------
  nlevels = dimsizes(levels)                          ;-- number of levels
  vhgh = ind(var .ge. levels(nlevels-1))
  do i = 0, dimsizes(vhgh)-1
     ;=== This is a special test to make sure we don't get a polygon that wraps around.
     f = fabs(vlon(vhgh(i),1:nv-1) - vlon(vhgh(i),0:nv-2))
     if (any(f.gt.eps)) then
        ;=== Fix vlon(vhgh(i),:) so it doesn't wrap around
        vlon(vhgh(i),:) = where(vlon(vhgh(i),:).gt.180, vlon(vhgh(i),:)-360, vlon(vhgh(i),:))
     end if
     pres@gsFillColor = colors(nlevels,:)
     varstring = unique_string("gon")
     plot@$varstring$ = gsn_add_polygon(wks,plot,vlon(vhgh(i),:), vlat(vhgh(i),:),pres)
  end do
  print ("finished level " + sprinti("%2.0i",nlevels) + "  -- " + sprinti("%6.0i",dimsizes(vhgh)) + \
         " polygons considered")
  delete(vhgh)

end

;==================================
;-- main
;==================================
begin
  vdir         = "$NCARG_ROOT/lib/ncarg/data/nug/"
  DataFile     = "atm_phy_mag0004_1985.nc"           ;-- data file
  gdir         = "./"
  GridInfoFile = "r2b4_amip.nc"                      ;-- grid info file
  VarName      = "clt"                               ;-- variable name       

  colormap     = "WhiteBlue"                         ;-- color map
  scale        =  100                                ;-- multiply by

  varMin       =   10                                ;-- minimum contour level
  varMax       =   90                                ;-- maximum contour level
  varInt       =   10                                ;-- interval between contours

  levels       = ispan(varMin,varMax,varInt)           ;-- contour levels
  nlevels      = dimsizes(levels)
  colors       = span_color_rgba (colormap,nlevels+1)  ;-- colors between levels

  lonMin       = -180                                ;-- longitude minimum
  lonMax       =  180                                ;-- longitude maximum
  latMin       =  -90                                ;-- latitude minimum
  latMax       =   90                                ;-- latitude maximum

;-------------------------------------------------------------------
;-- add data and grid file
;-------------------------------------------------------------------
  vfile = open_file(vdir,DataFile)           ;-- add data file
  gfile = open_file(gdir,GridInfoFile)       ;-- open lat/lon grid file (not contained in data file!!!)
  
  var = vfile->$VarName$(time|0,ncells|:)    ;-- set variable with dims: (time,ncell)
  var = var*scale                            ;-- scale factor (here 100 to get %)

;-------------------------------------------------------------------
;-- get the vertices of the polygons
;-------------------------------------------------------------------
  rad2deg = get_r2d("float")
 
  vlat = gfile->clat_vertices * rad2deg       ;-- cell latitude vertices
  vlon = gfile->clon_vertices * rad2deg       ;-- cell longitude vertices
  vlon = where(vlon.lt.0, vlon + 360, vlon)          ;-- longitude: 0-360
  nv   = dimsizes(vlon(0,:))                         ;-- number of points in polygon

  print("")
  print("Data longitude min/max: " + min(vlon) + "   " + max(vlon) + "   cell points = " + nv)
  print("")
  print("Plot area: "+lonMin+","+lonMax+" "+latMin+","+latMax)
  print("")

  ;-- open workstation
  wks = gsn_open_wks("png","NUG_ICON_triangles_colored")

  ;-- create a map
  map = create_map(wks,var,latMin,latMax,lonMin,lonMax)

  ;-- add filled triangles to the plot
  add_filled_triangles(wks,map,var,vlat,vlon,levels,colors)

  ;-- attach a labelbar to the plot
  attach_labelbar(wks,map,levels+"",colors)

  ;-- draw the map, filled triangles, labelbar
  draw(map)

  ;-- advance the frame
  frame(wks)
end


