;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_unstructured_grid_ICON.ncl
; 
;  Data:             ICON - unstructured
;
;  2015-02-24 kmf
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define
  rad2deg =  45./atan(1.)                              ;-- radians to degrees

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
  
;-- read data
  f       =  addfile(diri+fili,"r")
  var     =  f->S(time|0,depth|0,ncells|:)  ;-- set variable with dims: (time,ncell)
;  printVarSummary(var)

  x       =  f->clon * rad2deg              ;-- cell center, lon
  y       =  f->clat * rad2deg              ;-- cell center, lat
  x!0     = "lon"                           ;-- set named dimension lon
  y!0     = "lat"                           ;-- set named dimension lat
  x@units = "degrees_east"                  ;-- set lon units
  y@units = "degrees_north"                 ;-- set lat units

  vlon    =  f->clon_vertices * rad2deg     ;-- cell longitude vertices
  vlon    =  where(vlon.lt.0, vlon + 360, vlon) ;-- longitude: 0-360
  vlat    =  f->clat_vertices * rad2deg     ;-- cell lattitude vertices
  nv      =  dimsizes(vlon(0,:))            ;-- number of points in polygon

;-- open a PNG file
  wks = gsn_open_wks("png","NUG_unstructured_grid_ICON")

;-- set resources for contour plots
  res                    =  True
  res@cnFillOn           =  True
  res@cnFillMode         = "CellFill"               ;-- set fill mode

  res@sfXArray           =  x                       ;-- transform x to mesh scalar field
  res@sfYArray           =  y                       ;-- transform y to mesh scalar field
  res@sfXCellBounds      =  vlon                    ;-- needed if set cnFillMode = "CellFill"
  res@sfYCellBounds      =  vlat                    ;-- needed if set cnFillMode = "CellFill"

;-- create the plot
  plot = gsn_csm_contour_map(wks, var, res)

end
