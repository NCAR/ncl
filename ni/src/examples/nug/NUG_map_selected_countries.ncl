;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_map_selected_countries.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 

begin
  fill_colors = (/"red", "green", "white", "gray", "blue", \
                  "violet", "orange", "purple"/)           ;-- fill colors
  fill_areas = (/"Spain", "Italy", "Switzerland","Germany", "Latvia", \
                 "Ireland", "Norway", "Greece"/);-- countries to be colored
  outline_areas = (/"Belgium", "Croatia"/)      ;-- only outline countries
  outline_areas := array_append_record (outline_areas, fill_areas, 0) 
                                                ;-- concatenate arrays
  outline_colors = (/"red","green","white","gray","blue","hotpink",\
                     "orange","purple"/)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","NUG_map_selected_countries")

;-- set resources
  res                      =  True
  res@gsnMaximize          =  True
  
  res@mpOutlineOn          =  True       ;-- outline land  (default: False)
  res@mpOutlineSpecifiers  =  outline_areas  ;-- which country to be
                                             ;-- outlined
  res@mpFillOn             =  True       ;-- use land fill (default: True)
  res@mpFillAreaSpecifiers =  fill_areas  ;-- which country to be colored
  res@mpSpecifiedFillColors =  outline_colors ;-- set colors to be used
  
  res@mpOceanFillColor     = "lightblue"     ;-- color to fill ocean
  res@mpInlandWaterFillColor = "lightblue"   ;-- color to fill inland water
  res@mpLandFillColor      = "navajowhite1"  ;-- color to fill land

  res@mpGeophysicalLineColor = "blue"        ;-- outline color
  res@mpGeophysicalLineThicknessF = 1.2      ;-- thickness of continental
                                             ;-- outlines
  res@mpDataBaseVersion    = "MediumRes"     ;-- map resolution
  res@mpDataSetName        = "Earth..4"
  
  res@mpMinLatF            =  30.0           ;-- min latitude
  res@mpMaxLatF            =  80.0           ;-- max latitude
  res@mpMinLonF            = -15.0           ;-- min longitude
  res@mpMaxLonF            =  30.0           ;-- max longitude
  
;-- draw the map
  map = gsn_csm_map(wks, res)
  
end
