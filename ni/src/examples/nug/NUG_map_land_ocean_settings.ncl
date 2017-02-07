;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_map_land_ocean_settings.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 

begin
;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","NUG_map_land_ocean_settings")

;-- set resources
  res                      =  True
  res@mpFillOn             =  True       ;-- use land fill (default: True)
  res@mpOutlineOn          =  True       ;-- outline land  (default: False)
  
  res@mpOceanFillColor     = "lightblue" ;-- color to fill ocean
  res@mpInlandWaterFillColor = "lightblue"   ;-- color to fill inland water
  res@mpLandFillColor      = "navajowhite1"  ;-- color to fill land

  res@mpGeophysicalLineColor = "blue"        ;-- outline color
  res@mpGeophysicalLineThicknessF = 1.2      ;-- thickness of continental
                                             ;-- outlines
  res@mpDataBaseVersion    = "MediumRes"     ;-- map resolution

;-- draw the map
  map = gsn_csm_map(wks, res)

end
