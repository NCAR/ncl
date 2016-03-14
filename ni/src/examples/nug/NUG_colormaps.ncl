;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_colormaps.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

   diri  = "./"
   fili  = "rectilinear_grid_2D.nc"
   
   file1 =  addfile(diri+fili, "r")
   var   =  file1->tsurf(0,:,:)

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","plot_colormaps")

;-- set resources
   res                       =  True
   res@gsnMaximize           =  True

   res@cnFillOn              =  True            ;-- turn on contour fill

   res@tiMainString          = "NCL UG Example: Color maps"     ;-- title string
   res@tiMainFontHeightF     =  0.02

;-- 1: set color mapto "ncl_default" and draw the contour map
   gsn_define_colormap(wks,"ncl_default")
   plot = gsn_csm_contour_map(wks, var, res)

;-- 2: change the color map to "rainbow" and draw the contour map
   gsn_define_colormap(wks,"rainbow")
   plot = gsn_csm_contour_map(wks, var, res)

;-- 3: change the color map to "BlueRed" and draw the contour map
   gsn_define_colormap(wks,"BlueRed")
   plot = gsn_csm_contour_map(wks, var, res)

;-- 4: color map set by resource cnFillPalette instead of gsn_define_colormap
   res@cnFillPalette         = "OceanLakeLandSnow"
   plot = gsn_csm_contour_map(wks, var, res)

;-- 5: draw just the color map - no data. !! Uses the last gsn_define_colormap setting to wks
   gsn_define_colormap(wks,"BlGrYeOrReVi200")
   gsn_draw_colormap(wks)

;-- 6: draw the given color map. Doesn't require calling gsn_define_colormap. 
   draw_color_palette(wks,"BlueYellowRed",False)

end
