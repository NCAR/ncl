;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_legends.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data to be plotted
   diri  = "./"
   fili  = "rectilinear_grid_3D.nc"
   f     =  addfile(diri+fili, "r")
   temp  =  f->t(0,:,{55},{0:60})

;-- set the desired levels to extract
   levels = (/100000,85000,70000,50000/)

;-- define the colors of the lines and their labels
   colors = (/"red", "green", "blue", "orange"/)
   labels = " " + temp&lev@long_name + sprinti("%9i",levels) + \
            " [" + temp&lev@units + "]"

;---- define the workstation (plot output type and name)
   wks = gsn_open_wks("png","plot_legends")

;---- set resources
   res                        =  True
   res@gsnMaximize            =  True

   res@trYMinF                =  230
   res@trYMaxF                =  max(temp)
 
;-- set the title string
   res@tiMainString           = "NCL UG Example:  Legends"
   res@tiMainFontHeightF      =  0.02
   res@tiXAxisString          =  temp&lon@long_name 
   res@tiYAxisString          =  temp@long_name
   res@tiXAxisSide            = "Bottom"         ;-- X-Axis title on bottom
   res@tiXAxisFontHeightF     =  0.015           ;-- X-Axis title font size
   res@tiYAxisFontHeightF     =  0.015           ;-- Y-Axis title font size

   res@tmXBTickSpacingF       =  10              ;-- label X-Axis every 10 deg
   res@xyLineColors           =  colors
   res@xyExplicitLabels       =  labels
   res@xyDashPatterns         =  (/0,0,0,0/)     ;-- all dash pattern are solid
   res@xyLineThicknessF       =  2.0

   res@lgJustification        = "TopRight"
   res@lgLabelJust            = "CenterRight"
   res@lgLabelFontHeightF     =  0.01
   res@lgBoxMinorExtentF      =  0.16             ;-- make the legend lines shorter
   res@pmLegendDisplayMode    = "Always"
   res@pmLegendWidthF         =  0.15             ;-- set legend width
   res@pmLegendHeightF        =  0.1              ;-- set legend height
   res@pmLegendOrthogonalPosF = -0.34             ;-- move legend up
   res@pmLegendParallelPosF   =  0.97             ;-- move legend right

   plot = gsn_csm_xy(wks,temp&lon,temp({levels},:),res)

end
