;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_xy_plot_timeseries.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"

begin
   diri = "./"
   fili = "rectilinear_grid_2D.nc"
   
   f    =  addfile(diri+fili, "r")
   var  = f->tsurf
   time = var&time

;-- compute the area mean without weighting
   fldmean = wgt_areaave_Wrap(var,1.0,1.0,1)

   wks = gsn_open_wks("png","xy_plot_timeseries")

;-- set resources
   res                      =  True
   res@tiMainString         = "NCL UG Example: xy-plot timeseries"

   restime                  = True       ;-- set time tickmark resources
   restime@ttmFormat        = "%d %c %y" ;-- time tickmark format

   time_axis_labels(time,res,restime)    ;-- sets the correct time labels

   res@tmXBLabelFontHeightF =  0.01
   res@tmXBLabelJust        = "CenterRight"
   res@tmXBLabelDeltaF      =  1.0
   res@tmXBLabelAngleF      =  50.
   res@tmLabelAutoStride    =  True

   plot = gsn_csm_xy(wks, time, fldmean, res)

end
