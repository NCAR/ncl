;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_xy_plot_timeseries.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These three load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"

begin
   diri = "./"
   fili = "rectilinear_grid_2D.nc"

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
  
   f    =  addfile(diri+fili, "r")
   var  = f->tsurf
   time = var&time

;-- compute the area mean without weighting
   fldmean = wgt_areaave_Wrap(var,1.0,1.0,1)

   wks = gsn_open_wks("png","NUG_xy_plot_timeseries")

;-- set resources
   res                      =  True
   res@tiMainString         = "NCL Doc Example: xy-plot timeseries"

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
