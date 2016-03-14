;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_xy_plot.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   x = (/1,2,3,4,5,6,7,8,9,10/)
   y = (/3,7,4,9,2,1,8,8,4,2/)

   wks = gsn_open_wks("png","xy_plot")

   res                        =  True
   res@tiMainString           = "NCL UG Example: xy-plot"

   plot = gsn_csm_xy(wks, x, y, res)

 end
