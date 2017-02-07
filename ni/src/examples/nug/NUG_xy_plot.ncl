;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_xy_plot.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   x = (/1,2,3,4,5,6,7,8,9,10/)
   y = (/3,7,4,9,2,1,8,8,4,2/)

   wks = gsn_open_wks("png","NUG_xy_plot")

   res                        =  True
   res@tiMainString           = "NCL Doc Example: xy-plot"

   plot = gsn_csm_xy(wks, x, y, res)

 end
