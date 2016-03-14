;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_plot_in_5_steps.ncl
;
;  KMF 30.10.2014
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   diri = "./"
   fili = "rectilinear_grid_2D.nc"
   
   f    = addfile(diri+fili, "r")

   wks  = gsn_open_wks("png","plot_in_5_steps")

   res              =  True
   res@tiMainString = "NCL plot in 5 steps"

   plot = gsn_csm_contour_map(wks, f->tsurf(0,:,:), res)
end
