;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_plot_in_5_steps.ncl
;
;  KMF 30.10.2014
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   diri = "./"
   fili = "rectilinear_grid_2D.nc"
   
   if (fileexists(diri+fili)) then
      f = addfile(diri+fili, "r")
   else
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
  
   wks  = gsn_open_wks("png","NUG_plot_in_5_steps")

   res              =  True
   res@tiMainString = "NCL plot in 5 steps"

   plot = gsn_csm_contour_map(wks, f->tsurf(0,:,:), res)
end
