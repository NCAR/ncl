;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_curvilinear_basic.ncl
; 
;  KMF 
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
   fili = "tos_ocean_bipolar_grid.nc"
   
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

   var         =  f->tos(0,:,:)
   var@lat2d   =  f->lat
   var@lon2d   =  f->lon

   wks = gsn_open_wks("png","NUG_curvilinear_basic")  ;-- open a workstation

   res               =  True                  ;-- set resources
   res@gsnAddCyclic  =  False                 ;-- lon < 360 degrees
   res@cnFillOn      =  True                  ;-- turn on contour fill
   res@cnFillPalette = "BlueWhiteOrangeRed"   ;-- change color map
   res@tiMainString  = "Curvilinear grid:  MPI-ESM  (2D lat/lon arrays)"

   plot = gsn_csm_contour_map(wks,var,res)           ;-- create the plot

end
