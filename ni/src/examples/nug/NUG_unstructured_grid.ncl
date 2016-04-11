;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_unstructured_grid.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

   diri  = "$NCARG_ROOT/lib/ncarg/data/nug/"
   fili  = "camse_unstructured_grid.nc"

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
  
   f     =  addfile(diri+fili, "r")
   var   =  f->T850
   lat1d =  f->lat
   lon1d =  f->lon

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","plot_unstructured_grid_camse")

;-- set resources
   res                       =  True
   res@gsnMaximize           =  True

   res@cnFillOn              =  True                   ;-- turn on contour fill
   res@cnFillPalette         = "BlueWhiteOrangeRed"    ;-- choose color map 

   res@tiMainString          = "NCL Doc Example: Unstructured grid (CAM-SE)"  ;-- title string
   res@tiMainFontHeightF     =  0.02

;---Lat/lon arrays of curvilinear grid for overlaying on map
   res@sfXArray              =  lon1d
   res@sfYArray              =  lat1d

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
