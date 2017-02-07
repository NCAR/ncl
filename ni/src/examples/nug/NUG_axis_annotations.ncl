;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_axis_annotations.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var

   diri  = "$NCARG_ROOT/lib/ncarg/data/nug/"
   fili  = "rectilinear_grid_3D.nc"
   
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
  
   var   = f->t(0,{70000},{55},{0:60})
   lon_t = f->lon({0:60})                     ;-- longitude=0-60E

;---- define the workstation (plot output type and name)
   wks = gsn_open_wks("png","NUG_axis_annotations")

;---- set resources
   res                        =  True
   res@gsnMaximize            =  True
 
;-- set the title string. ~C~ insert a carriage return (no \ allowed).
   res@tiMainString           = "NCL Doc Example:  Axis Annotations"
   res@tiMainFontHeightF      =  0.02

   res@tiXAxisSide            = "Bottom"         ;-- X-Axis title on bottom
   res@tiXAxisFontHeightF     =  0.015           ;-- X-Axis title font size
   res@tiYAxisFontHeightF     =  0.015           ;-- Y-Axis title font size

   res@tiXAxisString          =  lon_t@long_name
   res@tiYAxisString          =  var@long_name + "  [" + var@units + "]"

   res@tmLabelAutoStride      =  True
   res@tmXBTickSpacingF       =  5               ;-- label X-Axis every 10 deg

   res@xyLineThicknessF       =  2.0

   plot = gsn_csm_xy(wks,lon_t,var,res)

end
