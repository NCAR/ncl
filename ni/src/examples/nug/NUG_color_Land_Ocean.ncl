;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_color_Land_Ocean.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data
   diri = "./"
   fili = "CNTASN_1m_200103_grid_T_curvilinear_grid.nc"
   
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
     
   var       =  f->votemper(0,0,:,:)
   var@lat2d =  f->nav_lat
   var@lon2d =  f->nav_lon
  
;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","NUG_color_Land_Ocean")

;-- set resources
   res                       =  True
   res@gsnAddCyclic          =  False           ;-- data are not global, don't add lon cyclic point
   res@gsnMaximize           =  True
   res@cnFillOn              =  True            ;-- turn on contour fill
   res@cnFillPalette         = "rainbow"
   res@tiMainString          = "NCL Doc Example: color land and ocean"  ;-- title string
   res@tiMainFontHeightF     =  0.02

;-- color land and ocean (looks nicer)
   res@mpOceanFillColor      = "lightblue"
   res@mpInlandWaterFillColor= "lightblue"
   res@mpLandFillColor       = "tan"

;-- zoom in on map
   res@mpMinLatF             =  min(var@lat2d)
   res@mpMaxLatF             =  max(var@lat2d)
   res@mpMinLonF             =  min(var@lon2d)
   res@mpMaxLonF             =  max(var@lon2d)

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
