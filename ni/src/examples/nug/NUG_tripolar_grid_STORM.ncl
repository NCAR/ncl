;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_tripolar_grid_STORM.ncl
; 
;  INFO: The STORM model grid is MPIOM TP6ML80 (3602x2394).
;        Only small regions could be plotted with cell outlines.
;
;  KMF 30.10.14
;-----------------------------------------------------------------
dd = systemfunc("date")
print("--> This plot will take some time! " + dd)

; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

   diri = "./"
   fili = "tripolar_grid_STORM.nc"

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
  
   f              = addfile(diri+fili, "r")
   var            =  f->sst(0,0,:,:)
   var@lat2d      =  f->lat
   var@lon2d      =  f->lon

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","NUG_tripolar_grid_STORM")

   plot = new(2,graphic)

;-- set resources
   res                       =  True
   res@gsnDraw               =  False
   res@gsnFrame              =  False
   res@gsnMaximize           =  True

   res@mpProjection          = "CylindricalEquidistant"   ;-- choose projection
   res@mpDataBaseVersion     = "MediumRes"
   res@mpPerimOn             =  False           ;-- turn off box around plot
   res@mpFillOn              =  False
   res@mpMinLonF             =   2.0
   res@mpMaxLonF             =  25.0
   res@mpMinLatF             =  52.0
   res@mpMaxLatF             =  65.0

   res@cnFillOn              =  True            ;-- turn on contour fill
   res@cnFillPalette         = "ncl_default"    ;-- choose color map
   res@cnFillMode            = "CellFill"
   res@cnLinesOn             =  False           ;-- Turn lines off
   res@cnLineLabelsOn        =  False           ;-- Turn labels off
;   res@cnCellFillEdgeColor   =  "black"
;   res@cnCellFillMissingValEdgeColor = "grey"
   res@cnCellFillEdgeColor   =  -1
   res@cnCellFillMissingValEdgeColor = -1

   res@tiMainString          = "NCL Doc Example: Tripolar grid STORM"   ;-- title string
   res@tiMainFontHeightF     =  0.02

;-- plot sub-region
   plot(0) = gsn_csm_contour_map(wks,var,res)

   dd = systemfunc("date")
   print("-- 1. plot done: " + dd)

;-- plot all -180-180 deg.
   res@mpMinLonF             = -180.0
   res@mpMaxLonF             =  180.
   res@mpMinLatF             =  -90.
   res@mpMaxLatF             =   90.

   delete(res@cnCellFillEdgeColor)
   delete(res@cnCellFillMissingValEdgeColor)
   delete(res@tiMainString)

   res@cnCellFillEdgeColor   =  -1
   res@cnCellFillMissingValEdgeColor = -1

   plot(1) = gsn_csm_contour_map(wks,var,res)

;-- create panel plot
   gsn_panel(wks,plot,(/2,1/),False)

   dd = systemfunc("date")
   print("-- 2. plot done: " + dd)

end
