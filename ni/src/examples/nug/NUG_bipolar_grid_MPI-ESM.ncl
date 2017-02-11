;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_bipolar_grid_MPI-ESM.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

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
  
   tos         =  f->tos
   tos@lat2d   =  f->lat
   tos@lon2d   =  f->lon
   var         =  tos(0,:,:)

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","NUG_bipolar_grid_MPI-ESM")

;-- set resources
   res                       =  True
   res@gsnMaximize           =  True
   res@gsnAddCyclic          =  True

   res@cnFillOn              =  True            ;-- turn on contour fill
   res@cnFillPalette         = "ncl_default"    ;-- Choose color map
   res@cnFillMode            = "CellFill"
   res@cnLinesOn             =  False           ;-- Turn lines off
   res@cnLineLabelsOn        =  False           ;-- Turn labels off

   res@cnCellFillEdgeColor   = 1
   res@cnCellFillMissingValEdgeColor = "black"

   res@tiMainString          = "NCL Doc Example: Bipolar grid MPI-ESM"
                                                ;-- title string
   res@tiMainFontHeightF     =  0.02

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
