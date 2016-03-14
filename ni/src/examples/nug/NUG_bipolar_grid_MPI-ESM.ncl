;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_bipolar_grid_MPI-ESM.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

   diri = "./"
   fili = "tos_ocean_bipolar_grid.nc"
   
   f           =  addfile(diri+fili, "r")
   tos         =  f->tos
   tos@lat2d   =  f->lat
   tos@lon2d   =  f->lon
   var         =  tos(0,:,:)

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","plot_bipolar_grid_MPI-ESM")

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

   res@tiMainString          = "NCL UG Example: Bipolar grid MPI-ESM"
                                                ;-- title string
   res@tiMainFontHeightF     =  0.02

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
