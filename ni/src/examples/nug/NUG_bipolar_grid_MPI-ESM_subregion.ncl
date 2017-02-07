;---------------------------------------------------
;  NCL User Guide Example:   NUG_bipolar_grid_MPI-ESM_subregion.ncl
;
;  KMF 01.11.14
;---------------------------------------------------
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
     
   tos         =  f->tos
   tos@lat2d   =  f->lat
   tos@lon2d   =  f->lon
   var         =  tos(0,:,:)                    ;-- select first time step

;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","NUG_bipolar_grid_MPI-ESM_subregion")

;-- set resources
   res                       =  True
   res@gsnMaximize           =  True            ;-- maximize plot output
   res@gsnAddCyclic          =  True            ;-- add cyclic point
   res@cnFillOn              =  True            ;-- turn on contour fill
   res@cnFillMode            = "CellFill"       ;-- fill mode
   res@cnFillPalette         = "ncl_default"    ;-- choose a color map
   res@cnLinesOn             =  False           ;-- turn lines off
   res@cnLineLabelsOn        =  False           ;-- turn labels off
   res@cnCellFillEdgeColor   = 1
   res@cnCellFillMissingValEdgeColor = "black"  ;-- _FillValue color
   res@mpLimitMode           = "Corners"
   res@mpLeftCornerLonF      = -95.             ;-- min longitude
   res@mpRightCornerLonF     =  35.             ;-- max longitude
   res@mpLeftCornerLatF      =  20.             ;-- min latitude
   res@mpRightCornerLatF     =  90.             ;-- max latitude
   res@mpDataBaseVersion     = "MediumRes"      ;-- map data base
   res@mpFillOn              =  False           ;-- turn off map fill

   res@tiMainString          = "NCL Doc Example: Bipolar grid MPI-ESM subregion"
   res@tiMainFontHeightF     =  0.02

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)

end
