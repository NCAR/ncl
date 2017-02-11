;---------------------------------------------------
;  NCL User Guide Example:   NUG_labelbars.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

   diri  = "./"
   fili  = "T2M_ERAINT_rectilinear_grid_2D.nc"
   
;-- read the data
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
   
   var   =  f->T2M(0,:,:)

;-- define the workstation (graphic will be written to a file)
   wks      =  gsn_open_wks("png","NUG_labelbars")

;-- set plot resources
   res                  = True
   res@gsnDraw          = False
   res@gsnFrame         = False
   res@gsnMaximize      = True

   res@cnFillOn         = True 
   res@cnLinesOn        = False
   res@cnLineLabelsOn   = False
   res@cnInfoLabelOn    = False
   res@cnLevelSelectionMode = "ManualLevels"
   res@cnMinLevelValF   =  250.
   res@cnMaxLevelValF   =  310.
   res@cnLevelSpacingF  =    5.
   res@cnFillPalette    = "BlueWhiteOrangeRed"

   plot = new(6,graphic)

;-- upper left
   res@tiMainString       = "Labelbar: horizontal(default)"
   plot(0) = gsn_csm_contour_map(wks,var,res)

;-- upper right
   res@tiMainString       = "Labelbar: vertical"
   res@lbOrientation      = "vertical"
   plot(1) = gsn_csm_contour_map(wks,var,res)

;-- middle left
   res@tiMainString       = "Labelbar: exclude outer boxes"
   res@lbOrientation      = "horizontal"
   res@cnLabelBarEndStyle = "ExcludeOuterBoxes"
   plot(2) = gsn_csm_contour_map(wks,var,res)

;-- middle right
   res@tiMainString       = "Labelbar: rotate labels and set labelbar title"
   res@lbTitleOn          =  True
   res@lbTitleString      = "degK"
   res@lbTitlePosition    = "Right"
   res@lbTitleOffsetF     =  -0.03
   res@lbTitleFontHeightF =  0.015
   res@lbLabelFontHeightF =  0.015
   res@lbLabelAngleF      =  30
   res@pmLabelBarOrthogonalPosF = 0.10
   delete(res@cnLabelBarEndStyle)

   plot(3) = gsn_csm_contour_map(wks,var,res)

;-- lower left
   cmap   = read_colormap_file("BlueWhiteOrangeRed")
   cmap_r = cmap(::-1,:)

   res@tiMainString        = "Labelbar: reverse colors"
   res@cnFillPalette       := cmap_r
   delete(res@lbLabelAngleF)
   plot(4) = gsn_csm_contour_map(wks,var,res)

;-- lower right

   getvalues plot@contour
     "cnLevels" : levels
   end getvalues

   res@lbLabelStrings      =  sprintf("%3.2f",levels)
   res@lbLabelAngleF       =  90
   res@cnFillPalette       = cmap   ; Use non-reversed color map again

   res@tiMainString        = "Labelbar: format labels"
   plot(5) = gsn_csm_contour_map(wks,var,res)

;-- draw the panel plot
   gsn_panel(wks,plot,(/3,2/),False)

end
