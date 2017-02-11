;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_contour_labels.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var
  diri  = "./"
  fili  = "rectilinear_grid_2D.nc"
  
  if (fileexists(diri+fili)) then
     file1 = addfile(diri+fili, "r")
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
  
  var =  file1->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_contour_labels")

;---- set resources
  res                       =  True
  res@gsnDraw               =  False
  res@gsnFrame              =  False
  res@gsnMaximize           =  True
  res@tiMainFontHeightF     =  0.02
  res@cnLevelSelectionMode  = "ManualLevels"
  res@cnMinLevelValF        =  250.
  res@cnMaxLevelValF        =  310.
  res@cnLevelSpacingF       =    2.
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLatF      =  35.
  res@mpRightCornerLatF     =  50.
  res@mpLeftCornerLonF      = -12.
  res@mpRightCornerLonF     =  10.

;-- create plots
  plot = new(3,graphic)

  res@cnInfoLabelOrthogonalPosF  = -0.88
  res@cnInfoLabelParallelPosF    = 1.1
  res@cnInfoLabelAngleF          = -90.
  res@cnInfoLabelFontColor       = "blue"
  res@cnInfoLabelPerimColor      = "blue"
  res@tiMainString               = "NCL Doc Example:  contour lines and labels"
  res@cnLineLabelPlacementMode   = "computed"
  plot(0) = gsn_csm_contour_map(wks, var, res)

  res@tiMainString               = ""
  res@cnLineLabelPlacementMode   = "constant"
  plot(1) = gsn_csm_contour_map(wks, var, res)

  res@tiMainString               = ""
  delete(res@cnLineLabelPlacementMode)          ;-- set to default: randomized
  res@cnLineLabelBackgroundColor = "white"
  res@cnSmoothingOn              =  True
  res@cnSmoothingTensionF        =  1.
  res@cnSmoothingDistanceF       =  0.005
  res@cnLevelSpacingF            =  1.
  plot(2) = gsn_csm_contour_map(wks, var, res)

;-- draw the panel plot
  pres = True
  pres@gsnPanelFigureStrings = (/"a)","b)","c)"/)
  gsn_panel(wks,plot,(/3,1/), pres)

end



