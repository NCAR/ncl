;---------------------------------------------------
;  NCL User Guide Example:   NUG_labelbar_title.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define
  diri   = "./"
  fili   = "rectilinear_grid_2D.nc"
  
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
  
  var = file1->tsurf(0,:,:)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","NUG_labelbar_title")

;-- set resources
  res                       =  True
  res@gsnMaximize           =  True

  res@cnFillOn              =  True            ;-- turn on contour fill
  res@cnLinesOn             =  False           ;-- turn off contour lines
  res@cnLineLabelsOn        =  False           ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"   ;-- set contour levels manually
  res@cnMinLevelValF        =  250.            ;-- minimum contour level
  res@cnMaxLevelValF        =  310.            ;-- maximum contour level
  res@cnLevelSpacingF       =  1               ;-- contour level spacing

  res@lbLabelAngleF         =  40              ;-- rotate the labels
  res@lbTitleOn             =  True            ;-- turn on title
  res@lbTitleString         = "Labelbar title string"
  res@lbTitleFontHeightF    =  0.018           ;-- increase label title size
  res@pmLabelBarOrthogonalPosF = 0.12          ;-- move the labelbar downward
  res@pmLabelBarWidthF      = 0.8              ;-- increase labelbar width
  res@pmLabelBarHeightF     =  0.08            ;-- decrease labelbar height

  res@tiMainString          = "NCL Doc Example: labelbar"   ;-- title
  res@tiMainFontHeightF     =  0.02

;-- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
