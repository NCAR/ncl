;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_contour_fillpattern.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var
  diri = "./"
  fili = "rectilinear_grid_2D.nc"
  
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

  var   =  file1->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_contour_fillpattern")

;---- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@tiMainString          = "NCL Doc Example:  contour fill pattern"
  res@tiMainFontHeightF     =  0.02

  res@cnLevelSelectionMode  = "ManualLevels"
  res@cnMinLevelValF        =  250.
  res@cnMaxLevelValF        =  310.
  res@cnLevelSpacingF       =    5.
  res@cnMonoFillPattern     =  False
  res@cnMonoFillScale       =  False
  res@cnFillOn              =  True
  res@cnFillColors          =  (/"blue4","blue","darkgreen","green","cyan3",\
                                 "gold","orange","darkorange","red","red4",\
                                 "violet","purple","mediumorchid4","purple4"/)
  res@cnFillPatterns        =  (/0,1,2,3,4,5,6,7,8,17,10,11,12,16/)
  res@cnFillDotSizeF        =  0.003
  res@cnFillScales          =  (/1.,.4,.5,.3,.5,.5,.5,.5,.5,1.,.5,.5,.5,.4/)

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
