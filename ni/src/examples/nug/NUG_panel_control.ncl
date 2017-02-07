;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_panel_control.ncl
;
;  KMF
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read data and set variable references
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "tas_rectilinear_grid_2D.nc"

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
  
  f    =  addfile(diri+fili, "r")
  t    =  f->tas

;-- open a PNG file
  wks = gsn_open_wks("png","NUG_panel_control")

;-- set resources for contour plots
  res                                  =  True
  res@gsnDraw                          =  False
  res@gsnFrame                         =  False
  res@gsnAddCyclic                     =  True
  res@gsnLeftStringOrthogonalPosF      =  0.03
  res@gsnRightStringOrthogonalPosF     =  0.03

  res1 = res
  res2 = res
  res3 = res
  res4 = res
  res5 = res

;-- global
  plot_1 = gsn_csm_contour_map(wks,t(0,:,:),res1)
  
;-- North America
  res2@mpMinLatF                       =   10.0
  res2@mpMaxLatF                       =   80.0
  res2@mpMinLonF                       = -175.0
  res2@mpMaxLonF                       =  -50.0
  plot_2 = gsn_csm_contour_map(wks,t(0,:,:),res2)

;-- Africa
  res3@mpMinLatF                       =   -40.0
  res3@mpMaxLatF                       =    40.0
  res3@mpMinLonF                       =   -20.0
  res3@mpMaxLonF                       =    50.0
  plot_3 = gsn_csm_contour_map(wks,t(0,:,:),res3)

;-- South America
  res4@mpMinLatF                       =   -60.0
  res4@mpMaxLatF                       =    15.0
  res4@mpMinLonF                       =  -100.0
  res4@mpMaxLonF                       =   -30.0
  plot_4 = gsn_csm_contour_map(wks,t(0,:,:),res4)

;-- Europe
  res5@mpMinLatF                       =    35.0
  res5@mpMaxLatF                       =    80.0
  res5@mpMinLonF                       =   -20.0
  res5@mpMaxLonF                       =    50.0
  plot_5 = gsn_csm_contour_map(wks,t(0,:,:),res5)

;-- create the panel plot
  pnlres                              =  True
  pnlres@gsnMaximize                  =  True
  pnlres@gsnPanelScalePlotIndex       =  3
  pnlres@gsnPanelTop                  =  0.94
  pnlres@gsnPanelBottom               =  0.001
  pnlres@gsnPanelXWhiteSpacePercent   =  0
  pnlres@gsnPanelXWhiteSpacePercent   =  30
  pnlres@txFontHeightF                =  0.020    ;-- text font size
  pnlres@gsnPanelMainString           = "NCL Doc Example: panel control"  ;-- resource added in 6.4.0
; pnlres@txString                     = "NCL Doc Example: panel control"
  
;-- create panel plot
  gsn_panel(wks,(/plot_1,plot_2,plot_3,plot_4,plot_5/),(/3,2/),pnlres)
  
end
