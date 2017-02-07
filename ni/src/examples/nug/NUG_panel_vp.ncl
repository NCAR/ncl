;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_panel_vp.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
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
  wks = gsn_open_wks("png","NUG_panel_vp")

;-- set resources for contour plots
  res                                  =  True
  res@gsnDraw                          =  False
  res@gsnFrame                         =  False
  res@gsnAddCyclic                     =  True
  res@gsnLeftStringOrthogonalPosF      =  0.03
  res@gsnRightStringOrthogonalPosF     =  0.03
  res@vpWidthF                         =  0.4
  res@vpHeightF                        =  0.27
;-- global
  res@vpXF                             =  0.08
  res@vpYF                             =  0.99
  
  plot_1 = gsn_csm_contour_map(wks,t(0,:,:),res)
  draw(plot_1)
  
;-- North America
  res@vpXF                            =  0.55
  res@vpYF                            =  0.99
  res@mpMinLatF                       =   10.0
  res@mpMaxLatF                       =   80.0
  res@mpMinLonF                       = -175.0
  res@mpMaxLonF                       =  -50.0

  plot_2 = gsn_csm_contour_map(wks,t(0,:,:),res)
  draw(plot_2)

;-- Africa
  res@vpXF                            =  0.1
  res@vpYF                            =  0.67
  res@mpMinLatF                       =  -40.0
  res@mpMaxLatF                       =   40.0
  res@mpMinLonF                       =  -20.0
  res@mpMaxLonF                       =   50.0
  
  plot_3 = gsn_csm_contour_map(wks,t(0,:,:),res)
  draw(plot_3)

;-- South America
  res@vpXF                            =  0.55
  res@vpYF                            =  0.67
  res@mpMinLatF                       =  -60.0
  res@mpMaxLatF                       =   15.0
  res@mpMinLonF                       = -100.0
  res@mpMaxLonF                       =  -30.0
  
  plot_4 = gsn_csm_contour_map(wks,t(0,:,:),res)
  draw(plot_4)

;-- Europe
  res@vpXF                            =  0.35
  res@vpYF                            =  0.33
  res@mpMinLatF                       =   35.0
  res@mpMaxLatF                       =   80.0
  res@mpMinLonF                       =  -20.0
  res@mpMaxLonF                       =   50.0
  
  plot_5 = gsn_csm_contour_map(wks,t(0,:,:),res)
  draw(plot_5)

;-- advance the frame
  frame(wks)
  
end

