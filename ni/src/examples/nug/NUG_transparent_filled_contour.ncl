;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_transparent_filled_contour.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; NOTE: This example will only work with NCL V6.1.0 and later.
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   

begin
;-- read data
  diri = "./"
  fili = "uv300.nc"

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
  
  a   = addfile(diri+fili,"r")
  u   = a->U(1,:,:)

;-- open a workstation
  wks  = gsn_open_wks("png","plot_transparency")

;-- set resources
  res                        =  True
  res@gsnFrame               =  False
  res@gsnDraw                =  False
  res@cnFillOn               =  True
  res@cnFillPalette          = "BlueYellowRed"
  res@cnLinesOn              =  False
  res@cnLineLabelsOn         =  False
  res@cnInfoLabelOn          =  False
  res@cnLevelSelectionMode   = "ExplicitLevels"
  res@cnLevels               =  ispan(-12,40,2)

;-- set resources only for main plot displaying the contours
  bres                       =  res
  bres@gsnMaximize           =  True       ;-- maximize main plot
  bres@mpFillOn              =  False 
  bres@tiMainString          = "NCL Doc Example: transparency I"
  bres@cnFillOpacityF        =  0.5        ;-- 50% opaque

  main_plot = gsn_csm_contour_map(wks,u,bres)

;-- set resources for the overlayed plot
  ores                       =  res
  ores@cnFillOpacityF        =  1.0        ;-- 100% opaque
  ores@gsnRightString        = ""
  ores@gsnLeftString         = ""
  ores@lbLabelBarOn          =  False

  overlay_plot = gsn_csm_contour(wks,u({-30:30},{-120:120}),ores)

  overlay(main_plot,overlay_plot)

  draw(main_plot)
  frame(wks)

;-- create a new plot with 100% opacity
  bres@tiMainString          = "NCL Doc Example: transparency II"
  bres@cnFillOpacityF        =  1.0        ;-- 100% opaque
  plot = gsn_csm_contour_map(wks,u,bres)

;---Set resources for a partially transparent polygon.
  gnres                      =  True
  gnres@gsFillOpacityF       =  0.5        ;-- 50% opaque
  gnres@gsFillColor          = "white"

  lat_box = (/ -30,-30, 30,  30, -30/)
  lon_box = (/-120,120,120,-120,-120/)

  gsid = gsn_add_polygon(wks,plot,lon_box,lat_box,gnres)

;-- draw the second plot to a new page
  draw(plot)
  frame(wks)
end
