;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_panel_plot.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read data and set variable references

  diri = "./"
  fil1 = "orog_mod1_rectilinear_grid_2D.nc"
  fil2 = "sftlf_mod1_rectilinear_grid_2D.nc"
  fil3 = "tas_rectilinear_grid_2D.nc"
  fil4 = "uas_rectilinear_grid_2D.nc"
  fil5 = "vas_rectilinear_grid_2D.nc"
  
  f1   =  addfile(diri+fil1, "r")
  f2   =  addfile(diri+fil2, "r")
  f3   =  addfile(diri+fil3, "r")
  f4   =  addfile(diri+fil4, "r")
  f5   =  addfile(diri+fil5, "r")

  orog  = f1->orog
  sftlf = f2->sftlf
  t     = f3->tas
  u     = f4->uas
  v     = f5->vas

;-- open a PNG file
  wks = gsn_open_wks("png","plot_panel_plot")

;-- create plot array
  plot = new(3,graphic)

;-- set resources for contour plots
  res                                 =  True
  res@gsnDraw                         =  False
  res@gsnFrame                        =  False
  res@gsnAddCyclic                    =  True
  res@gsnMaximize                     =  True
  res@tiMainString                    = "NCL UG Example: panel plot"
  res@cnInfoLabelOn                   =  False

  plot(0) = gsn_csm_contour_map(wks,u(0,:,:),res)

  res@tiMainString                    = ""
  plot(1) = gsn_csm_contour_map(wks,v(0,:,:),res)

;-- set resources for vector plot
  vres                                =  True
  vres@gsnDraw                        =  False
  vres@gsnFrame                       =  False
  vres@gsnAddCyclic                   =  True
  vres@gsnMaximize                    =  True
  vres@gsnLeftString                  = "Vector Wind"
  vres@vcRefAnnoOrthogonalPosF        =  -1.0
  vres@vcRefMagnitudeF                =  10.0
  vres@vcRefLengthF                   =  0.045
  vres@vcGlyphStyle                   = "CurlyVector"
  vres@vcMinDistanceF                 =  0.017

  plot(2) = gsn_csm_vector_map(wks,u(0,::4,::4),v(0,::4,::4),vres)

;-- create panel plot
  gsn_panel(wks,plot,(/3,1/),False)
end
