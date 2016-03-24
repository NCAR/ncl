;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_panel_3x2_plot.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

undef("file_ERROR")
procedure file_ERROR(diri,names[*]:string)
local files_exist, ii
begin
   files_exist = fileexists(diri+names)
   if(any(.not.files_exist)) then
     ii = ind(.not.files_exist)
     print("")
     print("You don't have the necessary data file(s) for this script. You can download them from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+names(ii))
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+names(ii))
     print("")
     exit
  end if
end


begin
;-- read data and set variable references

  diri = "./"
  fil1 = "orog_mod1_rectilinear_grid_2D.nc"
  fil2 = "sftlf_mod1_rectilinear_grid_2D.nc"
  fil3 = "tas_rectilinear_grid_2D.nc"
  fil4 = "uas_rectilinear_grid_2D.nc"
  fil5 = "vas_rectilinear_grid_2D.nc"

  file_ERROR(diri,(/fil1,fil2,fil3,fil4,fil5/))

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

  land_only = orog
  land_only = where(sftlf .ge.10, orog, orog@_FillValue)

;-- open a PNG file
  wks = gsn_open_wks("png","plot_panel_3x2_plot")

;-- create plot array (3 rows and 2 columns  3*2=6)

  plot = new(6,graphic)

;-- set resources for contour plots
  res                                 =  True
  res@gsnDraw                         =  False
  res@gsnFrame                        =  False
  res@gsnAddCyclic                    =  True

  res@tiMainString                    = ""
  
  res@cnInfoLabelOn                   =  False
  res@cnFillOn                        =  True
  res@cnFillMode                      = "RasterFill"
  res@cnRasterSmoothingOn             =  True
  res@cnFillPalette                   = "BlueRed"

  res@mpProjection                    = "CylindricalEquidistant"  ;-- change projection

;-- upper left plot

  plot(0) = gsn_csm_contour_map(wks,u(0,:,:),res)

;-- upper right plot
;-- set the viewport to the same size as plot(0)

  getvalues plot(0)
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  res@mpShapeMode                     = "FreeAspect"
  res@vpWidthF                        = vpw
  res@vpHeightF                       = vph

  res@cnFillPalette                   = "OceanLakeLandSnow"

  res@mpLimitMode                     = "Corners"
  res@mpLeftCornerLonF                =  60.0
  res@mpRightCornerLonF               = 120.0
  res@mpLeftCornerLatF                =   8.0
  res@mpRightCornerLatF               =  43.0

  res@cnLevelSelectionMode            = "ManualLevels"  ;-- Set contour levels manually
  res@cnMinLevelValF                  =  0.             ;-- Minimum contour level
  res@cnMaxLevelValF                  =  5000.          ;-- Maximum contour level
  res@cnLevelSpacingF                 =  250            ;-- Contour level spacing

  plot(1) = gsn_csm_contour_map(wks,land_only,res)

;-- delete some resources

  delete([/res@mpLimitMode, res@mpLeftCornerLonF, \
           res@mpRightCornerLonF, res@mpLeftCornerLatF, \
           res@mpRightCornerLatF, res@cnLevelSelectionMode, \
           res@cnMinLevelValF, res@cnMaxLevelValF, \
           res@cnLevelSpacingF/])

;-- middle left plot

  res@tiMainString                    = ""
  res@cnFillPalette                   = "BlueRed"
  res@cnLevelSelectionMode            = "AutomaticLevels" ;-- set contour levels manually

  plot(2) = gsn_csm_contour_map(wks,v(0,:,:),res)

;-- middle right plot

  res@cnFillPalette                   = "WhiteGreen"

  plot(3) = gsn_csm_contour_map(wks,sftlf,res)

;-- set resources for vector plot

  vres                                =  True
  vres@gsnDraw                        =  False
  vres@gsnFrame                       =  False
  vres@gsnAddCyclic                   =  True
  vres@gsnLeftString                  = "Vector Wind"
  vres@vcRefAnnoOrthogonalPosF        =  -1.0
  vres@vcRefMagnitudeF                =  10.0
  vres@vcRefLengthF                   =  0.045
  vres@vcGlyphStyle                   = "CurlyVector"
  vres@vcMinDistanceF                 =  0.017

;-- lower left plot

  plot(4) = gsn_csm_vector_map(wks,u(0,::4,::4),v(0,::4,::4),vres)

;-- lower right plot
;-- set the viewport to the same size as plot(4)

  getvalues plot(4)
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

  res@mpShapeMode                     = "FreeAspect"
  res@vpWidthF                        = vpw
  res@vpHeightF                       = vph

  res@cnFillPalette                   = "ncl_default"
  res@cnLinesOn                       =  False
  res@cnLevelSelectionMode            = "ManualLevels"  ;-- Set contour levels manually
  res@cnMinLevelValF                  =  245.           ;-- Minimum contour level
  res@cnMaxLevelValF                  =  305.           ;-- Maximum contour level
  res@cnLevelSpacingF                 =  2.5             ;-- Contour level spacing
  res@mpLimitMode                     = "Corners"
  res@mpLeftCornerLonF                = -10.0
  res@mpRightCornerLonF               =  60.0
  res@mpLeftCornerLatF                =  34.0
  res@mpRightCornerLatF               =  73.0

  plot(5) = gsn_csm_contour_map(wks,t(0,:,:),res)

;-- plot one title on top of the plot

  pnlres                              = True
  pnlres@txString                     = "NCL Doc Example: panel 3x2"

;-- create panel plot

  gsn_panel(wks,plot,(/3,2/),pnlres)

end
