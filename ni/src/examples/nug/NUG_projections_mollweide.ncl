;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_projections_mollweide.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

  diri = "./"
  fili = "MITgcm_rectilinear_grid_3D.nc"

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
  
  file1 =  addfile(diri+fili,"r")
  var   =  file1->SSS(0,0,:,:)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","NUG_projections_mollweide")

;-- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@lbLabelStride         =  2               ;-- label every other box
  res@lbBoxMinorExtentF     =  0.15            ;-- decrease the height of the labelbar
  res@cnFillOn              =  True            ;-- turn ofncontour fill
  res@cnFillPalette         = "BlueWhiteOrangeRed" ;--change color map
  res@cnLinesOn             =  False           ;-- turn off contour lines
  res@cnLineLabelsOn        =  False           ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"   ;-- set contour levels manually
  res@cnMinLevelValF        =   0.             ;-- minimum contour level
  res@cnMaxLevelValF        =  42.             ;-- maximum contour level
  res@cnLevelSpacingF       =   2               ;-- contour level spacing
  res@mpProjection          = "Mollweide"      ;-- change projection
  res@mpGridAndLimbOn       =  True             ;-- plot grid lines

  res@tiMainString          = "NCL Doc Example: Mollweide projection"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;-- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
