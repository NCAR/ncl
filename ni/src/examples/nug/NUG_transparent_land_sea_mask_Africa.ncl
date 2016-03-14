;---------------------------------------------------
;  NCL User Guide Example:   NCL_transparent_land_sea_mask_Africa.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
   diri      = "./"
   file1     = "tas_rectilinear_grid_2D.nc"
   mask_file = "sftlf_mod1_rectilinear_grid_2D.nc"

;-- read the data
   f                         =  addfile(diri+file1,"r")
   var                       =  f->tas(0,:,:)
   var&lat@units             = "degrees_north"
   var&lon@units             = "degrees_east"
   var                       =  lonFlip(var)         ;-- flip from 0-360 degrees to -180-180 degrees

   mask1                     =  addfile(diri+mask_file,"r")
   lsm1                      =  mask1->sftlf(:,:)
   lsm1&lat@units            = "degrees_north"
   lsm1&lon@units            = "degrees_east"
   lsm1                      =  lonFlip(lsm1)        ;-- flip from 0-360 degrees to -180-180 degrees

   land_only                 =  var                                        ;-- copy attributes, too.
   land_only                 =  where(lsm1 .ge. 50, var, var@_FillValue)   ;-- mask less than 50%

;-- define the workstation (graphic will be written to a file)
   wks      =  gsn_open_wks("png","plot_transparent_lsm")

;-- set plot resources
   res                       =  True
   res@gsnDraw               =  False
   res@gsnFrame              =  False
   res@cnFillOn              =  True
   res@cnLevelSpacingF       =  1
   res@cnLinesOn             =  False

   mres                      =  res
   mres@mpProjection         = "Orthographic"
   mres@mpCenterLonF         =  20
   mres@mpGridAndLimbOn      =  True
   mres@mpGridMaskMode       = "MaskNotOcean"
   mres@cnFillOpacityF       =  0.5        ;-- 50% opaque
   mres@lbLabelStride        =  10
   mres@lbBoxLinesOn         =  False
   mres@tiMainString         = "NCL UG Example: transparent lsm"

   map = gsn_csm_contour_map(wks,var,mres)

   ores                      =  res
   ores@cnFillOpacityF       =  1.0        ;-- 100% opaque
   ores@cnMinLevelValF       =  250.
   ores@cnMinLevelValF       =  310.
   ores@cnLevelSpacingF      =  1
   ores@cnLineLabelsOn       =  False
   ores@lbLabelBarOn         =  False
   ores@gsnRightString       = ""
   ores@gsnLeftString        = ""

   plot1 = gsn_csm_contour(wks,land_only({-38:40},{-25:59}),ores)

   overlay(map,plot1)
   draw(map)
   frame(wks)
end
