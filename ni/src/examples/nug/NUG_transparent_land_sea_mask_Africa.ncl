;---------------------------------------------------
;  NCL User Guide Example:   NUG_transparent_land_sea_mask_Africa.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;-------------------------------------------
;-- Check for existence of file and open it
;-------------------------------------------
undef("open_file")
function open_file(dir[1]:string,name[1]:string)
begin
   if (fileexists(dir+name)) then
      f = addfile(dir+name, "r")
      return(f)
   end if
   print("")
   print("You don't have the necessary data for this script. You can download it from:​")
   print("")
   print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   print("or use the wget command:")
   print("")
   print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   exit
end

begin
   diri      = "$NCARG_ROOT/lib/ncarg/data/nug/"
   data_file = "tas_rectilinear_grid_2D.nc"
   mask_file = "sftlf_mod1_rectilinear_grid_2D.nc"
   df = open_file(diri,data_file)
   mf = open_file(diri,mask_file)
  
;-- read the data
   var                       =  df->tas(0,:,:)
   var&lat@units             = "degrees_north"
   var&lon@units             = "degrees_east"
   var                       =  lonFlip(var)         ;-- flip from 0-360 degrees to -180-180 degrees

   lsm1                      =  mf->sftlf
   lsm1&lat@units            = "degrees_north"
   lsm1&lon@units            = "degrees_east"
   lsm1                      =  lonFlip(lsm1)        ;-- flip from 0-360 degrees to -180-180 degrees

   land_only                 =  var                                        ;-- copy attributes, too.
   land_only                 =  where(lsm1 .ge. 50, var, var@_FillValue)   ;-- mask less than 50%

;-- define the workstation (graphic will be written to a file)
   wks      =  gsn_open_wks("png","NUG_transparent_land_sea_mask_Africa")

;-- set common plot resources
   res                       =  True
   res@gsnDraw               =  False
   res@gsnFrame              =  False
   res@cnFillOn              =  True
   res@cnLevelSpacingF       =  1
   res@cnLinesOn             =  False
   res@lbBoxLinesOn          =  False

   bres                      =  res
   bres@mpProjection         = "Orthographic"
   bres@mpCenterLonF         =  20
   bres@mpGridAndLimbOn      =  True
   bres@mpGridMaskMode       = "MaskNotOcean"
   bres@cnFillOpacityF       =  0.5        ;-- 50% opaque
   bres@lbOrientation        = "horizontal"
   bres@lbLabelStride        =  10         ;-- label every 10th box
   bres@tiMainString         = "NCL Doc Example: transparent lsm"

   base_plot = gsn_csm_contour_map(wks,var,bres)

   ores                      =  res
   ores@cnFillOpacityF       =  1.0        ;-- 100% opaque
   ores@cnMinLevelValF       =  250.
   ores@cnMinLevelValF       =  310.
   ores@cnLevelSpacingF      =  1
   ores@cnLineLabelsOn       =  False
   ores@lbOrientation        = "vertical"
   ores@gsnRightString       = ""
   ores@gsnLeftString        = ""

   ovrly_plot = gsn_csm_contour(wks,land_only({-38:40},{-25:59}),ores)

   overlay(base_plot,ovrly_plot)
   draw(base_plot)
   frame(wks)
end
