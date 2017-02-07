;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_multiple_plots_along_y.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

   diri  = "$NCARG_ROOT/lib/ncarg/data/nug/"
   fili  = "rectilinear_grid_3D.nc"

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
  
   f     = addfile(diri+fili,"r")
   var   = f->t(0,:,{40},:)

   values = f->lev/100
   labels = (/ "1000", "925", "850", "775", "700", "600", "500", "400", "300", "250", "200", "150", "100", "70", "50", "30", "10" /)

;-- open workstation
   wks  = gsn_open_wks ("png", "NUG_multiple_plots_along_y" )
   gsn_define_colormap(wks,"BlWhRe")                ; choose colormap

;-- resources pressure height plot
   res                          =  True             ;-- plot mods desired
   res@gsnDraw                  =  False            ;-- don't draw yet
   res@gsnFrame                 =  False            ;-- don't advance frame yet

   xy_res  =  res

   res@cnFillOn                 =  True             ;-- turn on color fill
   res@cnLevelSelectionMode     = "ManualLevels"    ;-- manual contour levels
   res@cnMinLevelValF           =  180.             ;-- minimum level
   res@cnMaxLevelValF           =  320.             ;-- maximum level
   res@cnLevelSpacingF          =    5.             ;-- contour spacing
   res@pmLabelBarOrthogonalPosF = -0.05             ;-- position label bar
   res@tiMainString             = "NCL Doc Example: attach plot ~C~                     lat: 40~S~o~N~"
   res@trYReverse               =  True             ;-- reverese y-axis
   res@vpWidthF                 =  .65              ;-- set width
   res@vpHeightF                =  .60              ;-- set height
   res@vpXF                     =  .135             ;-- set x-pos
   res@vpYF                     =  .8               ;-- set y-pos
   res@tmYLMode                 = "Explicit"
   res@tmYLValues               =  values
   res@tmYLLabels               =  labels
   res@tmYROn                   =  False
   res@tmYLMinorOn              =  False
   res@tmXBLabelFontHeightF     =  .02
   res@tmYLLabelFontHeightF     =  .012
   res@gsnPresHgtHeightLabelOn  =  False

   plot_slice = gsn_csm_pres_hgt(wks, var, res)
;   plot_slice = gsn_csm_contour(wks, var, res)

;-- create xy-plot
   x   = dim_avg_Wrap(f->t(0,:,{40},:))
   y   = f->lev/100

   getvalues plot_slice
     "vpXF"      : vpx
     "vpYF"      : vpy
     "vpWidthF"  : vpw
     "vpHeightF" : vph
   end getvalues

   print("vpXF = "+vpx+"  vpYF = "+vpy+"  vpWidthF = "+vpw+"  vpHeightF = "+vph)

   xy_res@tmYLMode                = "Explicit"
   xy_res@tmYLValues              =  values
   xy_res@tmYLMinorOn             =  False        ; No minor tick marks.

;-- resources xy-plot
   xy_res@vpWidthF                =  .15                ;-- set width
   xy_res@vpHeightF               =  vph                ;-- set height
   xy_res@vpXF                    =  vpx+vpw+.02        ;-- set x-pos
   xy_res@vpYF                    =  vpy                ;-- set y-position

   xy_res@tmYRMode                = "Explicit"
   xy_res@tmYRValues              =  values
   xy_res@tmYRLabels              =  labels
   xy_res@tmYRBorderOn            =  True
   xy_res@tmXTOn                  =  True
   xy_res@tmXBOn                  =  True
   xy_res@tmXBMinorOn             =  False              ;-- no minor tickmarks
;   xy_res@tmXBLabelStride         =  2                  ;-- label stride
   xy_res@gsnCenterString         = "Zonal Ave"         ;-- add title
   xy_res@txFontHeightF           =  .015               ;-- change font height
   xy_res@trYMinF                 =  min(values)
   xy_res@trYMaxF                 =  max(values)
   xy_res@xyYStyle                = "Log"
   xy_res@trXLog                  =  True
   xy_res@trYReverse              =  True               ;-- reverese y-axis

   plot_xy = gsn_csm_xy(wks, x, y, xy_res)

;-- attach plot
;   plot_attach = gsn_attach_plots(plot_slice,plot_xy,res,xy_res)

   draw(plot_slice)
   draw(plot_xy)
   frame(wks)
 
end
