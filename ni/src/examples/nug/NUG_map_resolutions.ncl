;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_map_resolutions.ncl
; 
;     Use HighRes RANGS/GSHHS database
;
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- read the data and define

  diri = "./"
  fili = "tas_rectilinear_grid_2D.nc"
   
  f    =  addfile(diri+fili, "r")
  var  =  f->tas(0,:,:)
;  var&lat@units             = "degrees_north"
;  var&lon@units             = "degrees_east"

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png","plot_coastline_resolution")
  
;-- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@lbLabelStride         =  2
  res@lbBoxMinorExtentF     =  0.15            ;-- decrease the height of the labelbar
  res@cnFillOn              =  True            ;-- turn on contour fill
  res@cnFillPalette         =  "rainbow"       ;-- change color map
  res@cnLinesOn             =  False           ;-- turn off contour lines
  res@cnLineLabelsOn        =  False           ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"   ;-- set contour levels manually
  res@cnMinLevelValF        =  250.            ;-- minimum contour level
  res@cnMaxLevelValF        =  310.            ;-- maximum contour level
  res@cnLevelSpacingF       =  1               ;-- contour level spacing

  res@mpProjection          = "CylindricalEquidistant"  ;-- change projection
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLonF      =   7.5
  res@mpRightCornerLonF     =  11.0
  res@mpLeftCornerLatF      =  53.0
  res@mpRightCornerLatF     =  58.0

  res@mpGridAndLimbOn       =  True            ;-- plot grid lines
  res@mpGridLineColor       = "grey30"         ;-- set grid line color

  reslist = (/"LowRes", "MediumRes", "HighRes"/)
  datares = (/"Coarsest","Coarse", "Medium", "Fine", "Finest"/)

  do i=0,(dimsizes(reslist)-1)
     res@mpDataBaseVersion     = reslist(i)
     res@tiMainString          = "NCL UG Example: coastline resolution~C~        "+reslist(i)   ;-- title string
     res@tiMainFontHeightF     =  0.02

     if (reslist(i) .eq. "HighRes") then
        print("")
        do j=0,(dimsizes(datares)-1)
           print("High resolution plot will take some time")
           print("")
           res@mpDataResolution  = datares(j)
           res@tiMainString      = "NCL UG Example: coastline resolution~C~        "+reslist(i)+"   "+datares(j)   ;-- title string
           plot = gsn_csm_contour_map(wks, var, res)
        end do
     else
        plot = gsn_csm_contour_map(wks, var, res)   
     end if
  end do

end
