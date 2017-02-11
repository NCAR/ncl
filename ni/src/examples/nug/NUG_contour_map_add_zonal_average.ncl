;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_contour_map_add_zonal_average.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
;---- read the data and define variable reference var
  diri  = "./"
  fili  = "rectilinear_grid_2D.nc"
  
  if (fileexists(diri+fili)) then
     file1 = addfile(diri+fili, "r")
  else
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

  var   =  file1->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_contour_map_add_zonal_average")

;---- set resources
  res                       =  True
  res@gsnDraw               =  False
  res@gsnFrame              =  False

  res@tiMainString          = "NCL Doc Example:  contour map add zonal average"
  res@tiMainFontHeightF     =  0.02

;---- create the contour map
  plot = gsn_csm_contour_map_ce(wks, var, res)

;-- set the values and labels for the zonal average
  values = (/240., 273.15, 315./)
  labels = new(dimsizes(values),string)
  do i=0,dimsizes(values)-1
     labels(i) = flt2string(values(i))
  end do

;-- set the resources for the zonal mean attached plot
  zres                       =  True

  zres@gsnZonalMeanYRefLine  =  273.15		        ;-- set reference line X-axis value

  zres@trXMinF               =  values(0)               ;-- gsnZonalMeanXMinF could also be used instead
  zres@trXMaxF               =  values(dimsizes(values)-1) ;-- gsnZonalMeanXMaxF could also be used instead
  zres@trYMinF               = -90                      ;-- latitude min
  zres@trYMaxF               =  90                      ;-- latitude max
 
  zres@tmXBMode              = "Explicit"
  zres@tmXBValues            =  values                  ;-- set the explicit values
  zres@tmXBLabels            =  labels                  ;-- set the explicit labels
  zres@tmXBLabelFontHeightF  =  0.01                    ;-- use a smaller font size for the labels
  zres@tmXBLabelFontColor    = "blue"                   ;-- write text in blue color

  zres@xyLineColor           = "blue"                   ;-- draw the zonal average data with a blue color
  zres@xyLineThicknessF      =  2                       ;-- make the lines thicker

  zres@vpWidthF              = 0.1

;-- attach the zonal average plot to the contour plot
  zonal_id = gsn_csm_attach_zonal_means(wks,plot,var,zres)

;-- draw the plot and frame
  draw(plot)
  frame(wks)

end
