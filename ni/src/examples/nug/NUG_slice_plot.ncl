;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_slice_plot.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

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
   
  f     =  addfile(diri+fili, "r")  
  var   =  f->t(0,:,{40},{0:60})     ;-- first time step, latitude=40N, longitude=0-60E.
  lon_t =  f->lon({0:60})            ;-- longitude=0-60E
  lev_t =  f->lev                    ;-- currently 17 levels

;-- define workstation
  wks = gsn_open_wks("png","NUG_slice_plot")

;-- set resources
  res                          =  True
  res@tiMainString             = "NCL Doc Example: Slice plot at 40N"   ;-- title string

  res@cnFillOn                 =  True       ;-- turn on color fill
  res@cnLineLabelsOn           =  False      ;-- turns off contour line labels
  res@cnInfoLabelOn            =  False      ;-- turns off contour info label
  res@cnFillPalette            = "ncl_default" ;-- choose color map
  res@lbOrientation            = "vertical" ;-- vertical label bar
  res@tiYAxisString            =  var@long_name+"  [hPa]"
 
  res@sfXArray                 =  lon_t      ;-- uses lon_t as plot x-axis
  res@sfYArray                 =  lev_t/100  ;-- uses lev_t in hPa as plot y-axis

  res@trYReverse               =  True       ;-- reverses y-axis
  res@gsnYAxisIrregular2Log    =  True       ;-- converts y-axis irregular to linear depth


;-- generate the plot
  plot = gsn_csm_contour(wks,var,res)

end
