;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_title_strings.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;---- read the data and define variable reference var

  diri  = "./"
  fili  = "rectilinear_grid_2D.nc"

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
  
  file1 =  addfile(diri+fili, "r")
  var   =  file1->tsurf(0,:,:)

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_title_strings")

;---- set resources
  res                       =  True
  res@gsnMaximize           =  True

;-- set the gsn title strings
  res@gsnLeftString         = "Left String"
  res@gsnCenterString       = "Center String"
  res@gsnRightString        = "Right String"

;-- set the title string. ~C~ insert a carriage return (no \ allowed).
  res@tiMainString          = "NCL Doc Example:  Title strings ~C~      - second line of the title string ~C~ ~Z70~             - third line of the title string with font size 70% ~C~  "
  res@tiMainFontHeightF     =  0.02

  res@tiXAxisString         = "X-Axis title string"  
  res@tiYAxisString         = "Y-Axis title string"

  res@tiXAxisSide           = "Bottom"                 ;-- X-Axis title on bottom
  res@tiYAxisSide           = "Right"                  ;-- Y-axis title on right side
  res@tiYAxisAngleF         = 270                      ;-- Y-axis title rotate 270 degrees

  res@tiXAxisFontHeightF    = 0.015                    ;-- X-Axis title font size
  res@tiYAxisFontHeightF    = 0.015                    ;-- Y-Axis title font size

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, var, res)

end
