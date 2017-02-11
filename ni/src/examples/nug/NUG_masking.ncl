;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_masking.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

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
;---- read the data and define variable reference var
  diri  = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fil1  = "tas_rectilinear_grid_2D.nc"
  fil2  = "sftlf_mod1_rectilinear_grid_2D.nc"

  file1          = open_file(diri,fil1)
  var            =  file1->tas(0,:,:)
  var&lat@units  = "degrees_north"
  var&lon@units  = "degrees_east"

;---- read the land /sea mask data
  mask1          =  open_file(diri,fil2)
  lsm1           =  mask1->sftlf(:,:)
  lsm1&lat@units = "degrees_north"
  lsm1&lon@units = "degrees_east"

  land_only      =  var                                        ;-- copy attributes, too.
  land_only      =  where(lsm1 .ge. 50, var, var@_FillValue)   ;-- mask less than 50%

;---- define the workstation (plot output type and name)
  wks = gsn_open_wks("png","NUG_masking")

;---- set resources
  res                       =  True
  res@gsnMaximize           =  True
  res@lbBoxMinorExtentF     =  0.15                                   ;-- decrease the height of the labelbar
  res@cnFillOn              =  True                                   ;-- turn on contour fill
  res@cnLinesOn             =  False                                  ;-- turn off contour lines
  res@cnLineLabelsOn        =  False                                  ;-- turn off line labels
  res@cnLevelSelectionMode  = "ManualLevels"                          ;-- set contour levels manually
  res@cnMinLevelValF        =  250.                                   ;-- minimum contour level
  res@cnMaxLevelValF        =  310.                                   ;-- maximum contour level
  res@cnLevelSpacingF       =  1                                      ;-- contour level spacing
  res@tiMainString          = "NCL Doc Example:  masking"   ;-- title string
  res@tiMainFontHeightF     =  0.02

;---- draw the contour map
  plot = gsn_csm_contour_map(wks, land_only, res)

end
