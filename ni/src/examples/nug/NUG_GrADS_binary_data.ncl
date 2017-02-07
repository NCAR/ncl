;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_GrADS_binary_data.ncl
;
;  grads ctl data descriptor file
;
;  DSET   ^ps_grads_model.dat
;  OPTIONS little_endian
;  UNDEF  -2.56E33
;  TITLE 5 Days of Sample Model Output
;  XDEF 73 LINEAR  0.0 5.0
;  YDEF 46 LINEAR  -90.0 4.0
;  *ZDEF 7 LEVELS 1000 850 700 500 300 200 100
;  ZDEF 1 linear 1 1
;  TDEF 5 LINEAR 02JAN1987 1DY
;  VARS 1
;  PS     0   99   Surface Pressure
;  ENDVARS
;
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

undef("file_ERROR")
procedure file_ERROR(name)
begin
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
;-- read file information from GrADS descriptor ctl file
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  ctl  = "ps_grads_model.ctl" 
  
  if (fileexists(diri+ctl)) then
     fin = systemfunc("cat "+diri+ctl+" | grep DSET | awk '{print $2}'| sed -e 's/\^//g'")
  else
     file_ERROR(ctl)
  end if
  fin = diri + fin  
  if (.not. fileexists(fin)) then
     file_ERROR(fin)
  end if

  print("Binary file:  "+fin)

;-- read data
  setfileoption("bin", "ReadByteOrder", "LittleEndian")
  data = fbindirread (fin, 0, (/ 5, 46, 73 /), "float")

;-- read dimensions
  time = new ( 5, double  ,"No_FillValue")   ; generate a "time" variable
  lon  = new (73, double  ,"No_FillValue")   ; generate a "lon" variable
  lat  = new (46, double  ,"No_FillValue")   ; generate a "lat" variable

  time = ispan(0,4,1)
  lon  = ispan(0,360,5)
  lat  = ispan(0,180,4) -  90.0

  printVarSummary(data)
  printVarSummary(time)
  printVarSummary(lat)
  printVarSummary(lon)

;-- generate lat/lon

  lon!0          = "lon"
  lon@long_name  = "longitude"
  lon@units      = "degrees_east"

  lat!0          = "lat"
  lat@long_name  = "latitude"
  lat@units      = "degrees_north"

  print ("-- read GrADS binary data -- done")

  ps     = data(:,:,:)
  ps!0   = "time"
  ps!1   = "lat"
  ps!2   = "lon"
  ps&time=  time
  ps&lat =  lat
  ps&lon =  lon

  ps@long_name = "Surface Pressure"
  ps@units     = "Pa"
                
  printVarSummary(ps)
  print ("min(ps)="+min(ps))
  print ("max(ps)="+max(ps))

;-- plot variable

  variable = ps(0,:,:)

  wks      =  gsn_open_wks("png","NUG_GrADS_binary_data")

  res                   = True
  res@cnFillOn          = True     ; turn off contour fill
  res@cnLinesOn         = False    ; turn off contour lines
  res@cnLineLabelsOn    = False    ; turn off line labels
  res@gsnMaximize       = True     ; Maximize plot in frame
  res@gsnAddCyclic      = False     ; Don't add a cyclic point

  res@tiMainString      = "NCL Doc Example: reading GrADS binary file"
  res@mpProjection      = "CylindricalEquidistant"
  res@mpLimitMode       = "Corners"

  res@mpLeftCornerLonF  = -180.0
  res@mpRightCornerLonF =  180.0
  res@mpLeftCornerLatF  = -90.0
  res@mpRightCornerLatF =  90.0

  plot = gsn_csm_contour_map(wks,variable,res)

end


