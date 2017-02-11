;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_date_format.ncl
; 
;  This example requires the Climate Data Operators (cdo), which
;  can be downloaded from: https://code.zmaw.de/projects/cdo/files
;
;  KMF 30.10.14
;-----------------------------------------------------------------
undef ("getDate")
function getDate(time)
;----------------------
begin
   ;-- convert the time proleptic_gregorian calendar in UTC date
   utc_date = cd_calendar(time, 0)
   ;-- set date variable names (just helpful) 
   year   = tointeger(utc_date(:,0))
   month  = tointeger(utc_date(:,1))
   day    = tointeger(utc_date(:,2))
   hour   = tointeger(utc_date(:,3))
   minute = tointeger(utc_date(:,4))
   second = utc_date(:,5)
  ;-- write date as string (DD.MM.YYYY)
   date_str_i = sprinti("%0.2i",day) + "." + sprinti("%0.2i",month) + "."  + sprinti("%0.4i",year)
   return(date_str_i)
end

;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  diri = "./"
  fil1 = "tas_mon_1961-1990_rectilinear_grid_2D.nc"

  if (.not. fileexists(diri+fil1)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fil1)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fil1)
     print("")
     exit
  end if

;-- Calculate yearly means using climate data operators (cdo)
  print("You must have 'cdo' installed on this system. See comments for more informtion.")
  system("cdo -s -r -f nc yearmean "+diri+fil1+" tser_tmp.nc")

  f1                         = addfile("tser_tmp.nc","r")
  var                        = f1->tas(:,0,0,0)
  time                       = var&time
  timax                      = dimsizes(time) - 1

  f2                         = addfile(diri+fil1,"r")
  var2                       = f2->tas(:,0,:,:)
  var2_avg                   = dim_avg_n_Wrap(month_to_annual(var2,1),2)
  tas_avg = var2_avg(:,0)

;-- create the time strings, plot every second axis annotation
  incr                       = 2
  date_str_i                 = getDate(time)
  labels                     = (/ date_str_i(0::incr) /)

;-- define the workstation (plot type and name)
  wks  = gsn_open_wks("png", "NUG_date_format")

;-- set resources
  res                        =  True
  res@gsnDraw                =  False
  res@gsnFrame               =  False
  res@gsnMaximize            = True

  res@xyLineColor            = "blue"
  res@xyLineThicknessF       =  2
  res@xyDashPattern          =  0
  res@vpWidthF               =  0.7
  res@vpHeightF              =  0.37
  res@tiMainString           = "NCL Doc Example: date format"
  res@tiXAxisString          = "Time"
  res@tiYAxisString          = "Temperature"
  res@trYMinF                =  279.2
  res@trYMaxF                =  280.6
  res@trXMinF                =  time(0)
  res@trXMaxF                =  time(timax)
  res@tmXBMode               = "Explicit"
  res@tmXBValues             =  var&time(::incr)
  res@tmXBLabels             =  labels
  res@tmXBLabelFontHeightF   =  0.01
  res@tmXBLabelJust          = "CenterRight"
  res@tmXBLabelDeltaF        =  1.0
  res@tmXBLabelAngleF        =  50.
  res@tmYROn                 =  False
  res@tmXTOn                 =  False

;-- draw the contour map
  plot = gsn_csm_xy(wks, var&time, var, res)
  
  res@xyLineColor       = "red"
  plot2 = gsn_csm_xy(wks, var&time, tas_avg, res)

;-- merge contents from plot2 to plot
  overlay(plot,plot2)

;-- draw the merged plot
  draw(plot)
  frame(wks)

;-- delete temporary file
  system("rm -f tser_tmp.nc")

end
