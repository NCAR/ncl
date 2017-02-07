;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_xy_plot_timeseries_2.ncl
;
;  KMF 30.10.14
;-----------------------------------------------------------------
;==================================
;== define function getDate(t)
;==================================
undef ("getDate")
function getDate(t)
begin
   ;-- convert the time proleptic_gregorian calendar in UTC date
   utc_date = cd_calendar(t, 0)
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

;==================================================================================
;== MAIN
;==================================================================================
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
   diri = "./"
   fili = "rectilinear_grid_2D.nc"
   
   if (fileexists(diri+fili)) then
      f = addfile(diri+fili, "r")
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
  
   var  =  f->tsurf
   time =  var&time
   print(time)

   fldmean = wgt_areaave_Wrap(var,1.0,1.0,1)
   
   date_str_i = getDate(time)
   labels     = (/ date_str_i/)

   wks = gsn_open_wks("png","NUG_xy_plot_timeseries_2")

   res                        =  True
   res@tiMainString           = "NCL Doc Example: xy-plot timeseries"
   res@tmXBMode               = "Explicit"
   res@tmXBValues             =  time
   res@tmXBLabels             =  labels
   res@tmXBLabelFontHeightF   =  0.01
   res@tmXBLabelJust          = "CenterRight"
   res@tmXBLabelDeltaF        =  1.0
   res@tmXBLabelAngleF        =  50.
   res@tmLabelAutoStride      =  True

   plot = gsn_csm_xy(wks, time, fldmean, res)

 end
