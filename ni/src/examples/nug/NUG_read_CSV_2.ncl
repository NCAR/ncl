;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_CSV_2.ncl
;
;  KMF
;-----------------------------------------------------------------
; These two load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/time_axis_labels.ncl"

begin
;-- input file name
  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "multiple_columns.csv"

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
  
;-- used delimiter in file for columns or strings (date)
  delim1 = ","
  delim2 = "/"
  delim3 = ":"

;-- read in file as array of strings so we can parse each line
  data   = asciiread(diri+fili, -1, "string")
  scount = str_fields_count(data(0),delim1)      ;-- get number of columns
 
;-- read variable AOD (500)
  var    = tofloat(str_get_field(data,29,delim1))

;-- read date and split it to year, month and day
  dat    = str_get_field(data,2,delim1)
  year   = toint(str_get_field(dat,3,delim2))
  month  = toint(str_get_field(dat,1,delim2))
  day    = toint(str_get_field(dat,2,delim2))

;-- read time and split it to hour, minutes and seconds
  tim    = str_get_field(data,3,delim1)
  hour   = toint(str_get_field(tim,1,delim3))
  minute = toint(str_get_field(tim,2,delim3))
  second = toint(str_get_field(tim,3,delim3))

;-- convert the UT-referenced time to a mixed Julian/Gregorian time
  units  = "hours since 2000-01-01 00:00:00"
  time2  =  cd_inv_calendar(year,month,day,hour,minute,second,units, 0)
  time2!0= "time"
  
;-- define the workstation (plot type and name)
  wks  = gsn_open_wks("png","NUG_read_CSV_2")

;-- set resources
  res                        =  True
  res@gsnMaximize            =  True       ;-- maximize the plot

  res@xyMarkLineModes        = "Markers"   ;-- use markers instead of lines
  res@xyMarkers              =  5          ;-- type of marker (cross)
  res@xyMarkerColor          = "red"       ;-- marker color
  res@xyMarkerSizeF          =  0.007      ;-- marker size

  res@vpXF                   =  0.25       ;-- viewport x-position
  res@vpYF                   =  0.6        ;-- viewport y-position
  res@vpWidthF               =  0.7        ;-- viewport width
  res@vpHeightF              =  0.37       ;-- viewport height

  res@tiMainString           = "AOD (500nm)" ;-- title string
  res@tiYAxisString          = "AOD"       ;-- y-axis string

  res@trYMinF                =  0.0        ;-- y-axis minimum value
  res@trYMaxF                =  0.6        ;-- y-axis maximum value
  
  res@tmXBLabelFontHeightF   =  0.01       ;-- x-axis label font size
  res@tmXBLabelJust          = "CenterRight" ;-- x-axis label justification
  res@tmXBLabelDeltaF        =  1.0        ;-- moves x-axis labels downward
  res@tmXBLabelAngleF        =  50.        ;-- rotate x-axis labels
  res@tmYROn                 =  False     ;-- no tick marks on right y-axis
  res@tmXTOn                 =  False      ;-- no tick marks on top x-axis

;-- set the time format for res
  restime           =  True
  restime@ttmFormat = "%d %C %h %m"
  time_axis_labels(time2,res,restime)
  
;-- create the plot
  plot = gsn_csm_xy(wks, time2, var, res)
end

