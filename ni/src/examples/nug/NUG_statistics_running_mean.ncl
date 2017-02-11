;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_statistics_running_mean.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 

begin

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "tas_mod1_rcp85_rectilin_grid_2D.nc"
  
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
  
  f    =  addfile(diri+fili,"r")
  var  =  f->tas(:,0,:,:)
  time =  f->time                      ;-- get time values 

;-- convert a mixed Julian/Gregorian date to a UT-referenced date
  utc_date =  cd_calendar(time, 0)
  year     =  tointeger(utc_date(:,0))
  month    =  tointeger(utc_date(:,1))
  day      =  tointeger(utc_date(:,2))
  date_str =  sprinti("%0.4i-", year)+sprinti("%0.2i-", month)+ \
              sprinti("%0.2i", day)

  x         =  time                      ;-- get time values 
  y         =  dim_avg_n_Wrap(dim_avg_n_Wrap(var,1),1) ;-- timeserie

;-- calculate the running mean
  y_rave    = runave_n_Wrap(y,10,0,0)    ;-- 10 time steps are included in 
                                         ;-- running average
  
;-- open workstation
  wks = gsn_open_wks("png","NUG_statistics_running_mean")

;-- set resources
  res                   =  True
  res@gsnDraw           =  False          ;-- don't draw plot yet
  res@gsnFrame          =  False          ;-- don't advance frame
  
  res@tiMainString      = "NCL Doc Example:  Running Mean"
  
  res@vpHeightF         =  0.4            ;-- viewport height
  res@vpWidthF          =  0.78           ;-- viewport width
  res@vpXF              =  0.13           ;-- viewport x start pos
  
  res@trXMinF           =  min(x)         ;-- x-axis min value
  res@trXMaxF           =  max(x)         ;-- x-axis max value
  res@trYMinF           =  min(y)         ;-- y-axis min value
  res@trYMaxF           =  max(y)         ;-- y-axis max value

  res@tmXBMode          = "Explicit"      ;-- use explicit x-axis values
  res@tmXBValues        =  time(::4)      ;-- x-axis values (every 4th)
  res@tmXBLabels        =  date_str(::4)  ;-- x-axis labels (every 4th)
  res@tmXBLabelJust     = "CenterRight"   ;-- x-axis label justification
  res@tmXBLabelDeltaF   =  0.15           ;-- move x-axis labels down
  res@tmXBLabelAngleF   =  45.            ;-- rotate x-axis labels
  res@tmXBLabelFontHeightF =  0.012       ;-- decrease font size

  res@xyMarkLineModes   = "Lines"         ;-- line mode

;-- create timeserie plot
  res@xyLineColor       = "black"         ;-- line color black
  plot1 = gsn_csm_xy(wks, x, y, res)
  
;-- create linear regression plot
  res@xyLineColor       = "blue"          ;-- line color blue
  plot2 = gsn_csm_xy(wks, x, y_rave, res)

  overlay(plot1,plot2)                    ;-- overlay plot2 on plot1

;-- draw the plot
  draw(plot1)
  frame(wks)

end
