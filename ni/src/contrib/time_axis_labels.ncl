;=======================================================================
undef("time_axis_labels")
procedure time_axis_labels(time:numeric,resplot:logical,restick:logical)
; Purpose: Draw tickmark labels in the format of date/time on an axis.
;
; Arguments:
;  time: The variable containing the values of time. This variable should
;        have an attribute named "units". In other words, this variable
;        holds the values of the coordinate axis 'time' along with the units.
;
;  resplot: The general plot resources. This variable should then be used
;           in the function to plot. It can have any other attributes, as
;           usual. Only tmZZMode, tmZZValues and tmZZLabels are changed
;           inside this procedure (where ZZ is one of XB, XT, YL or YR
;           depending on which axis to mark).
;
;  restick: Resources for the time axis tick formats. If this variable is
;           true then its attributes are used to format the time axis.
;           Valid attributes are:
;           ttmFormat: a string specifying the format of the tick labels.
;                   This string is parsed as follows: the '%' acts as
;                   the escape character. The single character after every
;                   '%' is formatted according to the rule:
;                   Y => 4-digit year (e.g., 2007).
;                   y => 2-digit year (e.g., 07).
;                   C => CAPITAL month abbreviation (e.g., JUN).
;                   c => Small month abbreviation (e.g., Jun).
;                   F => CAPITAL full month (e.g., JUNE).
;                   f => Small full month (e.g., June).
;                   N => 2-digit month (e.g., 06).
;                   n => 1 or 2 digit month (e.g., 6 for June, 12 for December).
;                   D => 2-digit day (e.g., 04).
;                   d => 1 or 2 digit day (e.g., 4)
;                   H => 2-digit hour (e.g., 09).
;                   h => 1 or 2 digit hour (e.g., 9 or 11).
;                   M => 2 digit minute (e.g., 08).
;                   m => 1 or 2 digit minute (e.g., 07 or 56).
;                   S => 2 digit second (e.g., 02).
;                   s => 1 or 2 digit second (e.g., 2 or 23).
;                  Any character at any other place in the format string
;                   is drawn as is.
;                  NOTE: a '%' can be drawn using "%%".
;                  In case ttmFormat is absent, a minimal algorithm exists
;                   which tries to determine the format string depending
;                   on the length and values of the date-time.
;
;           ttmAxis: string. Can have values "XB", "XT", "YL", or "YR"
;                  depending on which axis is to label. Default is "XB".
;
;           ttmValues: integer. An Mx6 array containing M date-time values where
;                   tick labels should be marked. The 2nd dimension should
;                   contain the values of year, month, day, hour, minute
;                   and second in this order.
;                   If this attribute is missing, the script tries to
;                   determine the axis values for tick from the length
;                   of the data.
;
;           ttmNumTicks: Approximate number of tick labels. Used only when
;                   ttmValues is absent. Default value of this parameter is 4.
;
;           ttmMajorStride: Number timesteps in data between Major ticks. Note 
;                   that this trumps ttmNumTicks but not ttmValues.
;
;           ttmMinorStride: Number timesteps in data between Minor ticks

;
; Side effects:
;  Values of tmZZMode, tmZZValues and tmZZLabels (ZZ = one of XB, XT, YL, YR)
;   are set. Also resplot is set to True inside this procedure.
;
; Examples:
;  time = ... ; the time variable
;  y = ... ; variable to plot
;  resplot@... = ... ; general resources variable.
;
;  #1
;  resplot = True
;  restick = True
;  restick@ttmFormat = "%c%y"
;  time_axis_labels(time,resplot,restick) ; call the formating procedure
;  plot = gsn_csm_xy(wks,time,y,resplot) ; will produce bottom x-axis ticks
;         as CccYy (Apr98, May98 etc).
;
;  #2
;  resplot = True
;  restick = True
;  restick@ttmFormat = "%N/%Y"
;  restick@ttmAxis = "YL"
;  time_axis_labels(time,resplot,restick)
;  plot = gsn_csm_xy(wks,y,time,resplot) ; will produce left y-axis ticks
;         as NN/YYYY (04/1998, 05/1998 etc).
;
;  #3
;  resplot = True
;  restick = True
;  restick@ttmValues = (/(/1998,4,16,0,0,0/), \
;                     (/1998,6,16,0,0,0/), \
;                     (/1998,8,16,0,0,0/), \
;                     (/1998,10,16,0,0,0/), \
;                     (/1998,12,16,0,0,0/), \
;                     (/1999,2,16,0,0,0/)/)
;  time_axis_labels(time,resplot,restick)
;  plot = gsn_csm_xy(wks,time,y,resplot) ; will produce bottom x-axis ticks
;         at Apr 16, 1998, June 16, 1998 etc. ttmFormat will be determined
;         in the procedure. Specify ttmFormat attribute to override the default.
;
; #4
; ...
; resplot@ttmFormat = "%H:%M:%S" will produce HH:MM:SS kind of time formatting.
; To specify month/day as well, use
; resplot@ttmFormat = "%N/%D %H:%M:%S"
;
; #5
; To write hour values as "HH:00 Hours" use
; resplot@ttmFormat = "%H:00 Hours"
;
; #6
; restick = True
; restick@ttmFormat = "%d %c"
; restick@ttmAxis = "YL"
; restick@ttmMajorStride = 20
; time_axis_labels( data&time,  res, restick )
; plot = gsn_csm_hov( wks, data, res ) ; will produce y-axis ticks on a 
;         Hovmueller. Labels will be plotted every 20th data timestep in the
;         format 29 Jun, 4 Jul, etc.
; 

; Author: Arindam Chakraborty <arch at caos.iisc.ernet.in>

; Feb 06, 2012: Mary changed "ut_calendar" and "ut_inv_calendar" to use
;               "cd_calendar" and "cd_inv_calendar" instead.

local plot, ymdhms0, taxisvalues, minorvalues, ymdhms1, ymdhms1i, format, months, fmonths, inside

begin

 months = (/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
 fmonths = (/"","January","February","March","April","May","June","July","August","September","October","November","December"/)

 ; check input
 if (.not. isatt(time,"units")) then
  print("time_axis_labels: The variable containing time values must have a 'units' attribute.")
  return
 end if

 resplot = True

 if (restick) then
  if (.not. isatt(restick,"ttmAxis")) then
   restick@ttmAxis = "XB"
  end if

  if (restick@ttmAxis .eq. "XB") then
   resplot@tmXBMode = "Explicit"
  end if
  if (restick@ttmAxis .eq. "XT") then
   resplot@tmXTMode = "Explicit"
  end if
  if (restick@ttmAxis .eq. "YL") then
   resplot@tmYLMode = "Explicit"
  end if
  if (restick@ttmAxis .eq. "YR") then
   resplot@tmYRMode = "Explicit"
  end if

  ymdhms0 = cd_calendar(time,0)
  ymdhms0dim = dimsizes(ymdhms0)
  nt = ymdhms0dim(0)

  if (.not. isatt(restick,"ttmValues")) then
   if (.not. isatt(restick,"ttmNumTicks")) then
    restick@ttmNumTicks = 4
   end if
   if(.not.isatt(restick,"ttmMajorStride")) then
    restick@ttmMajorStride = floattoint((nt-1)/restick@ttmNumTicks)
   end if
   ymdhms1 = ymdhms0(::restick@ttmMajorStride,:)
  else
   ymdhms1 = restick@ttmValues
  end if
  if (typeof(ymdhms1) .eq. "float") then
   ymdhms1i = floattointeger(ymdhms1)
  else
   ymdhms1i = ymdhms1
  end if
  delete(ymdhms1)

;---Check for a "calendar" attribute.
  if(isatt(time,"calendar"))
    opt_calendar = 0
    opt_calendar@calendar = time@calendar
  else
    opt_calendar = 0
  end if

  taxisvalues = cd_inv_calendar(ymdhms1i(:,0),ymdhms1i(:,1),ymdhms1i(:,2),ymdhms1i(:,3),ymdhms1i(:,4),ymdhms1i(:,5),time@units,opt_calendar)

  dimsz = dimsizes(ymdhms1i)
  ntick = dimsz(0)
  tticklabels = new(ntick,"string")

  if( ( .not.isatt(restick,"ttmMinorStride") ) \\
       .and.(isatt(restick,"ttmMajorStride") ) ) then
    restick@ttmMinorStride = max((/restick@ttmMajorStride / 5, 1/))
  end if

  if( isatt(restick,"ttmMinorStride") ) then
    minorvalues = time(::restick@ttmMinorStride)
  else
    minorvalues = taxisvalues
  end if

  if (restick@ttmAxis .eq. "XB") then
   resplot@tmXBValues = taxisvalues
   resplot@tmXBMinorValues = minorvalues
  end if
  if (restick@ttmAxis .eq. "XT") then
   resplot@tmXTValues = taxisvalues
   resplot@tmXTMinorValues = minorvalues
  end if
  if (restick@ttmAxis .eq. "YL") then
   resplot@tmYLValues = taxisvalues
   resplot@tmYLMinorValues = minorvalues
  end if
  if (restick@ttmAxis .eq. "YR") then
   resplot@tmYRValues = taxisvalues
   resplot@tmYRMinorValues = minorvalues
  end if

  if (.not. isatt(restick,"ttmFormat")) then
   if (ymdhms0(nt-1,0) .ne. ymdhms0(0,0)) then ; year changes
    if (abs(ymdhms0(nt-1,0)-ymdhms0(0,0)) .gt. ntick) then
     restick@ttmFormat = "%Y"
    else
     restick@ttmFormat = "%c%y"
    end if
   else ; same year
    if (ymdhms0(nt-1,1) .ne. ymdhms0(0,1)) then ; month changes
     if (abs(ymdhms0(nt-1,1)-ymdhms0(0,1)) .gt. ntick) then
      restick@ttmFormat = "%c%y"
     else
      restick@ttmFormat = "%N/%D"
     end if
    else ; same year/month
     if (ymdhms0(nt-1,2) .ne. ymdhms0(0,2)) then ; day changes
      if (abs(ymdhms0(nt-1,2)-ymdhms0(0,2)) .gt. ntick) then
       restick@ttmFormat = "%N/%D"
      else
       restick@ttmFormat = "%D, %H hrs"
      end if
     else ; same year/month/day
      if (ymdhms0(nt-1,3) .ne. ymdhms0(0,3)) then ; hour changes
       if (abs(ymdhms0(nt-1,3)-ymdhms0(0,3)) .gt. ntick) then
        restick@ttmFormat = "%D, %H:%M"
       else
        restick@ttmFormat = "%H:%M:%S"
       end if
      else ; same year/month/day/hour
       if (ymdhms0(nt-1,4) .ne. ymdhms0(0,4)) then ; minute changes
        if (abs(ymdhms0(nt-1,4)-ymdhms0(0,4)) .gt. ntick) then
         restick@ttmFormat = "%H:%M:%S"
        else
         restick@ttmFormat = "%H:%M:%S"
        end if
       else ; same year/month/day/hour/minute
        restick@ttmFormat = "%H:%M:%S"
       end if
      end if
     end if
    end if
   end if
  end if

  lenfmt = strlen(restick@ttmFormat)
  format = stringtochar(restick@ttmFormat)

  do i = 0, ntick-1
   tticklabels(i) = ""
   inside = False
   j = 0
   do while(j .lt. lenfmt)
    if (format(j) .eq. "%") then
     inside = True
     j = j + 1
    end if

    if (.not. inside) then
     tticklabels(i) = tticklabels(i)+format(j)
    else
     if (format(j) .eq. "Y") then
      tticklabels(i) = tticklabels(i)+sprinti("%04d",ymdhms1i(i,0))
      inside = False
     end if
     if (format(j) .eq. "y") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,0)%100)
;      tticklabels(i) = tticklabels(i)+sprinti("%02d",mod(ymdhms1i(i,0),100))
      inside = False
     end if
     if (format(j) .eq. "C") then
      tticklabels(i) = tticklabels(i)+str_upper(months(ymdhms1i(i,1)))
      inside = False
     end if
     if (format(j) .eq. "c") then
      tticklabels(i) = tticklabels(i)+months(ymdhms1i(i,1))
      inside = False
     end if
     if (format(j) .eq. "F") then
      tticklabels(i) = tticklabels(i)+str_upper(fmonths(ymdhms1i(i,1)))
      inside = False
     end if
     if (format(j) .eq. "f") then
      tticklabels(i) = tticklabels(i)+fmonths(ymdhms1i(i,1))
      inside = False
     end if
     if (format(j) .eq. "N") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,1))
      inside = False
     end if
     if (format(j) .eq. "n") then
      tticklabels(i) = tticklabels(i)+sprinti("%d",ymdhms1i(i,1))
      inside = False
     end if
     if (format(j) .eq. "D") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,2))
      inside = False
     end if
     if (format(j) .eq. "d") then
      tticklabels(i) = tticklabels(i)+sprinti("%d",ymdhms1i(i,2))
      inside = False
     end if
     if (format(j) .eq. "H") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,3))
      inside = False
     end if
     if (format(j) .eq. "h") then
      tticklabels(i) = tticklabels(i)+sprinti("%d",ymdhms1i(i,3))
      inside = False
     end if
     if (format(j) .eq. "M") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,4))
      inside = False
     end if
     if (format(j) .eq. "m") then
      tticklabels(i) = tticklabels(i)+sprinti("%d",ymdhms1i(i,4))
      inside = False
     end if
     if (format(j) .eq. "S") then
      tticklabels(i) = tticklabels(i)+sprinti("%02d",ymdhms1i(i,5))
      inside = False
     end if
     if (format(j) .eq. "s") then
      tticklabels(i) = tticklabels(i)+sprinti("%d",ymdhms1i(i,5))
      inside = False
     end if
     if (format(j) .eq. "%") then
      tticklabels(i) = tticklabels(i)+"%"
      inside = False
     end if
    end if

    j = j + 1
   end do

  end do

;  print(tticklabels)

  if (restick@ttmAxis .eq. "XB") then
   resplot@tmXBLabels = tticklabels
  end if
  if (restick@ttmAxis .eq. "XT") then
   resplot@tmXTLabels = tticklabels
  end if
  if (restick@ttmAxis .eq. "YL") then
   resplot@tmYLLabels = tticklabels
  end if
  if (restick@ttmAxis .eq. "YR") then
   resplot@tmYRLabels = tticklabels
  end if
 end if

 return
end
