;-----------------------------------------------------------------------------
;
; calendar_decode2_fix - this is identical to calendar_decode2, except
; it calls the ut_calendar_fix routine that was added in NCL V6.4.0
; to address some problems with seconds sometimes being equal to 60.
; We are still testing ut_calendar_fix, so we are not yet ready to
; have calendar_decode2 call it yet.
; 
; See calendar_decode2.ncl for a full description.
;----------------------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
undef("calendar_decode2_fix")
function calendar_decode2_fix (time:numeric, option:integer)

local base_time_coord, base_time_days, bases, cal, cal_flag, chr, \
   d1, d2, date_c, db, doff, dout, ho, hours, i, ibase, idims, \
   igap, isp, len2, m, mb, mcross, mins, mlens, mmo, mo, moffset, \
   multiplier, multipliers, nidims, nstrings, odims, opt_round, \
   out, p1, p2, since, so, space, tdate, tdays, time_c, \
   uatt, uc, unit, unsupported, yb, ylen, ymd, yo

begin

;----------------------------------------
; Determine type of calendar.
;----------------------------------------

; If Gregorian calendar is selected, then pass all control to NCL's
; ut_calendar_fix routine for translation.

   cal_flag = isatt (time, "calendar")

   if (cal_flag) then
      cal = str_lower (time@calendar)	; make calendar attrib CASE INSENSITIVE
      if (cal .eq. "gregorian" .or. cal .eq. "standard") then
         return (ut_calendar_fix (time, option))
      end if
   end if

; Check for unsupported custom calendar attributes.

   unsupported = (/ "month_lengths", "leap_year", "leap_month" /)
   uatt = isatt (time, unsupported)

   if (any (uatt)) then
      print ("calendar_decode2_fix: FATAL: Custom calendar attribute(s) found,")
      print ("   but not currently supported:")
      print ("   " + oneDtostring (unsupported (ind (uatt))) )
      exit
   end if

; If calendar attribute is missing, and no custom calendar atributes are
; present, then default to Gregorian calendar; pass control to ut_calendar_fix.

   if (.not. cal_flag) then
      return (ut_calendar_fix (time, option))
   end if

; Check for supported alternative calendars, and set configuration.

   if (cal .eq. "365_day" .or. cal .eq. "noleap") then
      ylen = 365
      mlens = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
   else

   if (cal .eq. "366_day" .or. cal .eq. "all_leap") then
      ylen = 366
      mlens = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
   else

   if (cal .eq. "360_day") then
      ylen = 360
      mlens = (/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
   else

   if (cal .eq. "proleptic_gregorian") then	; special for this case only:
      ylen = 366				; params just for limit checks
      mlens = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

   else
      print ("calendar_decode2_fix: FATAL: Unsupported calendar system:")
      print ("   time@calendar = '" + time@calendar + "'")
      exit
   end if
   
   end if		; end "if case" block
   end if
   end if
   
;----------------------------------------
; Parse the time units string.
;----------------------------------------

; Get the units string.

   if (.not. isatt (time, "units")) then
      print ("calendar_decode2_fix: FATAL: Time units attribute is missing.")
      exit
   end if
   
   if (dimsizes (time@units) .ne. 1) then
      print ("calendar_decode2_fix: FATAL: Time units attribute is not scalar.")
      exit
   end if
   
   if (.not. isstring (time@units)) then
      print ("calendar_decode2_fix: FATAL: Time units attribute is not a string.")
      exit
   end if
   
   uc = stringtochar (" " + str_lower (time@units) + " x ")
   					; pad on both ends to facilitate parsing

; Define character constants.

   chr   = stringtochar(" ")			; space character
   space = chr(0)

; Parse out the main substrings in the units string.

; Example: "days since yyyy-mm-dd hh:mm:ss.s zzz"

; Numerics are all variable length.
; Leading and trailing spaces are allowed.
; Internal multiple spaces are allowed.
; Time of day substring is currently ignored.
; Time zone suffix is currently ignored.
; Trailing garbage is not allowed.
   
   isp = ind (uc .eq. space)		; find all spaces in padded unit string
   len2 = dimsizes (isp)
   igap = ind (isp(0:len2-2)+1 .ne. isp(1:len2-1))   ; find non-blank substrings
   nstrings = dimsizes (igap) - 1	; number of strings, excluding "x" pad
   
   if (nstrings .lt. 3 .or. nstrings .gt. 5) then
      print ("calendar_decode2_fix: FATAL: Invalid time units string.")
      exit
   end if

   p1 = isp(igap(0:3)) + 1			; start of each substring
   p2 = isp(igap(0:3)+1) - 1			; end of each substring
   
   unit   = chartostring (uc(p1(0):p2(0)))
   since  = chartostring (uc(p1(1):p2(1)))
   date_c = uc(p1(2):p2(2))
   time_c = uc(p1(3):p2(3))
   
   delete (p1)					; prevent conflicts
   delete (p2)

; Basic diagnostics.

   if (since .ne. "since") then
      print ("calendar_decode2_fix: FATAL: Missing 'since' in time units string.")
      exit
   end if

; Parse the date.

   ymd = parse_date_ymd (date_c, "-")
   delete (ymd@_FillValue)
   
   yb = ymd(0)
   mb = ymd(1)
   db = ymd(2)

   if (isatt (ymd, "error")) then
      print ("calendar_decode2_fix: FATAL: Invalid date in time units string.")
      exit
   end if
   
   delete (ymd)				; prevent conflicts

;--------------------------------------------
; Special handling for proleptic_gregorian.
;--------------------------------------------

   if (cal .eq. "proleptic_gregorian") then
      return (cal_proleptic (time, option, yb))	   ; call special handler
   end if

; Now check requested output option for all other calendars.

   if (option .lt. -5 .or. option .eq. -4 .or. option .gt. 4) then
      print ("calendar_decode2_fix: FATAL: Option " + option \
         + " is not suported for calendar type: " + time@calendar)
      exit
   end if

;--------------------------------------------
; Validate and interpret the units string.
;--------------------------------------------

; Validate the base unit.

   bases = (/ "day", "days", "hour", "hours", "minute", "minutes", \
      "second", "seconds" /)
   multipliers = (/ 1, 1, 24, 24, 1440, 1440, 86400, 86400 /)
   						; number of units in one day

   ibase = ind (unit .eq. bases)		; look up base unit in table
   
   if (ismissing (ibase)) then
      print ("calendar_decode2_fix: FATAL: Unsupported base unit in time units" \
         + " string.")
      exit
   end if
   
   multiplier = multipliers(ibase)

; Supplemental validation of the base date.

   if (db .gt. mlens(mb-1)) then
      print ("calendar_decode2_fix: FATAL: Invalid day of month in time units" \
         + " string.")
      exit
   end if

; *** ASSUME TIME STRING IS ZERO IN THIS VERSION.  DO NOT CHECK. ***

; Parse the optional time string.
; Must be zero (00:00:00) in this version.
;
;   if (nstrings .ge. 4) then
;
;      hms = parse_time (time_c)
;
;      if (isatt (hms, "error")) then
;         print ("calendar_decode2_fix: FATAL: Invalid time in time units string.")
;         exit
;      end if
;
;      if (abs (hms) .gt. 0.01) then
;         print ("calendar_decode2_fix: FATAL: Nonzero time in time units string" \
;            + " is not supported.")
;         exit
;      end if
;   
;   end if

; *** CHECK FOR UNSUPPORTED TIME ZONE STRING. ***
   
   if (nstrings .eq. 5) then
      print ("calendar_decode2_fix: FATAL: Time zone in time units string")
      print ("   is not supported.")
      exit
   end if

;----------------------------------------
; Compute the numeric base time.
;----------------------------------------

; Compute offsets from Jan 1 to start of each month, for fixed size years.

   moffset = new (12, integer)
   moffset(0) = 0
   
   do i = 1, 11
      moffset(i) = moffset(i-1) + mlens(i-1)
   end do

; Compute the coordinate offset from ZERO TIME, for the specified time base.
; Note: ZERO TIME for non-real calendars = -0001-Jan-01 00:00:00.

; All base time math must be coerced to double precision!

   base_time_days = int2dble (yb) * ylen + moffset(mb-1) + (db - 1)
   base_time_coord = base_time_days * multiplier

;----------------------------------------
; Translate the time coordinate array.
;----------------------------------------

; This should work for time arrays of any dimension.
; Negative year numbers are supported here.

   tdays = (time + base_time_coord) / multiplier  ; abs time from zero, in days

; Output option 4, YYYY.fraction_of_year as type double.

   if (option .eq. 4) then
      return (tdays / ylen)			; convert to year and fraction
   end if

   tdate = floor (tdays)			; date portion, in days
   yo    = floor (tdate / ylen)			; year number
   doff  = doubletoint (tdate - yo * ylen)	; day offset within year, int

; Construct an array to cross reference days of year to month numbers.
; This is the most efficient method when the array to translate is large.
   
   mcross = new (ylen, integer)
   d1 = 0				; first day of first month
   
   do m = 1, 12				; for each month...
      d2 = d1 + mlens(m-1) - 1		; last day of current month
      mcross(d1:d2) = m			; fill month num. for all days in month
      d1 = d2 + 1			; go to first day of next month
   end do

; Now use this array to get month numbers for the outputs.

   mo = mcross(doff)			; month numbers, 1-12
   dout = doff - moffset(mo-1) + 1	; day of month, 1-31

; Decode the time of day.

   hours = (tdays - tdate) * 24.	; hours plus fraction
   ho    = floor (hours)		; hours only
   mins  = (hours - ho) * 60.		; minutes plus fraction
   mmo   = floor (mins)			; minutes only
   so    = (mins - mmo) * 60.		; seconds plus fraction
   
;----------------------------------------
; Construct the result array.
;----------------------------------------

; Options 0 and -5, separate values for year, month, day, hour, minute, second.

   if (option .eq. 0 .or. option .eq. -5) then
      idims = dimsizes (yo)
      nidims = dimsizes (idims)
      odims = new (nidims + 1, integer)	; add a new dimension on the right
      odims(0:nidims-1) = dimsizes (yo)
      odims(nidims) = 6

; Here is the only restriction to one dimension, options 0 and -5 only.
; (2006-nov-30)
   
      out = new (odims, float)
      out(:,0) = doubletofloat (yo)
      out(:,1) = mo
      out(:,2) = dout
      out(:,3) = doubletofloat (ho)
      out(:,4) = doubletofloat (mmo)
      out(:,5) = doubletofloat (so)
      
      if (option .eq. 0) then
         return (out)			; option 0, return floats
      else
         return (floattoint (out))	; option -5, return integers
      end if				; seconds are truncated
   end if

; Options 1-3 and -1 to -3, decimal coded numbers.

; Omitted remainders are truncated in all of these options.
; Caution, option -3 overflows in year 2147 and later.

; There are no restrictions on dimensionality for these options.

   if (abs (option) .eq. 1) then			; YYYYMM
      out = yo*100 + mo
   else
   
   if (abs (option) .eq. 2) then			; YYYYMMDD
      out = yo*10000 + mo*100 + dout
   else

   if (abs (option) .eq. 3) then			; YYYYMMDDHH
      out = yo*1000000 + mo*10000 + dout*100 + ho
   end if
   end if
   end if

   if (option .gt. 0) then		; sign of option selects output type
      return (out)			; positive = select doubles
   else
      return (doubletoint (out))	; negative = select integers
   end if

; Option 4 is in the previous section.

end			; end function calendar_decode2_fix
