;-----------------------------------------------------------------------------
;
; calendar_decode2 -- Translate numeric time coordinates to calendar times.
;
; This is an alternate for NCL's ut_calendar function.  Two
; additional CF calendar systems are supported that are not in
; the current version of the built-in ut_calendar.  This
; version has some limitations and extra diagnostics.
;
; 1.00	2006-nov-30	Original version.  By Dave Allured.
;			Add partial support for CF-1.0 calendar systems.
; 1.01	2008-jul-17	Improve number diagnostics in time units string.
;			Comments added for proleptic_gregorian, but no code yet.
; 1.02	2008-sep-09	Add support for proleptic_gregorian calendar type.
;
; 1.03	2010-jan-19	Add support for remaining ut_calendar output options.
;			Replace deprecated changeCase function.
;			NCL version 5.1.1 or later is required.
;			Fix dimension mismatch bug in cal_proleptic.
;
; 1.04	2017-feb-17	Replace ut_calendar with cd_calendar. This fixes
;			the minute/second issue, in some cases, where
;			you'd get "2000,12,31,18,59,60" instead of
;			"2000,12,31,19,0,0".

; Usage:  result = calendar_decode2 (time, option)
;	  Same interface as for ut_calendar built-in function.
;	  See NCL documentation for ut_calendar:
;
;	  www.ncl.ucar.edu/Document/Functions/Built-in/ut_calendar.shtml
;
; Input:  time = an array of numeric time coordinates in one of the
;	  supported calendar systems.  See dimension restrictions below.
;
;	  time@units = Udunits-style time reference string, scalar.
;	  E.g. "days since 1800-1-1 00:00:00".  The attribute name
;	  "units" must be lower case.  The string value is case
;	  insensitive.  The units attribute is required.
;
;	  time@calendar = name of calendar system, string scalar.
;	  E.g. "gregorian".  The attribute name "calendar" must be
;	  lower case.  The string value is case insensitive.  The
;	  calendar attribute is optional.  If missing, Gregorian
;	  is implied.
;
;	  option = an integer specifying the format of the result
;	  array.
;
; Output: result = an array of translated date and time values
;	  equivalent to the input time coordinates.  The format of
;	  the output array is selected by the "option" parameter.
;
;	  option = 0 or -5:  The result array will be of type float
;	  (option 0) or integer (option -5), with one added
;	  dimension on the right:
;
;	     result(:,0) --> years
;	     result(:,1) --> months
;	     result(:,2) --> days
;	     result(:,3) --> hours
;	     result(:,4) --> minutes
;	     result(:,5) --> seconds
;
;	  Other options:  The result array will have the same
;	  dimensions as the input array.  For a description of the
;	  other formats, see NCL documentation for ut_calendar.
;
; Calendar systems:
;
; This version contains partial support for several calendar systems
; described in CF-1 conventions.  The COARDS convention is also
; supported, as it uses the Gregorian calendar by default.  This is
; the definitive reference for CF-1:
;
;    cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/ch04s04.html
;    cf-pcmdi.llnl.gov
;
; Supported calendar systems:
;
;    gregorian or standard
;	Mixed Gregorian/Julian calendar as defined by UDUNITS.
;       This is the default when the "calendar" attribute is missing.
;	See caution below.
;
;    proleptic_gregorian
;	Gregorian calendar extended to dates before 1582-10-15.
;
;    noleap or 365_day
;	Gregorian calendar with no leap years.  Every year has 365 days.
;
;    all_leap or 366_day
;	Gregorian calendar with every year being a leap year.  Every
;	year has 366 days.
;
;    360_day
;	All years are 360 days divided into 30 day months.
;
; Calendar systems in CF-1.4, not currently supported here:
;
;    julian
;    "none"
;
; Custom calendar attributes in CF-1.4, not currently supported here:
;
;    month_lengths
;    leap_year
;    leap_month
;
; Caution:  When using the "gregorian" calendar system, the UDUNITS
; mixed Gregorian/Julian interpretation is used.
;
;    www.unidata.ucar.edu/packages/udunits
;
; For the mixed Gregorian/Julian calendar, it is strongly recommended
; that all encoded dates as well as the reference date in the units
; string be greater than or equal to October 15, 1582.  This is the
; valid start date for the contemporary interval of the Gregorian
; calendar system.
;
; Restrictions in this version:
;
; When calendar = "gregorian" or equivalent, this routine defers to
; NCL's ut_calendar function and includes all of its functionality.
;
; This also applies for "proleptic_gregorian" or equivalent, but only
; when all dates including the time unit string are "safe" dates later
; than 1582.
;
; The following apply when any of the other supported calendars are used:
;
; * Options:
;	For proleptic_gregorian calendar with any dates 1852 or
;	earlier, only output options 0 and -5 are supported.
;
; * Dimensions:
;	For output options 0 and -5, all calendars other than
;	gregorian and proleptic_gregorian, the input time variable
;	must be scalar or a one dimensional array.
;	For all other output options, there is no restriction on
;       dimensionality.
;
; * Units string syntax must be one of these three:
;	uuuu since y-m-d
;	uuuu since y-m-d h:m:s
;	uuuu since y-m-d h:m:s.s
;
;   For example:
;	days since 1800-1-1
;	days since 1800-01-01 0:0:0
;	days since 1800-01-01 00:00:0.000
;
; * Base unit for time (uuuu) must be one of these:
;	day or days
;	hour or hours
;	minute or minutes
;	second or seconds
;
; * The base year must be zero or positive.
;
; * Time of day in the units string is NOT CHECKED and MUST BE ZERO.
;
; * (LATER: Time of day in the units string is restricted to zero only.)
;
; * The time zone suffix in the units string is not supported and must
;   not be included.
;
; * The global "conventions" attribute is not checked for consistency
;   with calendar attributes encountered.
;
; * Type and dimension checking of calendar attributes is minimal.
;
; General notes:
;
; Following typical conventions, attribute names are CASE SENSITIVE.
;
; String attribute values are CASE INSENSITIVE.
;
; Organization:
;
; University of Colorado, CIRES Climate Diagnostics Center (CDC)
; NOAA/ESRL/PSD, Climate Analysis Branch (CAB)
;
; Custom library dependencies:
;
;	find_substring.ncl
;	substring.ncl
;
;----------------------------------------------------------------------------


; Support functions start here.

;----------------------------------------------------------------------------
; parse_date_ymd -- Parse date in yyyy-mm-dd format into three numbers.
;----------------------------------------------------------------------------
;
; Input:  instr = "yyyy-mm-dd" in a 1D character array.  One date only.
;	  	No leading, trailing, or embedded spaces.
;	  	All three numbers are variable length.
;	  delim_str = delimiter character, as a string.
;
; Output: result = year, month, day in an integer array.
;	  result@error = defined if error found; no attribute if date is valid.
;
; Note:   Only coarse range checking is done.  Month must be 1 to 12;
; 	  day must be 1 to 31.  Year may be any non-negative integer.
;
;----------------------------------------------------------------------------

undef("parse_date_ymd")
function parse_date_ymd (instr:character, delim_str:string)

local chr, d, delim, i, id, lengths, m, out, p, p1, p2, valid

begin

   chr   = stringtochar(delim_str)		; get delimiter as character
   delim = chr(0)
   
   out = new (3, integer)			; make empty result array

; Parse out the elements of the date.
   
   id = ind (instr .eq. delim)			; find all delimiters
   
   if (dimsizes (id) .ne. 2 ) then		; require exactly 2 delimiters
      print ("parse_date_ymd: Incorrect number of delimiters.")
      print ("date = [" + chartostring (instr) + "]")
      out@error = True
      return (out)
   end if
   
   p1 = (/ -1, id(0), id(1) /) + 1			; start pointers
   p2 = (/ id(0), id(1), dimsizes (instr) /) - 1	; end pointers
   lengths = (p2 - p1) + 1				; lengths of substrings
   
   if (any (lengths .lt. 1)) then	; require at least 1 digit in each num.
      print ("parse_date_ymd: Missing number in date.")
      print ("date = [" + chartostring (instr) + "]")
      out@error = True
      return (out)
   end if

; Check for valid integers.

   valid = stringtochar ((/ "-0123456789" /))
   				; must include dashes when checking full string
				; remember to exclude null byte in usage
   
   do p = 0, dimsizes (instr) - 1
      if (.not. any (instr(p) .eq. valid(0:10))) then
         print ("parse_date_ymd: Invalid character in date.")
         print ("date = [" + chartostring (instr) + "]")
         print ("date(" + (p+1) + ") = [" + instr(p) + "]")
         out@error = True
         return (out)
      end if
   end do

; Convert number strings to numbers.

; CAUTION:  2008-JUL-17:
; LEADING ZEROS ARE HANDLED OKAY IN NCL 5.0.1, BUT ONLY BY ACCIDENT.
; IF THE BEHAVIOR OF STRINGTOINT() IS CHANGED, THEN STRIPPING OF
; LEADING ZEROS MUST BE ADDED.

   do i = 0, 2
      out(i) = stringtoint (chartostring (instr (p1(i):p2(i))))
   end do

; Check month and day.

   m = out(1)
   d = out(2)
   
   if (m .lt. 1 .or. m .gt. 12 .or. d .lt. 1 .or. d .gt. 31) then
      print ("parse_date_ymd: Invalid month or day number.")
      print ("date = [" + chartostring (instr) + "]")
      out@error = True
   end if
   
   return (out)

end		; end function parse_date_ymd


;----------------------------------------------------------------------------
; cal_proleptic -- Special handling for proleptic_gregorian calendar.
;----------------------------------------------------------------------------
;
; Input:  time = caller's time coordinate array, same as for main function.
;	     See calendar_decode2 input arguments above.
;	  option = caller's option value, same as for main function.
;	  base_year = integer base year, already parsed from units string.
;
; Output: result = calendar output array, same as for main function.
;	     See calendar_decode2 output arguments above.
;
; Notes:  This is an internal support function only, not for
; independent usage.  This routine depends on initial processing
; from the first part of calendar_decode2 for the following purposes:
;
; * Initial validation of the caller's input arguments.
; * Recognize and call this function only for calendar = proleptic_gregorian.
; * Parse out the base year from the input time units string.
;
; Restrictions for this subroutine:
;
; * If any dates are 1852 or earlier, then only NCL options 0 and -5
;   are supported (Y-M-D output array).
;
; Capabilities:
;
; * Time coordinate arrays of any rank are supported in all cases.
;
; Strategy:  If needed, offset the base year in the time units string
; by multiples of 400, then let ut_calendar translate the time
; coordinates as if for normal Gregorian.  Then remove the times-400
; offset from the year numbers in the output array.
;
; Before entry, we have done only minimal validation of elements
; of the time units string.  This is so that we can use as much of
; the native functionality of ut_calendar as possible, in all cases.
;
;----------------------------------------------------------------------------

undef("cal_proleptic")
function cal_proleptic (time:numeric, option:integer, base_year:integer)

local blocks_needed, dash, deficit, greg_offset, min_coord_year, \
   min_time, min_year, n_elements, offset_base_year, out, \
   remainder, sp1, tcopy, tdims, transition_year, units1, units2, \
   units_lc, ymd_1d, ymd_min, ymd_mod

begin
   transition_year = 1582		; Gregorian calendar started here;
					; distrust all date math here and before
   
   tcopy = time				; make copy; protect caller's array
   
   delete (tcopy@calendar)		; delete this attrib to make ut_calendar
					; think it's handling normal Gregorian

; Check the earliest date used in either units string or time coordinates.

   min_time = min (tcopy)		; get earliest date in the coord array
   min_time@units = tcopy@units

   ymd_min = cd_calendar (min_time, -5) ; get earliest year number; -5 = integer
   min_coord_year = ymd_min(0,0)

   min_year = min ((/ base_year, min_coord_year /))
					; earliest year in units string or array

; If all are later than transition year, just revert to normal ut_calendar.

   if (min_year .gt. transition_year) then   ; translate with forced Gregorian;
      out = cd_calendar (tcopy, option)      ; allow all NCL options
      
      if (isatt (out, "calendar")) then		; remove misleading attribute
         delete (out@calendar)			; in this case
      end if
      
      return (out)				; return normal array
   end if

; Some dates earlier than 1582 -- hacking will be required!
; Now check the option restriction.

   if (option .ne. 0 .and. option .ne. -5) then
      print ("calendar_decode2: FATAL: Option " + option \
         + " is not suported for calendar type: " + tcopy@calendar)
      print ("when any year is 1582 or earlier.")
      exit
   end if

; Compute the times-400 offset needed to make all dates greater than 1582.

   deficit = transition_year - min_year + 1	; both must be integers
   blocks_needed = (deficit + 399) / 400	; integer divide with truncation
						; should always be 1 or greater
   greg_offset = blocks_needed * 400		; number of years to offset

; Offset the time units string to make all date math later than 1582.

; Because we want to be very conservative and keep intact as much of
; the original units string as possible, some re-parsing is needed.

   units_lc = str_lower (tcopy@units)		; make case insensitive
   sp1 = find_substring (units_lc, " since ")	; find space before "since"
   remainder = substring (units_lc, sp1+7, 0)	; find dash following "since "
   dash = find_substring (remainder, "-")

   if (sp1 .lt. 1 .or. dash .lt. 1) then
      print ("calendar_decode2: FATAL: Format problem in time units string.")
      exit
   end if

   units1 = substring (tcopy@units, 0, sp1+6)	    ; first part through
   						    ; first space after "since"
   units2 = substring (tcopy@units, sp1+7+dash, 0)  ; last part from 
   						    ; first dash through end

   offset_base_year = base_year + greg_offset	; advance base year by 400's
   tcopy@units = units1 + offset_base_year + units2
					; insert new base year into units string

; Finally, translate the time coordinates using the modified units string.

; Use ut_calendar to preserve all possible functionality.
; But only output options 0 and -5 are supported at this time,
; because of the need to readjust the output years.

   ymd_mod = cd_calendar (tcopy, option)	; time s/b double up to here;
						; function works for any rank
      
   if (isatt (ymd_mod, "calendar")) then	; remove misleading attribute
      delete (ymd_mod@calendar)			; in this case
   end if

; Now make the reverse adjustment to all of the dates in the output array.
; Support arrays of any rank.

   tdims = dimsizes (ymd_mod)		; save original dimensions
   ymd_1d = ndtooned (ymd_mod)		; must convert to 1-D to support
					; original dimensionality

   n_elements = dimsizes (ymd_1d)	; scalar, total number of elements
   ymd_1d(0:n_elements-6:6) = ymd_1d(0:n_elements-6:6) - greg_offset
				; remove X 400 offset from YEAR NUMBERS ONLY

   return (onedtond (ymd_1d, tdims))	; reshape to original dimensions
					; and output N-dimensional result array

end			; end function cal_proleptic


;----------------------------------------------------------------------------
; Main function calendar_decode2.
;----------------------------------------------------------------------------

undef("calendar_decode2")
function calendar_decode2 (time:numeric, option:integer)

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
; ut_calendar routine for translation.

   cal_flag = isatt (time, "calendar")

   if (cal_flag) then
      cal = str_lower (time@calendar)	; make calendar attrib CASE INSENSITIVE
      if (cal .eq. "gregorian" .or. cal .eq. "standard") then
         return (cd_calendar (time, option))
      end if
   end if

; Check for unsupported custom calendar attributes.

   unsupported = (/ "month_lengths", "leap_year", "leap_month" /)
   uatt = isatt (time, unsupported)

   if (any (uatt)) then
      print ("calendar_decode2: FATAL: Custom calendar attribute(s) found,")
      print ("   but not currently supported:")
      print ("   " + oneDtostring (unsupported (ind (uatt))) )
      exit
   end if

; If calendar attribute is missing, and no custom calendar atributes are
; present, then default to Gregorian calendar; pass control to ut_calendar.

   if (.not. cal_flag) then
      return (cd_calendar (time, option))
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
      print ("calendar_decode2: FATAL: Unsupported calendar system:")
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
      print ("calendar_decode2: FATAL: Time units attribute is missing.")
      exit
   end if
   
   if (dimsizes (time@units) .ne. 1) then
      print ("calendar_decode2: FATAL: Time units attribute is not scalar.")
      exit
   end if
   
   if (.not. isstring (time@units)) then
      print ("calendar_decode2: FATAL: Time units attribute is not a string.")
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
      print ("calendar_decode2: FATAL: Invalid time units string.")
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
      print ("calendar_decode2: FATAL: Missing 'since' in time units string.")
      exit
   end if

; Parse the date.

   ymd = parse_date_ymd (date_c, "-")
   delete (ymd@_FillValue)
   
   yb = ymd(0)
   mb = ymd(1)
   db = ymd(2)

   if (isatt (ymd, "error")) then
      print ("calendar_decode2: FATAL: Invalid date in time units string.")
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
      print ("calendar_decode2: FATAL: Option " + option \
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
      print ("calendar_decode2: FATAL: Unsupported base unit in time units" \
         + " string.")
      exit
   end if
   
   multiplier = multipliers(ibase)

; Supplemental validation of the base date.

   if (db .gt. mlens(mb-1)) then
      print ("calendar_decode2: FATAL: Invalid day of month in time units" \
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
;         print ("calendar_decode2: FATAL: Invalid time in time units string.")
;         exit
;      end if
;
;      if (abs (hms) .gt. 0.01) then
;         print ("calendar_decode2: FATAL: Nonzero time in time units string" \
;            + " is not supported.")
;         exit
;      end if
;   
;   end if

; *** CHECK FOR UNSUPPORTED TIME ZONE STRING. ***
   
   if (nstrings .eq. 5) then
      print ("calendar_decode2: FATAL: Time zone in time units string")
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

end			; end function calendar_decode2
