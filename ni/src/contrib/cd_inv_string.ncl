;;;;   cd_inv_string.ncl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A. Brammer
;;;  Adapted from cd_string to work in reverse
;;;  2015
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to do ;
;; - add month abbrev. recognition. 

;;;; Take a string and a format string and convert to a cd_time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; '%' is formatted according to the rule:
; Y => 4-digit year (e.g., 2007).
; y => 2-digit year (e.g., 07).    ;; this will be added to a base of 2000, unless format@century is defined.
; N => 2-digit month (e.g., 06).
; D => 2-digit day (e.g., 04).
; J => 3-digit day-of-year (e.g., 091)
;; j => 1, 2, or 3 digit day-of-year (e.g., 4, 91, or 181)
; H => 2-digit hour (e.g., 09).
;; h => 1 or 2 digit hour (e.g., 9 or 11).
; M => 2 digit minute (e.g., 08).
;; m => 1 or 2 digit minute (e.g., 7 or 56).
; S => 2 digit second (e.g., 02).
;; s => 1 or 2 digit second (e.g., 2 or 23).
; f => 2 digit fractional second (e.g. 25 = 15 seconds)

;;  abrammer 
;; Variable options above are not available as I'm not sure how to parse them automatically. 


; e.g.
; instring = "GFS_140621_18_000"
; format = "GFS_%y%N%D_%H_000"
; print(cd_calendar((cd_inv_string(instring, format)),0))
; format@century = 1900
; format@units = "hours since 1900-01-01 00:00:00"
; print(cd_calendar((cd_inv_string(instring, format)),0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef("cd_inv_string")
function cd_inv_string(instring, informat)
local months, fmonths, century, time, year, month, day, hour, minute,\
    second, doy, inchars, strm, c, n, pm_code, monday, units, format, informat
begin
months = (/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
fmonths = (/"","January","February","March","April","May","June","July","August","September","October","November","December"/)

format = informat  ;; copy so it isn't changed. 
century = get_res_value(format, "century", 2000)
units = get_res_value(format, "units", "hours since 1800-01-01 00:00:00")

time = new( dimsizes(instring), double)
do n=0, dimsizes(instring)-1
   year = 2000
   month = 01
   day = 01
   hour = 00
   minute = 00
   second = 00
   doy = 101
   doy@used = False
    
    if(dimsizes(format).eq.dimsizes(instring))
      inchars = tochar(format(n)) 
    else
      inchars = tochar(format(0))
    end if
    strm = 0
    do c=0, dimsizes(inchars)-1
        if(inchars(c) .ne. "%")
          strm = strm+ 1
          continue
        else
           c= c+1
           pm_code = inchars(c)

           if(pm_code .eq. "Y")
               year = toint(str_get_cols(instring(n), strm, strm+3 ) )
               strm = strm+4
           end if
           if(pm_code .eq. "y")
               year = toint(str_get_cols(instring(n), strm, strm+1 ) ) + century
               strm = strm+2
           end if
           if(pm_code .eq. "N")
               month = toint(str_get_cols(instring(n), strm, strm+1 ) )
               strm = strm+2
           end if
           if(pm_code .eq. "D")
               day = toint(str_get_cols(instring(n), strm, strm+1 ) )
               strm = strm+2
           end if
           if(pm_code .eq. "H")
               hour = toint(str_get_cols(instring(n), strm, strm+1 ) )
               strm = strm+2
           end if
           if(pm_code .eq. "M")
               minute = toint(str_get_cols(instring(n), strm, strm+1 ) )
               strm = strm+2
           end if
           if(pm_code .eq. "S")
               second = toint(str_get_cols(instring(n), strm, strm+1 ) )
               strm = strm+2
           end if
	   if(pm_code .eq. "f")
               second = toint(str_get_cols(instring(n), strm, strm+1 ) )*0.6 ;;; *100/60
               strm = strm+2
	   end if
           if(pm_code .eq. "J")
               doy = toint(str_get_cols(instring(n), strm, strm+2 ) )
               doy@used = True
               strm = strm+2
           end if
        end if
    end do
    if(doy@used)    ;  put it at the end so doy can come before a year is defined
     monday = monthday(year, doy)
     month = toint(monday)/100
     day = toint( toint(monday) - (month*100) )
    end if
    time(n) = cd_inv_calendar(year, month, day, hour, minute, second, units,0)
end do
return(time)
end

