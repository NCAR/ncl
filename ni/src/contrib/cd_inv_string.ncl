;;;;   cd_inv_string.ncl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A. Brammer
;;;  Adapted from cd_string to work in reverse
;;;  2018
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to do ;
;; -  

;;;; Take a string and a format string and convert to a cd_time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; '%' is formatted according to the rule:
; Y => 4-digit year (e.g., 2007).
; y => 2-digit year (e.g., 07).    ;; this will be added to a base of 2000, unless format@century is defined.
; N => 2-digit month (e.g., 06).
; D => 2-digit day (e.g., 04).
; J => 3-digit day-of-year (e.g., 091)
;;;;;;Not Available;;;;;;;;;;;;;;;;; j => 1, 2, or 3 digit day-of-year (e.g., 4, 91, or 181)
; H => 2-digit hour (e.g., 09).
;;;;;;Not Available;;;;;;;;;;;;;;;;; h => 1 or 2 digit hour (e.g., 9 or 11).
; M => 2 digit minute (e.g., 08).
;;;;;;Not Available;;;;;;;;;;;;;;;;; m => 1 or 2 digit minute (e.g., 7 or 56).
; S => 2 digit second (e.g., 02).
;;;;;;Not Available;;;;;;;;;;;;;;;;; s => 1 or 2 digit second (e.g., 2 or 23).
; r => 2 digit fractional second (e.g. 25 = 15 seconds) ;; changed from f  in 2018
; c => 3 character Month case insensitive   (e.g. jun)
; C => 3 character month case insensitive   (e.g. JUN)
; f => Full Month name  case insensitive    (e.g. january)
; F => Full Month name case insensitive     (e.g. JANUARY)


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
function cd_inv_string(instring, format)
local months, fmonths, century, time, year, month, day, hour, minute,\
    second, doy, inchars, strm, c, n, pm_code, monday, units, format, informat,\
    month_str, m
begin
months = (/"","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"/)
fmonths = (/"","january","february","march","april","may","june","july","august","september","october","november","december"/)

century = get_res_value_keep(format, "century", 2000)
units = get_res_value_keep(format, "units", "hours since 1800-01-01 00:00:00")

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
           if(any( pm_code .eq. (/"c","C"/) ))
               month_str := str_lower( str_get_cols(instring(n), strm, strm+2 ) )
               month := ind( month_str .eq. months)
               strm = strm+3
           end if
           if(any( pm_code .eq. (/"f","F"/) ))
               do m=1,12
                   month_str := str_lower( str_get_cols(instring(n), strm, strm+strlen(fmonths(m))-1 ) ) ; we can overshoot with str_get_cols
                    if( month_str .eq. fmonths(m) )
                        month = m
                        strm = strm+strlen(fmonths(m))
                        break  ; save some loops at break out once we've found it
                    end if
                end do
           end if
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
           if(pm_code .eq. "r")
               second := toint(str_get_cols(instring(n), strm, strm+1 ) )*0.6 ;;; *100/60 ; assignment type bug fix 2018
               strm = strm+2
           end if
           if(pm_code .eq. "J")
               doy = toint(str_get_cols(instring(n), strm, strm+2 ) )
               doy@used = True
               strm = strm+3
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

