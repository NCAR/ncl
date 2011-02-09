load "$NCARGTEST/nclscripts/functions.ncl"

;----------------------------------------------------------------------
; This script is for testing ind and num on large arrays that won't
; contain missing values.
;
; THIS SCRIPT IS NOT COMPLETE. I PUT IT IN SVN SO I COULD MOVE IT TO
; ANOTHER SYSTEM.
;
;----------------------------------------------------------------------
;
;  2^63-1 = 9223372036854775807
;  2^31-1 = 2147483647
;

;begin
  DEBUG     = True

  i_2_31    = tolong(2^31)     ; make a long so it prints better
  i_2_31_m1 = i_2_31-1
  i_2_63_m1 = tolong(2^63)-1

  int_dims = (/8,8,8,8,8,8,8,8,8,8/)   ; The product is < 2Gb
  lng_dims = (/8,8,8,8,8,8,8,8,8,16l/) ; The product is == 2Gb

  if(DEBUG) then
    print("==================================================")
    print("2^31   =            " + i_2_31)
    print("2^31-1 =            " + i_2_31_m1)
    print("2^63-1 =            " + i_2_63_m1)
    print("typeof(int_dims)  = " + typeof(int_dims))
    print("typeof(lng_dims)  = " + typeof(lng_dims))
    print("product(int_dims) = " + product(int_dims))
    print("product(lng_dims) = " + product(lng_dims))
    print("==================================================")
  end if

  i_2_31_m1 = tolong(2^31)-1   ; make a long so it prints better
  i_2_63_m1 = tolong(2^63)-1

;---Create a 1D array with exactly 2Gb-1 total elements
  XBYTE_1D          = new(i_2_31_m1,byte,"No_FillValue")
  XBYTE_1D          = 1b
  XBYTE_1D_IND      = ind(XBYTE_1D.eq.1)
  ii = ispan(0,dimsizes(XBYTE_1D)-1,1)
  check_values("ind","(2Gb-1 int array)",XBYTE_1D_IND,ii,True,False,0)
  delete(XBYTE_1D)
  delete(XBYTE_1D_IND)

;---Create a 1D array with exactly 2Gb total elements
  XLONG_1D          = new(i_2_31,byte,"No_FillValue")
  XLONG_1D          = 3
  XLONG_1D_IND      = ind(XLONG_1D.eq.3)
  check_values("ind","(2Gb long array)",XLONG_1D_IND,ii,True,False,0)
  delete(XLONG_1D)
  delete(XLONG_1D_IND)
  delete(ii)

;---Create an nD array with < 2Gb total elements
  XBYTE_MD          = new(int_dims,byte,"No_FillValue")
  XBYTE_MD          = 2
  XBYTE_MD_IND      = ind(ndtooned(XBYTE_MD).eq.2)
  ii = ispan(0,product(dimsizes(XBYTE_MD))-1,1)

  check_values("ind","(< 2Gb int multi-d array)",XBYTE_MD_IND,ii,True,False,0)
  delete(XBYTE_MD)
  delete(XBYTE_MD_IND)
  delete(ii)

;---Create an nD array with exactly 2Gb total elements
  XBYTE_MD         = new(lng_dims,byte,"No_FillValue")  ; Won't work with short (?)
                                         ; Not enough memory on Mac?
  XBYTE_MD          = 0
  XBYTE_MD_IND      = ind(ndtooned(XBYTE_MD).eq.0)
  ii = ispan(0,product(dimsizes(XBYTE_MD))-1,1)
  check_values("ind","(2Gb byte multi-d array)",XBYTE_MD_IND, ii,True,False,0)

  delete(XBYTE_MD)
  delete(XBYTE_MD_IND)
;end
