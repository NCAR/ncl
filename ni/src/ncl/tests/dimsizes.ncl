load "$NCARGTEST/nclscripts/functions.ncl"

;----------------------------------------------------------------------
; This script is for playing around with dimsizes.
;----------------------------------------------------------------------
;
;  2^63-1 = 9223372036854775807
;  2^31-1 = 2147483647
;

begin
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
  XINT_1D          = new(i_2_31_m1,byte)
  dimsizes_XINT_1D = dimsizes(XINT_1D)
  check_values("dimsizes","typeof(dimsizes_XINT_1D)",\
               typeof(dimsizes_XINT_1D),"integer",True,False,0)
  check_values("dimsizes","dimsizes_XINT_1D",dimsizes_XINT_1D,i_2_31_m1,\
               True,False,0)
  delete(XINT_1D)

;---Create a 1D array with exactly 2Gb total elements
  XLONG_1D          = new(i_2_31,byte)
  dimsizes_XLONG_1D = dimsizes(XLONG_1D)
  check_values("dimsizes","typeof(dimsizes_XLONG_1D)",\
               typeof(dimsizes_XLONG_1D),"long",True,False,0)
  check_values("dimsizes","dimsizes_XLONG_1D",dimsizes_XLONG_1D,i_2_31,\
               True,False,0)
  delete(XLONG_1D)

;---Create an nD array with < 2Gb total elements
  XINT_MD          = new(int_dims,byte)
  dimsizes_XINT_MD = dimsizes(XINT_MD)
  check_values("dimsizes","typeof(dimsizes_XINT_MD)",\
               typeof(dimsizes_XINT_MD),"integer",True,False,0)
  check_values("dimsizes","dimsizes_XINT_MD",dimsizes_XINT_MD,int_dims,\
               True,False,0)
  delete(XINT_MD)

;---Create an nD array with exactly 2Gb total elements
  XLONG_MD          = new(lng_dims,byte)  ; Won't work with short (?)
                                          ; Not enough memory on Mac?
  dimsizes_XLONG_MD = dimsizes(XLONG_MD)
  check_values("dimsizes","typeof(dimsizes_XLONG_MD)",\
               typeof(dimsizes_XLONG_MD),"long",True,False,0)
  check_values("dimsizes","dimsizes_XLONG_MD",dimsizes_XLONG_MD,lng_dims, \
               True,False,0)
  delete(XLONG_MD)
end
