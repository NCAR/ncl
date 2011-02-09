load  "$NCARGTEST/nclscripts/functions.ncl"

;
; Tests for large (> INT32_MAX) types of length input to fspan.
;
; This test is not complete. We need to test for different length
; types (int64, long, int).
;

begin 
  int64_2_63_m1 = toint64(2^63)-1        ; 9223372036854775807
  l_2_31        = tolong(2^31)           ;          2147483648
  i_2_31_m1     = toint(l_2_31 - 1)      ;          2147483647
  int64_2_31_x2 = l_2_31 * 2q            ;          4294967296

;---Test semi-large input to "fspan"
  x = fspan( -1000000, 1000000, l_2_31)

  dimsx = dimsizes(x)
  printVarSummary(x)
  printVarSummary(dimsx)

  check_values("fspan","typeof(dimsizes(x))",typeof(dimsx),"long",True,False,0)
  check_values("fspan","long length",dimsx,l_2_31,True,False,0)
  check_values("fspan","min",min(x),-1000000,True,False,0)
  check_values("fspan","max",max(x), 1000000,True,False,0)

  delete(x)
  delete(dimsx)

;
; Haven't finished this test yet.
;
  x = fspan(1e1,1e10,int64_2_31_x2)
  dimsx = dimsizes(x)
  printVarSummary(x)
  printVarSummary(dimsx)

end
