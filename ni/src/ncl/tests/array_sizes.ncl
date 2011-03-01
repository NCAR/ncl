;----------------------------------------------------------------------
; This script is for playing around with creating arrays of
; different sizes (for testing the 64-bit overhaul code).
;----------------------------------------------------------------------

begin
  version       = get_ncl_version()
  major_version = toint(str_get_field(version,1,"."))

  print("==================================================")
  print("NCL version = '" + version + "'")
  print("NCL major version = " + major_version)
  print("==================================================")

;
; "i_2_31_m1" is the largest value that can be held in an int type.
; In NCL V5.x, this is the largest total size that an array can be.
;
; "i_2_63_m1" is the largest value that can be held in a long type.
; In NCL V6.x, this is the largest total size that an array can be.
;
; Of course, you are limited by the memory on your machine.
;
  i_2_63_m1    = tolong(2^63)-1         ; 9223372036854775807
  i_2_31       = tolong(2^31)           ;          2147483648
  i_2_31_m1    = i_2_31 - 1             ;          2147483647
  i_2_31_m1_d2 = i_2_31_m1 / 2          ;          1073741823
  i_2_31_m1_d4 = i_2_31_m1 / 4          ;           536870911

  print("i_2_63_m1    = (2^63)-1     = " + i_2_63_m1)
  print("i_2_31       = (2^31)       = " + i_2_31)
  print("i_2_31_m1    = (2^31)-1     = " + i_2_31_m1)
  print("i_2_31_m1_d2 = ((2^31)-1)/2 = " + i_2_31_m1_d2)
  print("i_2_31_m1_d4 = ((2^31)-1)/4 = " + i_2_31_m1_d4)

  print("==================================================")

;---Set "size" to one of the above i_xxxx variables.
  size = i_2_31_m1_d4

  print("Size is " + size + " bytes, and major version is " + major_version)

  if(size.eq.i_2_31_m1) then
    if(major_version.le.5) then
      print("so allocating a byte array...")
      b = new(size,byte)
    else
      print("so allocating a byte array of one size larger...")
      b = new(size+1,byte)   ; This would fail under V5.x
    end if
    printVarSummary(b)
  end if

  if(size.ge.i_2_31_m1_d2.and.size.lt.i_2_31_m1) then
    if(major_version.le.5) then
      print("so allocating a short array...")
      s = new(size,short)
    else
      print("so allocating a short array of one size larger...")
     s = new(size+1,short)      ; This would fail under NCL 5.x
    end if
    printVarSummary(s)
  end if

  if(size.ge.i_2_31_m1_d4.and.size.lt.i_2_31_m1_d2) then
    if(major_version.le.5) then
      print("so allocating a float array...")
      f = new(size,float)
    else
      print("so allocating a float array of one size larger...")
      f = new(size+1,float)
    end if
    printVarSummary(f)
  end if
end
