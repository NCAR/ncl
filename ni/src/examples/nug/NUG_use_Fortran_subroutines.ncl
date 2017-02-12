;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_use_Fortran_subroutines.ncl
;
; In order for this example to work, you must get the "ex01.f" 
; file from http://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml
; and run:
;
; WRAPIT ex01.f
;
; to generate the "ex01.so" file.
;-----------------------------------------------------------------
external EX01 "./ex01.so"
 
begin
   print("")
;-- Calculate three values of a quadratic equation
   nump = 3
   x    = (/ -1., 0.0, 1.0 /)
   qval = new(nump,float)
;-- Call the NCL version of your Fortran subroutine.     
   EX01::cquad(-1., 2., 3., nump, x, qval)
   print("Polynomial value = " + qval)     ;-- should be (/0,3,4/)
;-- Calculate an arc length.
   xc = (/ 0., 1., 2. /)
   yc = (/ 0., 1., 0. /)
;-- Call the NCL version of your Fortran function.
   arclen = EX01::arcln(nump,xc,yc)
   print("Arc length = " + arclen)         ;-- should be 2.82843
   print("")
end
