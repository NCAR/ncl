;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm16n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Tue Dec  1 15:32:40 MST 1998
;
;   Description:  Two-dimensional approximation and second
;                 order mixed partial using csa2s and csa2xs.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;
;  Create the input arrays.
;
  xmin = -1.
  xmax =  1.
  ymin = -1.
  ymax =  1.

  nx = 29
  ny = 25

  ndata = 1000
  xi = new(ndata,float)
  yi = new(ndata,float)
  zi = new(ndata,float)

;
;  Generate input data using the function f(x,y) = y**2 - 0.5*y*x**2
;
  do i=0,ndata-1
      xi(i) = xmin + (xmax-xmin)*rand()/32767.
      yi(i) = ymin + (ymax-ymin)*rand()/32767.
      zi(i) = yi(i)*yi(i) - 0.5*xi(i)*xi(i)*yi(i)
  end do

;
;  Set up the output grid.
;
  xo = fspan(xmin,xmax,nx)
  yo = fspan(ymin,ymax,ny)

  knots = (/4,4/)
;
;  Calculate the approximated functuion values.
;
  func = csa2s(xi,yi,zi,knots,xo,yo)

;
;  Calculate the approximated second order mixed partial.
;
  wts = (/-1./)
  smth = 0.0
  nderiv = (/1,1/)
  funcd = csa2xs(xi,yi,zi,wts,knots,smth,nderiv,xo,yo)

;
; Create workstation object.
;
  NCGM=1
  X11=0
  PS=0
 
  if (NCGM .eq. 1) then
    wid = gsn_open_wks("ncgm","nm16n")
  end if
  if (X11 .eq. 1) then
    wid = gsn_open_wks("x11","nm16n")
  end if
  if (PS .eq. 1) then
    wid = gsn_open_wks("ps","nm16n")
  end if

;
;  Draw plot of approximated function.
;
  rho = 2.7
  theta = 45.
  phi = 78.
  tdez2d(wid, xo, yo, func, rho, theta, phi, 6)
  txres = True
  txres@txFontHeightF = 0.04
  gsn_text_ndc(wid,                                                         \
     ":F25:z = f(x,y) = y:S:2:E:  - -:H-10::S:1:E::B::V-6:2:E:  y:V-6:*:V+6:x:S:2:E:",  \
     0.5,0.85,txres)
  frame(wid)

;
;  Draw plot of second order mixed partial.
;
  tdez2d(wid, xo, yo, funcd, rho, theta, phi, 6)
  gsn_text_ndc(wid,                                      \
    ":F25:z =  :F34::S::H8:6:F25::S:2:E::E::F34:>:B::F34::H-35::V-6:6:F25:x:F34:6:F25:y:E:  f(x,y) = - x",                               \
    0.5,0.85,txres)
  frame(wid)

end
