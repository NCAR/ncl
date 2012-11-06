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

;
;  Generate input data using the function f(x,y) = y**2 - 0.5*y*x**2
;
  ndata = 1000
  xi    = xmin + (xmax-xmin) * random_uniform(0.,1.0,ndata)
  yi    = ymin + (ymax-ymin) * random_uniform(0.,1.0,ndata)
  zi    = yi*yi - 0.5*xi*xi*xi

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
  wks_type = "ncgm"
  wid = gsn_open_wks(wks_type,"nm16n")

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
     "~F25~z = f(x,y) = y~S~2~E~  - -~H-10~~S~1~E~~B~~V-6~2~E~  y~V-6~*~V+6~x~S~2~E~",  \
     0.5,0.85,txres)
  frame(wid)

;
;  Draw plot of second order mixed partial.
;
  tdez2d(wid, xo, yo, funcd, rho, theta, phi, 6)
  gsn_text_ndc(wid,                                      \
    "~F25~z =  ~F34~~S~~H8~6~F25~~S~2~E~~E~~F34~>~B~~F34~~H-35~~V-6~6~F25~x~F34~6~F25~y~E~  f(x,y) = - x",                               \
    0.5,0.85,txres)
  frame(wid)

end
