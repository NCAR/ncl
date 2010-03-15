;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm18n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Thu Dec  3 12:41:12 MST 1998
;
;   Description:  This program illustrates the use of csa3s.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;
;  Create the input arrays.
;
  xmin = -2.
  xmax =  2.
  ymin = -2.
  ymax =  2.
  zmin = -2.
  zmax =  2.

  nx = 21
  ny = 31
  nz = 41

  ndata = 1000
  xi    = xmin + (xmax-xmin) * random_uniform(0.,1.0,ndata)
  yi    = ymin + (ymax-ymin) * random_uniform(0.,1.0,ndata)
  zi    = zmin + (zmax-zmin) * random_uniform(0.,1.0,ndata)
  ui    = 0.75*xi*xi - 1.6*yi*yi + 2.*zi*zi

;
;  Set up the output grid.
;
  xo = fspan(xmin,xmax,nx)
  yo = fspan(ymin,ymax,ny)
  zo = fspan(zmin,zmax,nz)

;
;  Interpolate.
;
  knots = (/4,4,4/)
  uo = csa3s(xi,yi,zi,ui,knots,xo,yo,zo)

;
; Create workstation object.
;
  wks_type = "ncgm"
  wid = gsn_open_wks(wks_type,"nm18n")

;
;  Draw plot of approximated function.
;
  rho = 2.3
  theta = -13.
  phi = 75.
  tdez3d(wid, xo, yo, zo, uo, 0.7, rho, theta, phi, 6)
  frame(wid)

end
