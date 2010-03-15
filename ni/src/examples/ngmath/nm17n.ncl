;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm17n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Wed Dec  2 17:17:40 MST 1998
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
  ny = 21
  nz = 21

  ndata = 1000
  xi    = xmin + (xmax-xmin) * random_uniform(0.,1.0,ndata)
  yi    = ymin + (ymax-ymin) * random_uniform(0.,1.0,ndata)
  zi    = zmin + (zmax-zmin) * random_uniform(0.,1.0,ndata)
  ui    = xi*xi + yi*yi + zi*zi

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
  wid = gsn_open_wks(wks_type,"nm17n")

;
;  Draw an isosurface plot of the approximated function at isovalue = 3.
;
  rho = 2.
  theta = -35.
  phi = 65.
  tdez3d(wid, xo, yo, zo, uo, 3.0, rho, theta, phi, 6)
  frame(wid)

end
