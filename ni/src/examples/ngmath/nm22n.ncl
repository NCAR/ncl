;
;      $Id: nm22n.ncl,v 1.5 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                    Copyright (C)  1999                               ;
;            University Corporation for Atmospheric Research           ;
;                    All Rights Reserved                               ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:         nm22n.ncl
;
;  Author:       Fred Clare
;                National Center for Atmospheric Research
;                PO 3000, Boulder, Colorado
;
;  Date:         Thu Sep  9 10:39:59 MDT 1999
;
;  Description:  This program illustrates the use of shgrid.
;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

;
; Main program.
;
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
  fval  = 0.75*xi*xi - 1.6*yi*yi + 2.*zi*zi
  
;
;  Set up the output grid.
;
  xo = fspan(xmin,xmax,nx)
  yo = fspan(ymin,ymax,ny)
  zo = fspan(zmin,zmax,nz)

;
;  Interpolate.
;
  uo = shgrid(xi,yi,zi,fval,xo,yo,zo)

;
; Create workstation object.
;
  wks_type = "ncgm"
  wid = gsn_open_wks(wks_type,"nm22n")

;
;  Draw plot of approximated function.
;
  rho = 2.3
  theta = -13.
  phi = 75.
  tdez3d(wid, xo, yo, zo, uo, 0.7, rho, theta, phi, 6)
  frame(wid)

;
; End NCL script.
;
end
