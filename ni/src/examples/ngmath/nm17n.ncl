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
  xi = new(ndata,float)
  yi = new(ndata,float)
  zi = new(ndata,float)
  ui = new(ndata,float)

  do i=0,ndata-1
      xi(i) = xmin + (xmax-xmin)*rand()/32767.
      yi(i) = ymin + (ymax-ymin)*rand()/32767.
      zi(i) = zmin + (zmax-zmin)*rand()/32767.
      ui(i) = xi(i)*xi(i) + yi(i)*yi(i) + zi(i)*zi(i)
  end do

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

  NCGM=1
  X11=0
  PS=0
 
  if (NCGM .eq. 1) then
    wid = gsn_open_wks("ncgm","nm17n")
  end if
  if (X11 .eq. 1) then
    wid = gsn_open_wks("x11","nm17n")
  end if
  if (PS .eq. 1) then
    wid = gsn_open_wks("ps","nm17n")
  end if

;
;  Draw an isosurface plot of the approximated function at isovalue = 3.
;
  rho = 2.
  theta = -35.
  phi = 65.
  tdez3d(wid, xo, yo, zo, uo, 3.0, rho, theta, phi, 6)
  frame(wid)

end
