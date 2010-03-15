;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm19n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Fri Dec  4 12:25:37 MST 1998
;
;   Description:  This program illustrates the use of csa2ls.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;
;  Create the input arrays.
;
xmin = -1.4
xmax =  1.4
ymin = -1.2
ymax =  1.2

ndata = 500

xi = xmin + (xmax-xmin)*random_uniform(0.,1.,ndata)
yi = ymin + (ymax-ymin)*random_uniform(0.,1.,ndata)
t1 = 1.0/((fabs(xi-0.1))^2.75 + fabs(yi)^2.75 + 0.09)
t2 = 1.0/((fabs(xi+0.1))^2.75 + fabs(yi)^2.75 + 0.09)
zi = 0.3 * ((xi + yi) + t1 - t2)

delete(t1)
delete(t2)


;
;  Set up a list of output points that corresponds to a 2D grid.
;
nx = 29
ny = 25
no = nx*ny
xo = new(no,float)
yo = new(no,float)
scalex = 1./(nx-1)*(xmax-xmin)
do i=0,nx-1
  istart = i*ny
  iend   = istart+ny-1
  xo(istart:iend) = xmin + i*scalex
  yo(istart:iend) = fspan(ymin,ymax,ny)
end do

;
;  Find the approximation surface.
;
knots = (/10,10/)
zo = csa2ls(xi,yi,zi,knots,xo,yo)

;
;  Convert to a 2D array for plotting.
;
zo2 = onedtond(zo,(/nx,ny/))

;
; Create workstation object.
;
  wks_type = "ncgm"
  wid = gsn_open_wks(wks_type,"nm19n")

;
;  Draw plot.
;
rho = 2.5
theta = -154.
phi = 80.
xo2 = fspan(xmin,xmax,nx)
yo2 = fspan(ymin,ymax,ny)
tdez2d(wid, xo2, yo2, zo2, rho, theta, phi, 6)
frame(wid)

end
