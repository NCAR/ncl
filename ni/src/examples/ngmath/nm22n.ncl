;
;      $Id: nm22n.ncl,v 1.1 1999-09-09 16:45:06 fred Exp $
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
procedure nrand(first:integer, nextn:integer, result:float)
;
;  Portable random number generator.  This is here in place
;  of the built-in random number generator "rand"
;  so that the results for this example will be the same
;  as for the equivalent Fortran and C examples.
;
;  Arguments
;    first  - is 0 if this is the first call to the procedure, 1 otherwise
;    nextn  - a temporary storage variable that should be the same for
;             all calls.
;    result - the desired random number
;
local mplier, modlus, mobymp, momdmp, hvlue, lvlue, testv
begin
 
  mplier =      16807
  modlus = 2147483647
  mobymp =     127773
  momdmp =       2836
 
  if (first .eq. 0) then
    nextn = 9
  end if
 
  hvlue = nextn / mobymp
  lvlue = nextn%mobymp;
  testv = mplier*lvlue - momdmp*hvlue;
  if (testv .gt. 0) then
    nextn = testv;
  else
    nextn = testv + modlus;
  end if
 
  result = (1. * nextn) / (1. * modlus)
end

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
  xi = new(ndata,float)
  yi = new(ndata,float)
  zi = new(ndata,float)
  fval = new(ndata,float)
 
  do i=0,ndata-1
      xi(i) = xmin + (xmax-xmin)*rand()/32767.
      yi(i) = ymin + (ymax-ymin)*rand()/32767.
      zi(i) = zmin + (zmax-zmin)*rand()/32767.
      fval(i) = 0.75*xi(i)*xi(i) - 1.6*yi(i)*yi(i) + 2.*zi(i)*zi(i)
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
  uo = shgrid(xi,yi,zi,fval,xo,yo,zo)

;
; Create workstation object.
;
  NCGM=1
  X11=0
  PS=0
 
  if (NCGM .eq. 1) then
    wid = gsn_open_wks("ncgm","nm22n")
  end if
  if (X11 .eq. 1) then
    wid = gsn_open_wks("x11","nm22n")
  end if
  if (PS .eq. 1) then
    wid = gsn_open_wks("ps","nm22n")
  end if

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
