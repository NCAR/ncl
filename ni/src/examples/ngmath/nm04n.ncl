;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm04n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Tue Dec 16 10:18:13 MST 1997
;
;   Description: Simple 3D interpolation.
;

begin

NUM = 1000
NX = 21
NY = 21
NZ = 21
RAND_MAX = 32767.0

xi = new((/NUM/),float)
yi = new((/NUM/),float)
zi = new((/NUM/),float)
u = new((/NUM/),float)

xo = new((/NX/),float)
yo = new((/NY/),float)
zo = new((/NZ/),float)

xmin = -2.0
ymin = -2.0
zmin = -2.0
xmax =  2.0
ymax =  2.0
zmax =  2.0

;
; Create random data in three space and define a function.
;
rand1 = new((/NUM/),float)
rand2 = new((/NUM/),float)
rand3 = new((/NUM/),float)
srand(1)
do i=0,NUM-1
  rand1(i) = rand
  rand2(i) = rand
  rand3(i) = rand
end do
xi = xmin+(xmax-xmin)*(rand1 / RAND_MAX)
yi = ymin+(ymax-ymin)*(rand2 / RAND_MAX)
zi = zmin+(zmax-zmin)*(rand3 / RAND_MAX)

u = xi*xi + yi*yi + zi*zi
;
; Create the output grid.
;
ii = fspan(0,20.,21)
xo = xmin + (ii/(NX-1)) * (xmax-xmin)
yo = ymin + (ii/(NY-1)) * (ymax-ymin)
zo = zmin + (ii/(NZ-1)) * (zmax-zmin)

;
; Interpolate.
;
output = dsgrid3s(xi, yi, zi, u, xo, yo, zo)

NCGM=1
X11=0
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "dsgrid" ncgmWorkstationClass defaultapp
      "wkMetaName" : "nm04n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    wid = create "dsgrid" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      wid = create "dsgrid" psWorkstationClass defaultapp
        "wkPSFileName" : "nm04n.ps"
      end create
    end if
  end if
end if

;output!0 = "i"
;output!1 = "j"
;output!2 = "k"
tdez3d(wid,xo,yo,zo,output,3.,2.,-35.,65.,6)
frame(wid)

end
