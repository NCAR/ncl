;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm05n.ncl
;
;   Author:       Fred Clare (converted to NCL by Mary Haley)
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Tue Dec 16 10:18:13 MST 1997
;
;   Description: How to vary the exponent of the distances in a
;                2D interpolation.
;
begin

NUM = 6
NX = 61
NY = 61

xi = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
yi = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
zi = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
rho =  3.0
theta = -45.
phi =  55.

xo = new((/NX/),float)
yo = new((/NY/),float)

;
; Create the output grid.
;
xinc = 1./(NX-1) 
yinc = 1./(NY-1) 
ii = fspan(0.,60.,NX)
xo = xinc * ii
yo = yinc * ii
;
; Exponent equals 0.5
;
dssetp("exp", 0.5)
xexp = dsgetp("exp")
if (xexp.ne.0.5) then
  print("xexp should equal 0.5")
end if
output = dsgrid2s(xi, yi, zi, xo, yo)

NCGM=1
X11=0
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "dsgrid" ncgmWorkstationClass defaultapp
      "wkMetaName" : "nm05n.ncgm"
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
        "wkPSFileName" : "nm05n.ps"
      end create
    end if
  end if
end if

tdez2d(wid, xo, yo, output, rho, theta, phi, 6)
frame(wid)

;
; Exponent equals 1.0
;
dssetp("exp", 1.0)
output = dsgrid2s(xi, yi, zi, xo, yo )

tdez2d(wid, xo, yo, output, rho, theta, phi, 6)
frame(wid)

;
; Exponent equals 5.0
;
dssetp("exp", 5.0)
output = dsgrid2s(xi, yi, zi, xo, yo)

output!0 = "i"
output!1 = "j"
tdez2d(wid, xo, yo, output, rho, theta, phi, 6)
frame(wid)

end
