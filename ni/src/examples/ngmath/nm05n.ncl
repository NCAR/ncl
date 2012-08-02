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

wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "dsgrid" ncgmWorkstationClass defaultapp
      "wkMetaName" : "nm05n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "dsgrid" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "dsgrid" psWorkstationClass defaultapp
    "wkPSFileName" : "nm05n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "dsgrid" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm05n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "dsgrid" documentWorkstationClass defaultapp
    "wkFileName" : "nm05n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "dsgrid" imageWorkstationClass defaultapp
    "wkFileName" : "nm05n"
    "wkFormat" : wks_type
  end create
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
