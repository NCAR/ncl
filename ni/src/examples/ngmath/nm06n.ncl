;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm06n.ncl
;
;   Author:       Fred Clare (converted to NCL by Mary Haley)
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Tue Dec 16 10:29:03 MST 1997
;
;   Description:  Smoothing in a simple 2D interpolation.
;

begin

NUM = 171
NX = 21
NY = 21

RAND_MAX = 32767.0

xi = new((/NUM/),float)
yi = new((/NUM/),float)
zi = new((/NUM/),float)

xo = new((/NX/),float)
yo = new((/NY/),float)
output2 = new((/NX,NY/),float)

xminin = -0.2
yminin = -0.2
xmaxin = 1.2
ymaxin = 1.2
xminot =  0.0
yminot =  0.0
xmaxot = 1.0
ymaxot = 1.0

rho = 3.
theta = -54.
phi = 32.
;
; Create random data in three space and define a function.
;
rand1 = new((/NUM/),float)
rand2 = new((/NUM/),float)
srand(1)
do i=0,NUM-1
  rand1(i) = rand
  rand2(i) = rand
end do
xi = xminin+(xmaxin-xminin)*(rand1 / RAND_MAX)
yi = yminin+(ymaxin-yminin)*(rand2 / RAND_MAX)
zi = (xi-0.25)*(xi-0.25) + (yi-0.50)*(yi-0.50)

;
; Create the output grid.
;
ii = fspan(0.,NX-1,NX)
xo = xminot + ( ii / (NX-1)) * (xmaxot-xminot)
yo = yminot + ( ii / (NY-1)) * (ymaxot-yminot)

;
; Interpolate using both dsgrid2s and dspnt2s.
;

output1 = dsgrid2s(xi, yi, zi, xo, yo)

do i=0,NX-1
  do j=0,NY-1
    dspnt2s(xi, yi, zi, xo(i), yo(j), output2(i,j))
  end do
end do


wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  wid = create "dsgrid" ncgmWorkstationClass defaultapp
      "wkMetaName" : "nm06n.ncgm"
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
    "wkPSFileName" : "nm06n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "dsgrid" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "nm06n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "dsgrid" documentWorkstationClass defaultapp
    "wkFileName" : "nm06n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "dsgrid" imageWorkstationClass defaultapp
    "wkFileName" : "nm06n"
    "wkFormat" : wks_type
  end create
end if

tdez2d(wid, xo, yo, output1, rho, theta, phi, 6)
frame(wid)

tdez2d(wid, xo, yo, output2, rho, theta, phi, 6)
frame(wid)

end
