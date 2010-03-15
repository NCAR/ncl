;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm12n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Tue Jan 20 12:49:41 MST 1998
;
;   Description:  This program illustrates the use of ftsurf.
;

begin

;
;  Create the input arrays.
;
nxi = 11
nyi = 17

xi = fspan(0.,1.,nxi)
yi = fspan(0.,1.,nyi)
zi = new((/nxi,nyi/),float)
zi = 0.5 + 0.25  * (sin(-7.*conform(zi,xi,0)) + cos(5.*conform(zi,yi,1)))

;
;  Set up the output grid.
;
nxo = 31
nyo = 21
xo = fspan(0.,1.,nxo)
yo = fspan(0.,1.,nyo)

;
;  Interpolate.
;
zo = ftsurf(xi,yi,zi,xo,yo)


;
; Create workstation object.
;
  wks_type = "ncgm"
 
  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "nm12Work" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm12n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an XWorkstation object.
;
    wid = create "nm12Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  end if
  if (str_lower(wks_type).eq."ps") then
;
; Create a PSWorkstation object.
;
    wid = create "dsgrid" psWorkstationClass defaultapp
      "wkPSFileName" : "nm12n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."pdf") then
;
; Create a PDFWorkstation object.
;
    wid = create "dsgrid" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "nm12n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."newpdf".or.str_lower(wks_type).eq."newps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "dsgrid" documentWorkstationClass defaultapp
      "wkFileName" : "nm12n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."newpng".or.str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "dsgrid" imageWorkstationClass defaultapp
      "wkFileName" : "nm12n"
      "wkFormat" : wks_type
    end create
  end if
 
;
;  Draw plot.
;
rho = 3.
theta = 36.
phi = 67.
tdez2d(wid, xo, yo, zo, rho, theta, phi, -6)
frame(wid)

end
