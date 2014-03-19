;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm01n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 15 10:38:27 MST 1997
;
;   Description: Simple example of natural neighbor linear interpolation.
;

begin
  ISLIM = 6
  NumXOut = 21
  NumYOut = 21
;
; Define the input data arrays.
;
  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
;
; Define the output grid.
;
  xc = 1./(NumXOut-1.) 
  xo = ispan(0,NumXOut-1,1) * xc

  yc = 1./(NumYOut-1.) 
  yo = ispan(0,NumYOut-1,1) * yc
;
; Do the gridding.
;
  out = natgrids(x, y, z, xo, yo)
;
; Create a workstation object.
;
  wks_type = "ncgm"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "natgrids" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm01n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "natgrids" windowWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "natgrids" psWorkstationClass defaultapp
      "wkPSFileName" : "nm01n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "natgrids" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "nm01n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "natgrids" documentWorkstationClass defaultapp
      "wkFileName" : "nm01n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "natgrids" imageWorkstationClass defaultapp
      "wkFileName" : "nm01n"
      "wkFormat" : wks_type
    end create
  end if
;
; Retrieve the GKS workstation ID so we can use low-level calls to
; draw the surface.
;
  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues
;
; Draw the surface.
;
  drwsrfc(gkswid,xo,yo,out,15.,-25.,90.)
;
; Show how to do the above, only using double precision.
; First we need to remove our float variables and redeclare 
; them as double precision.
;
  delete(x)
  delete(y)
  delete(z)
  delete(xo)
  delete(yo)
  delete(out)
;
; Create double precision variables.
;
  x = new((/ISLIM/),double)
  y = new((/ISLIM/),double)
  z = new((/ISLIM/),double)
  xo = new((/NumXOut/),double)
  yo = new((/NumXOut/),double)

  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
;
; Define the output grid.
;
  xc = 1./(NumXOut-1.) 
  xo = ispan(0,NumXOut-1,1) * xc

  yc = 1./(NumYOut-1.) 
  yo = ispan(0,NumYOut-1,1) * yc
;
; Do the gridding.
;
  out = natgridd(x, y, z, xo, yo)
;
; Draw the surface.
;
  xp = doubletofloat(xo)
  yp = doubletofloat(yo)
  outp = doubletofloat(out)
    
  drwsrfc(gkswid,xp,yp,outp,15.,-25.,90.)

  frame(wid)
end

