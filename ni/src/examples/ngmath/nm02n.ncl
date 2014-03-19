;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm02n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 15 10:48:08 MST 1997
;
;   Description:   Simple example of natural neighbor linear regridding.
;
begin
  NUMXOUT = 21
  NUMYOUT = 21

  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)

  xc = 1./(NUMXOUT-1.) 
  xi = ispan(0,NUMXOUT-1,1) * xc

  yc = 1./(NUMYOUT-1.) 
  yi = ispan(0,NUMYOUT-1,1) * yc

  nnsetp("igr",1)
  zi = natgrids(x, y, z, xi, yi)

  wks_type = "ncgm"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "natgrids" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm02n.ncgm"
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
      "wkPSFileName" : "nm02n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "natgrids" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "nm02n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "natgrids" documentWorkstationClass defaultapp
      "wkFileName" : "nm02n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "natgrids" imageWorkstationClass defaultapp
      "wkFileName" : "nm02n"
      "wkFormat" : wks_type
    end create
  end if

  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues

  drwsrfc(gkswid,xi,yi,zi,15.,-25.,90.)

  frame(wid)
end

