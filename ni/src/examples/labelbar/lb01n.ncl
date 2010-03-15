;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           lb01n.ncl
;
;      Author:         Bob Lackman
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 13 18:31:18 MDT 1995
;
;      Description:    Demonstrates the LabelBar Object defaults.
;

;
; Begin NCL script.
;
begin

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "lb01" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "lb01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./lb01n.ncgm"
  end create
else
  if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
    wid = create "lb01Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (str_lower(wks_type).eq."ps") then
;
; Create a PS workstation.
;
      wid = create "lb01Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./lb01n.ps"
      end create
    else
      if (str_lower(wks_type).eq."pdf") then
;
; Create a PDF workstation.
;
        wid = create "lb01Work" pdfWorkstationClass defaultapp
          "wkPDFFileName" : "./lb01n.pdf"
        end create
      end if
    end if
  end if
end if

;
; Specify the viewport extent of the object.
;
pid = create "LabelBar" labelBarClass wid
    "vpXF" : 0.
    "vpYF" : 1.
    "vpWidthF" : 1.
    "vpHeightF" : 1.
end create

draw(pid)
frame(wid)

;
; End NCL script.
;
end
