;
;      $Id: xy01n.ncl,v 1.18 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy01n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:46:58 MST 1995
;;
;;  Description:    This program shows how to create an XyPlot object
;;                 with all the default resources being used, with the
;;                 exception of the data resource.  There's no "default
;;                 data", so we need to create some.  A resource file
;;                 is included with this example, but only to show what
;;                 all the XyPlot resources are and what their defaults
;;                 are set to. The whole resource file is commented out.
;;
;;                 The "CoordArrays" object is used to set up the data.
;;

;
; Begin NCL script.
;
begin

;
; Create variable to contain data.
;
npts = 500

;
; Initialize some data for the XyPlot object.
; 
ydra = 500.+.9*ispan(0,npts-1,1)*sin(0.031415926535898*ispan(0,npts-1,1))
;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy01.res" in this
; case.
; 
appid = create "xy01" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy01Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy01n.ncgm"
  end create
end if

if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "xy01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if

if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy01n.ps"
  end create
end if

if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy01n.pdf"
  end create
end if

if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS or PDF DocumentWorkstation object.
;
  xworkid = create "xy01Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy01n"
    "wkFormat"   : str_lower(wks_type)
  end create
end if

if (str_lower(wks_type).eq."png")
;
; Create a cairo PNG imageWorkstation object.
;
  xworkid = create "xy01Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy01n"
    "wkFormat"   : str_lower(wks_type)
  end create
end if

;
; Define the data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The id
; for this object will later be used as the value for the XyPlot
; object resource, "xyCoordData".
;
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.
; 
plotid = create "xyPlot" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Draw the plot.
; 
draw(plotid)
frame(xworkid)

;
; End NCL script.
;
end
