;
;      $Id: xy03n.ncl,v 1.18 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy03n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:47:16 MST 1995
;;
;; Description:    This program shows how to create an XyPlot object
;;                 with some of the XyPlot line resources tweaked.  A
;;                 resource file is used to changed the resources 
;;                 except in those cases where a resource is an array
;;                 and can only be changed programmatically.
;;                 This program uses the same Y-axis dataset as the
;;                 example "xy02", but this time values for the X
;;                 axis are specified, changing the look of the plot.
;;
;;                 The "CoordArrays" object is used to set up the data.
;;

;
; Begin NCL script.
;
begin

;
; Create variables to contain data.
;
npts = 500

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = (/ (/0.0,0.0,0.0/), \
	 (/1.0,1.0,1.0/), \
	 (/0.0,0.5,1.0/), \
	 (/0.0,1.0,0.0/) /)

;
; Initialize some data for the XyPlot object.
; 
xdra = 500.+.9*ispan(0,npts-1,1)*cos(0.031415926535898*ispan(0,npts-1,1))
ydra = 500.+.9*ispan(0,npts-1,1)*sin(0.031415926535898*ispan(0,npts-1,1))
;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy03.res" in this
; case.
; 
appid = create "xy03" appClass defaultapp
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
  xworkid = create "xy03Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy03n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "xy03Work" windowWorkstationClass defaultapp
    "wkPause"   : True
    "wkColorMap": cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy03Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy03n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy03Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy03n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy03Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy03n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy03Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy03n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Define the data object.  The id for this object will later be used
; as the value for the XyPlot data resource, "xyCoordData".
;
dataid = create "xyData" coordArraysClass defaultapp
    "caXArray": xdra
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; Xworkstation object.  The resources that are being changed are done
; in the "xy03.res" file, and they will affect this XyPlot object.
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
