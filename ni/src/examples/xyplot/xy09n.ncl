;
;      $Id: xy09n.ncl,v 1.9 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy09n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Tue May  2 14:50:39 MDT 1995
;;
;;  Description:    This example is similar to the ncargex Autograph
;;                  example "agex06".  It shows how to create different
;;                  kinds of axes using a combination of TickMark and
;;                  XyPlot resources.
;;
;;

;
; Begin NCL script.
;
begin

;
; Create variables to contain data.
;
npts = 501

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = (/ (/0.0,0.0,0.0/), \
	 (/1.0,1.0,1.0/), \
	 (/1.0,0.5,0.0/), \
	 (/0.0,1.0,0.5/), \
	 (/0.5,0.0,1.0/), \
	 (/0.6,0.2,0.2/) /)

;
; Initialize some data for the XyPlot object.
; 
xdra = 500.+.9*ispan(0,npts-1,1)*cos(0.031415926535898*ispan(0,npts-1,1))
ydra = 500.+.9*ispan(0,npts-1,1)*sin(0.031415926535898*ispan(0,npts-1,1))

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy09.res" in this
; case.
; 
appid = create "xy09" appClass defaultapp
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
  xworkid = create "xy09Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy09n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "xy09Work" windowWorkstationClass defaultapp
    "wkPause"   : True
    "wkColorMap": cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy09Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy09n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy09Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy09n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy09Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy09n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy09Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy09n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Define the data object.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caXArray": xdra
    "caYArray": ydra
end create

;
; Create and draw four XyPlot objects.
;
do i = 1,4
  plotid = create "xyPlot"+i xyPlotClass xworkid
      "xyCoordData": dataid
  end create
  draw(plotid)
end do

frame(xworkid)

;
; End NCL script.
;
end
