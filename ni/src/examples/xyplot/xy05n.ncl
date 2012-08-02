;
;      $Id: xy05n.ncl,v 1.18 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                     ;
;                Copyright (C)  1995                                  ;
;        University Corporation for Atmospheric Research              ;
;                All Rights Reserved                                  ;
;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy05n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Tue Apr  4 13:02:35 MDT 1995
;;
;; Description:    This example shows one way on how to create an
;;                 XyPlot object with multiple lines and multiple
;;                 lengths using multiple data objects.  Some of the
;;                 XyPlot line resources are tweaked to show how to
;;                 change the appearances of the lines.
;;
;;                 The "CoordArrays" object is used to set up the data.
;;                 (The C version uses the "CoordArrTable" object which
;;                 is not available in Fortran or NCL.)
;;
;
; Begin NCL script.
;
begin

;
; Create variables to contain data.
;
ncurve = 10
npts = 100

;
; Create array to contain dataitem ids.
;
dataid = new(ncurve,graphic)

;
; Initialize some data for the XyPlot object.
; 

ii = new((/ncurve,npts/),float)
jj = new((/npts,ncurve/),float)
ii = onedtond(ispan(0,npts-1,1),(/ncurve,npts/))
jj = onedtond(ispan(1,ncurve,1),(/npts,ncurve/))
jj!0 = "x"
jj!1 = "y"

pi = 3.14159
y = jj(y | :,x | :)*sin((2.*ii*pi)/((npts-(jj(y | :, x | :)-1)*10)-1))
delete(ii)
delete(jj)
;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
ncolors = 12
cmap = (/ (/0.00,0.00,0.00/), \
	 (/1.00,1.00,1.00/), \
	 (/0.00,0.00,1.00/), \
	 (/0.00,1.00,0.00/), \
	 (/0.00,1.00,0.75/), \
	 (/0.50,0.50,0.63/), \
	 (/1.00,0.00,0.00/), \
	 (/0.75,0.38,0.25/), \
	 (/0.75,0.00,0.75/), \
	 (/1.00,0.38,0.38/), \
	 (/1.00,0.83,0.00/), \
	 (/1.00,1.00,0.00/) /)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy05.res" in this
; case.
; 
appid = create "xy05" appClass defaultapp
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
  xworkid = create "xy05Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy05n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "xy05Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy05Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy05n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy05Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy05n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy05Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy05n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy05Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy05n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Create the CoordArrays which defines the data for the XyPlot
; object. The id from this object will become the value for the XyPlot
; resource, "xyCoordData".
;
do i=0,ncurve-1
  dataid(i) = create "xyData"+i coordArraysClass defaultapp
    "caYArray": y(i,0:(npts-i*10)-1)
  end create
end do

;
; Create the XyPlot object and tweak some of the tickmark, title and
; view port resources (some in the "xy05.res" resource file).
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
