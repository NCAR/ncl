;
;      $Id: xy04n.ncl,v 1.18 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy04n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:47:16 MST 1995
;;
;; Description:    This program shows one way on how to create an XyPlot
;;                 object with multiple lines in the plot using
;;                 the CoordArrays object.  Some of the XyPlot line
;;                 resources are tweaked in the resource file to
;;                 show how to change the appearance of these multiple
;;                 lines.
;;
;;                 The "CoordArrays" object is used to set up the data,
;;                 and the "DataDep" object is used to describe
;;                 attributes of the data being plotted, like the
;;                 line color and the dash patterns.
;;

;
; Begin NCL script.
;
begin

;
; Create variables to contain data.
;
npts = 500
ncurve = 4

;
; Initialize data for the XyPlot object.
; 
ii = new((/ncurve,npts/),float)
jj = new((/npts,ncurve/),float)
ii = onedtond(ispan(0,npts-1,1),(/ncurve,npts/))
jj = onedtond(ispan(0,ncurve-1,1),(/npts,ncurve/))
jj!0 = "x"
jj!1 = "y"

ydra = jj(y | :,x | :)*200.+ii*.9*sin(0.031415926535898*ii)

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
ncolors = 6
cmap = (/"black","white","red","green","blue","yellow"/)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy04.res" in this
; case.
; 
appid = create "xy04" appClass defaultapp 
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
  xworkid = create "xy04Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy04n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  xworkid = create "xy04Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy04Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy04n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy04Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy04n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy04Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy04n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy04Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy04n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Define the data object.  The id for this object will later be used
; as the value for the XYPlot data resource, 'xyCoordData'.
; Since only the Y values are specified here, each Y value will be
; paired with its integer array index.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.  The resources that are being changed are done
; in the "xy04.res" file, and they will affect this XyPlot object.
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
