;
;      $Id: xy04n.ncl,v 1.1 1995-02-09 23:07:23 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy04n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:47:16 MST 1995
;;
;;  Description:    This script shows how to create an XY plot object with
;;                  some of the XY Plot axis resources tweaked.  A resource
;;                  file is used to changed the resources except in those
;;                  cases where a resource has to be change programmatically,
;;                  like array resources.
;;
;;                  The "CoordArrays" object is used to set up the data.

procedure assigndata(
    y[*][*]:float,
	ncurve:integer,
    npts:integer
) 
local
    i,
    theta
begin
  do j = 0, ncurve-1
    do i = 0, npts-1
      theta = 0.031415926535898*i
      y(j,i) = (j-1)*100.+.9*i*sin(theta)
    end do
  end do
end

;
; Create variables to contain data
;
npts = 500
ncurve = 4
ydra = new((/ncurve,npts/), float)
colors = new(ncurve, integer)
xydash = new(ncurve, integer)
colors = (/50,75,100,40/)
xydash = (/0,5,10,15/)
explabs = new(ncurve, character)
explabs = (/"Curve 1","Curve 2","Curve 3", "Curve 4"/)

;
; Initialize some data for the XY plot
; 
assigndata(ydra,ncurve,npts)

;
; Create application and X workstation object.  The application name
; is used to determine the name of the resource file, which will be
; "xy04.res" in this case.
; 
appid = create "xy04" appLayerClass noparent end create

;
; Create an x workstation
;
xworkid = create "x" xWorkstationLayerClass noparent end create

;
; Define data object
;
dataid = create "xyData" coordArraysLayerClass noparent
    "caYArray": ydra
end create

;
; Define Data Dependent resources.  Here's where you specify the arrays
; for defining the color, label, and dash pattern of each line, 
;
datadepid = create "XYDep" xyDataDepLayerClass noparent
	"dsDataItem":       dataid
    "xyColors":         colors
	"xyDashPatterns":   xydash
	"xyExplicitLabels": explabs
end create

;
; Create the Plot object which is created as a child of the X workstation
; object.  The resources that are being changed are done in the "xy04.res"
; file, and they affect this Plot object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCurveData": datadepid
end create

;
; Draw the plot (to its parent X Workstation)
; 
draw(plotid)
frame(xworkid)
