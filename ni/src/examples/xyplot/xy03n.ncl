;
;      $Id: xy03n.ncl,v 1.2 1995-02-09 23:07:20 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy03n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:47:16 MST 1995
;;
;;  Description:    This script shows how to create an XY plot object with
;;                  some of the XY Plot axis resources tweaked.  A resource
;;                  file is used to change the resources except in those
;;                  cases where a resource has to be changed programmatically,
;;                  like array resources.
;;
;;                  The "CoordArrays" object is used to set up the data.

procedure assigndata(
    x[*]:float,
    y[*]:float,
    npts:integer
) 
local
    i,
    theta
begin
  do i = 0, npts-1
    theta = 0.031415926535898*i
    x(i) = 500.+.9*i*cos(theta)
    y(i) = 500.+.9*i*sin(theta)
  end do
end

;
; Create variables to contain data
;
npts = 500
xdra = new(npts, float)
ydra = new(npts, float)

;
; Initialize some data for the XY plot
; 
assigndata(xdra,ydra,npts)

;
; Create application and X workstation object.  The application name
; is used to determine the name of the resource file, which will be
; "xy03.res" in this case.
; 
appid = create "xy03" appLayerClass noparent end create

;
; Create an x workstation
;
xworkid = create "x" xWorkstationLayerClass noparent end create

;
; Define data object
;
dataid = create "xyData" coordArraysLayerClass noparent
    "caXArray": xdra
    "caYArray": ydra
end create

;
; Create the Plot object which is created as a child of the X workstation
; object.  The resources that are being changed are done in the "xy03.res"
; file, and they affect this Plot object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCurveData": dataid
end create

;
; Draw the plot (to its parent X Workstation)
; 
draw(plotid)
frame(xworkid)
