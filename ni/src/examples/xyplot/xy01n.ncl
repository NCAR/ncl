;
;      $Id: xy01n.ncl,v 1.1 1995-02-09 14:57:49 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy01n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:46:58 MST 1995
;;
;;  Description:    This NCL script shows how to create an XY plot object with
;;                  all the default resources being used, with the exception 
;;                  of the data resource.  There's no "default data", so we
;;                  need to create some.  A resource file is included with
;;                  this example, but only to show what all the XY
;;                  resources are and what their defaults are set to.
;;                  The whole resource file is commented out.
;;
;;                  The "CoordArrays" object is used to set up the data.

procedure assigndata(
    y[*]:float,
    npts:integer
) 
local
    i,
    theta
begin
  do i = 0, npts-1
    theta = 0.031415926535898*i
    y(i) = 500.+.9*i*sin(theta)
  end do
end

;
; Create variable to contain data
;
npts = 500
ydra = new(npts, float)

;
; Initialize some data for the XY plot
; 
assigndata(ydra,npts)

;
; Create application and X workstation object.  The application name
; is used to determine the name of the resource file, which will be
; "xy01.res" in this case.
; 
appid = create "xy01" appLayerClass noparent end create

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
; Create the Plot object which is created as a child of the X workstation
; object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCurveData": dataid
end create

;
; Draw the plot (to its parent X Workstation)
; 
draw(plotid)
frame(xworkid)
