;
;      $Id: xy01n.ncl,v 1.2 1995-02-16 14:53:21 haley Exp $
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
; Create variable to contain data.
;
npts = 500
ydra = new(npts, float)

;
; Initialize some data for the XyPlot object.
; 
assigndata(ydra,npts)

;
; Create Application and XWorkstation objects.  The Application object
; name is used to determine the name of the resource file, which is
; "xy01.res" in this case.
; 
appid = create "xy01" appLayerClass noparent end create
xworkid = create "xy01Work" xWorkstationLayerClass noparent end create

;
; Define the data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The id
; for this object will later be used as the value for the XyPlot
; object resource, "xyCurveData".
;
dataid = create "xyData" coordArraysLayerClass noparent
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCurveData": dataid
end create

;
; Draw the plot (to its parent XWorkstation).
; 
draw(plotid)
frame(xworkid)
