;
;      $Id: xy03n.ncl,v 1.6 1995-03-23 16:31:21 haley Exp $
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
; Create variables to contain data.
;
npts = 500
xdra = new(npts, float)
ydra = new(npts, float)

;
; Initialize some data for the XyPlot object.
; 
assigndata(xdra,ydra,npts)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy03.res" in this
; case.
; 
appid = create "xy03" appLayerClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  xworkwid = create "xy03Work" ncgmWorkstationLayerClass defaultapp
      "wkMetaName" : "xy03n.ncgm"
  end create
else
;
; Create an XWorkstation object.
;
  xworkid = create "xy03Work" xWorkstationLayerClass defaultapp
      "wkPause" : "True"
  end create
end if

;
; Define the data object.  The id for this object will later be used
; as the value for the XyPlot data resource, "xyCoordData".
;
dataid = create "xyData" coordArraysLayerClass defaultapp
    "caXArray": xdra
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; Xworkstation object.  The resources that are being changed are done
; in the "xy03.res" file, and they will affect this XyPlot object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCoordData": dataid
end create

;
; Draw the plot (to its parent XWorkstation).
; 
draw(plotid)
frame(xworkid)

;
; End NCL script.
;
end
