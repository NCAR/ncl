;
;      $Id: xy02n.ncl,v 1.6 1995-03-23 16:31:18 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy02n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 07:47:16 MST 1995
;;
;; Description:    This program shows how to create an XyPlot object
;;                 and how to tweak some of the XyPlot resources to
;;                 change the appearance of the plot.  A resource file
;;                 is used to change the resources except in those
;;                 cases where a resource is an array and can only be
;;                 changed programmatically.
;;
;;                 The "CoordArrays" object is used to set up the data.
;;

;
; Begin NCL script.
;
begin

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
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy02.res" in this
; case.
; 
appid = create "xy02" appLayerClass defaultapp
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
  xworkwid = create "xy02Work" ncgmWorkstationLayerClass defaultapp
      "wkMetaName" : "xy02n.ncgm"
  end create
else
;
; Create an XWorkstation object.
;
  xworkid = create "xy02Work" xWorkstationLayerClass defaultapp
      "wkPause" : "True"
  end create
end if

;
; Define the data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The id
; for this object will later be used as the value for the XyPlot object
; resource, "xyCoordData".
;
dataid = create "xyData" coordArraysLayerClass defaultapp
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.  The resources that are being changed are done
; in the "xy02.res" file, and they will affect this XyPlot object.
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
