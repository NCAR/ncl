;
;      $Id: xy04n.ncl,v 1.5 1995-03-17 20:56:43 haley Exp $
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
      y(j,i) = j*200.+i*.9*sin(theta)
    end do
  end do
end

;
; Create variables to contain data.
;
npts = 500
ncurve = 4
ydra = new((/ncurve,npts/), float)

;
; Initialize data for the XyPlot object.
; 
assigndata(ydra,ncurve,npts)

;
; Create Application and XWorkstation objects.  The Application
; object name is used to determine the name of the resource file,
; which is "xy04.res" in this case.
; 
appid = create "xy04" appLayerClass defaultapp 
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

xworkid = create "xy04Work" xWorkstationLayerClass defaultapp end create

;
; Define the data object.  The id for this object will later be used
; as the value for the XYPlot data resource, 'xyCoordData'.
; Since only the Y values are specified here, each Y value will be
; paired with its integer array index.
;
dataid = create "xyData" coordArraysLayerClass defaultapp
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.  The resources that are being changed are done
; in the "xy04.res" file, and they will affect this XyPlot object.
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
