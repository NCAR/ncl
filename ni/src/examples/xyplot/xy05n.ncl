;
;      $Id: xy05n.ncl,v 1.1 1995-02-09 23:07:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy05n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 10:33:47 MST 1995
;;
;;  Description:    This example shows how to create an XY plot object with
;;                  multiple lines in the plot using the CoordArrays object.
;;                  The CoordArrTable object is only available for the C
;;                  programming language, so we must use CoordArrays here.
;;
;;                  Some of the XY marker resources are tweaked in the
;;                  resource file to show how to change the appearance of
;;                  these multiple lines.  This example also shows you how to
;;                  use the xyYIrregularPoints resource to define your own Y
;;                  axis values.
;;
;;                  The "CoordArrays" object is used to set up the data,
;;                  and the "DataDep" object is used to describe attributes
;;                  of the data being plotted, like the marker styles and
;;                  sizes.

procedure assigndata(
    y[*][*]:float,
    npts:integer,
    ncurve:integer
) 
local
    i,
    j,
    theta
begin
  do j = 0, ncurve-1
    do i = 0, npts-1
      theta = 0.031415926535898*i
      y(j,i) = (j+1)*100.+.9*i*sin(theta)
    end do
  end do
end

procedure assignexp(
    exp[*]:float,
    npts:integer
) 
local
    i
begin
  do i = 0, npts-1
    exp(i) = 2.^(i+4)
  end do
end

;
; Create variables to contain data
;
ncurve = 4
npts = 500
len = new(ncurve,integer)
len = (/500,200,400,300/)
ydra = new((/ncurve,npts/),float)
linecolors = new(ncurve, integer)
markcolors = new(ncurve, integer)
markmodes = new(ncurve, character)
marksizes = new(ncurve, float)
linecolors = (/40, 0, 100, 50/)
markcolors = (/0, 75, 15, 0/)
markers = (/1, 2, 3, 4/)
markmodes = (/"NOMARKERS","MARKERSONLY","MARKLINES","NOMARKERS"/)
marksizes = (/.015,.008,.024,.015/)

;
; Create array for explicit data values
;
exp_vals = new(10,float)
assignexp(exp_vals,10)

;
; Initialize some data for the XY plot
; 
assigndata(ydra,npts,ncurve)

;
; Create application and X workstation object.  The application name
; is used to determine the name of the resource file, which will be
; "xy05.res" in this case.
; 
appid = create "xy05" appLayerClass noparent end create

;
; Create an x workstation
;
xworkid = create "x" xWorkstationLayerClass noparent end create

;
; Define data objects.  Notice that for each array, we want a different
; number of points, so we use special array indexing that's available
; in NCL.
;
dataid = new(ncurve,graphic)
do i=0,ncurve-1
  dataid(i) = create "xyData"+i coordArraysLayerClass noparent
    "caYArray": ydra(i,0:(len(i)-1))
  end create
end do

;
; Define Data Dependent resources.  Here's where you specify the resources
; for defining the marker styles of each line.
;
datadepid = new(ncurve,graphic)
do i=0,ncurve-1
  datadepid(i) = create "XYDep"+i xyDataDepLayerClass noparent
    "dsDataItem":      dataid(i)
    "xyMarkerMode":    markmodes(i)
    "xyMarkerSizeF":   marksizes(i)
    "xyMarker":        markers(i)
    "xyMarkerColor":   markcolors(i)
    "xyColor":         linecolors(i)
  end create
end do

;
; Create the Plot object which is created as a child of the X workstation
; object.  The resources that are being changed are done in the "xy05.res"
; file, and they affect this Plot object.
; 
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCurveData": datadepid
    "xyYIrregularPoints":  exp_vals
end create

;
; Draw the plot (to its parent X Workstation)
; 
draw(plotid)
frame(xworkid)
