;
;      $Id: xy05n.ncl,v 1.5 1995-03-01 22:10:44 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy05n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Feb  9 10:33:47 MST 1995
;;
;; Description:    This example shows how to create an XyPlot object
;;                 with multiple lines using the CoordArrays and
;;                 multiple Data objects.  Using multiple Data
;;                 objects allows you to have a different number of
;;                 points in each line.
;;
;;                 Some of the XyPlot marker resources are tweaked in
;;                 the resource file to show how to change the
;;                 appearance of these multiple lines.  This example
;;                 also shows you how to use the xyYIrregularPoints
;;                 resource to define your own Y axis values.
;;
;;                 The "CoordArrays" object is used to set up the data,
;;                 and the "DataDep" object is used to describe
;;                 attributes of the data being plotted, like the
;;                 marker styles and sizes.
;;

;
; Begin NCL script.
;
begin

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
; Create variables to contain data.
;
ncurve = 4
npts = 500
len = new(ncurve,integer)
len = (/500,200,400,300/)
ydra = new((/ncurve,npts/),float)

;
; Create array for explicit data values.
;
exp_vals = new(10,float)
assignexp(exp_vals,10)

;
; Initialize some data for the XyPlot object.
; 
assigndata(ydra,npts,ncurve)

;
; Create Application and XWorkstation objects.  The Application
; object name is used to determine the name of the resource file,
; which is "xy05.res" in this case.
; 
appid = create "xy05" appLayerClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

xworkid = create "xy05Work" xWorkstationLayerClass defaultapp end create

;
; Define the Data objects.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The id for
; each object will then later be used as a value for a DataDep
; resource, "dsDataItem".
;
dataid = new(ncurve,graphic)
do i=1,ncurve
  dataid(i-1) = create "xyData"+i coordArraysLayerClass defaultapp
    "caYArray": ydra(i-1,0:(len(i-1)-1))
  end create
end do

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.  The resources that are being changed are done
; in the "xy05.res" file, and they will affect this XyPlot object.
;
plotid = create "xyPlot" xyPlotLayerClass xworkid
    "xyCoordData": dataid
    "xyYIrregularPoints":  exp_vals
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
