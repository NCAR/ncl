;
;      $Id: xy05n.ncl,v 1.12 1995-06-22 21:09:35 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                     ;
;                Copyright (C)  1995                                  ;
;        University Corporation for Atmospheric Research              ;
;                All Rights Reserved                                  ;
;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy05n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Tue Apr  4 13:02:35 MDT 1995
;;
;; Description:    This example shows one way on how to create an
;;                 XyPlot object with multiple lines and multiple
;;                 lengths using multiple data objects.  Some of the
;;                 XyPlot line resources are tweaked to show how to
;;                 change the appearances of the lines.
;;
;;                 The "CoordArrays" object is used to set up the data.
;;                 (The C version uses the "CoordArrTable" object which
;;                 is not available in Fortran or NCL.)
;;
;
; Begin NCL script.
;
begin

procedure assigndata(
    y[*][*]:float,
    length[*]:integer,
    npts:integer,
    ncurve:integer
) 
local
    i,
    j,
    pi
begin
  pi = 3.14159
  do j = 0, ncurve-1
    length(j) = npts - j*10
    do i = 0, npts-1
      y(j,i) = (j+1)*sin((2.*i*pi)/(length(j)-1))
    end do
  end do
end

;
; Create variables to contain data.
;
ncurve = 10
npts = 100
y = new((/ncurve,npts/),float)
length = new(ncurve,integer)

;
; Create array to contain dataitem ids.
;
dataid = new(ncurve,graphic)

;
; Initialize some data for the XyPlot object.
; 
assigndata(y,length,npts,ncurve)

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
ncolors = 12
cmap = new( (/ncolors,3/), float)
cmap(0,:) = (/0.00,0.00,0.00/)
cmap(1,:) = (/1.00,1.00,1.00/)
cmap(2,:) = (/0.00,0.00,1.00/)
cmap(3,:) = (/0.00,1.00,0.00/)
cmap(4,:) = (/0.00,1.00,0.75/)
cmap(5,:) = (/0.50,0.50,0.63/)
cmap(6,:) = (/1.00,0.00,0.00/)
cmap(7,:) = (/0.75,0.38,0.25/)
cmap(8,:) = (/0.75,0.00,0.75/)
cmap(9,:) = (/1.00,0.38,0.38/)
cmap(10,:) = (/1.00,0.83,0.00/)
cmap(11,:) = (/1.00,1.00,0.00/)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy05.res" in this
; case.
; 
appid = create "xy05" appClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy05Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy05n.ncgm"
      "wkColorMap" : cmap
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    xworkid = create "xy05Work" xWorkstationClass defaultapp
      "wkPause" : "True"
      "wkColorMap" : cmap
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "xy05Work" psWorkstationClass defaultapp
        "wkPSFileName" : "xy05n.ps"
        "wkColorMap" : cmap
      end create
    end if
  end if
end if

;
; Create the CoordArrays which defines the data for the XyPlot
; object. The id from this object will become the value for the XyPlot
; resource, "xyCoordData".
;
do i=0,ncurve-1
  dataid(i) = create "xyData"+i coordArraysClass defaultapp
    "caYArray": y(i,0:length(i)-1)
  end create
end do

;
; Create the XyPlot object and tweak some of the tickmark, title and
; view port resources (some in the "xy05.res" resource file).
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
