;
;      $Id: xy09n.ncl,v 1.1 1995-05-03 21:39:50 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy09n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Tue May  2 14:50:39 MDT 1995
;;
;;  Description:    This example is similar to the ncargex Autograph
;;                  example "agex06".  It shows how to create different
;;                  kinds of axes using a combination of TickMark and
;;                  XyPlot resources.
;;
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
npts = 501
xdra = new(npts, float)
ydra = new(npts, float)

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = new( (/6,3/), float)
cmap(0,:) = (/0.0,0.0,0.0/)
cmap(1,:) = (/1.0,1.0,1.0/)
cmap(2,:) = (/1.0,0.5,0.0/)
cmap(3,:) = (/0.0,1.0,0.5/)
cmap(4,:) = (/0.5,0.0,1.0/)
cmap(5,:) = (/0.6,0.2,0.2/)

;
; Initialize some data for the XyPlot object.
; 
assigndata(xdra,ydra,npts)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy09.res" in this
; case.
; 
appid = create "xy09" appClass defaultapp
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
  xworkid = create "xy09Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy09n.ncgm"
      "wkColorMap" : cmap
  end create
else
;
; Create an XWorkstation object.
;
  xworkid = create "xy09Work" xWorkstationClass defaultapp
      "wkPause"   : "True"
      "wkColorMap": cmap
  end create
end if

;
; Define the data object.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caXArray": xdra
    "caYArray": ydra
end create

;
; Create and draw four XyPlot objects.
;
do i = 1,4
  plotid = create "xyPlot"+i xyPlotClass xworkid
      "xyCoordData": dataid
  end create
  draw(plotid)
end do

frame(xworkid)

;
; End NCL script.
;
end
