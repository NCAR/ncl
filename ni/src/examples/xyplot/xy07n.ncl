;
;      $Id: xy07n.ncl,v 1.4 1998-04-15 21:40:48 ethan Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy07n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;; Date:       Mon May  8 11:40:41 MDT 1995
;;
;; Description:    This example is similar to the ncargex Autograph
;;                 example "agex11".
;;

;
; Begin NCL script.
;
begin

function fran( 
	xfran:double
)
local yfran, j
begin
    yfran = 9821.*xfran+.211327
    j = doubletoint(yfran)
    xfran = yfran - j
    return(xfran)
end


procedure assigndata(
    x[*]:float,
    y[*]:float,
    npts:integer,
    xfran:double
) 
local
    i
begin
  franx = new(npts,double)
  frany = new(npts,double)
  do i = 0, npts-1
    franx(i)=fran(xfran)
    frany(i)=fran(xfran)
  end do
  x=.5+(2.*((doubletofloat(franx))-.5))^5
  y=.5+(2.*((doubletofloat(frany))-.5))^5
end

; Create variables to contain data.
;
npts = 250
xdra = new(npts, float)
ydra = new(npts, float)

;
; Create array to contain dataitem ids.
;
dataid = new(2,graphic)

;
; Initialize data for the XyPlot object.
; 
xfran = new(1,double)
xfran = 2.718281828459045

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy07.res" in this
; case.
; 
appid = create "xy07" appClass defaultapp 
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = new( (/4, 3/), float)
cmap(0,:) = (/1.0, 1.0, 1.0/)
cmap(1,:) = (/0.0, 0.0, 0.0/)
cmap(2,:) = (/1.0, 0.0, 0.0/)
cmap(3,:) = (/0.0, 0.0, 1.0/)

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy07Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy07n.ncgm"
      "wkColorMap" : cmap
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    xworkid = create "xy07Work" xWorkstationClass defaultapp
      "wkPause" : True
      "wkColorMap" : cmap
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "xy07Work" psWorkstationClass defaultapp
        "wkPSFileName" : "xy07n.ps"
        "wkColorMap" : cmap
      end create
    end if
  end if
end if

;
; Since we have two sets of points that we want to color differently,
; we need to create two Data obects here.
;
do j = 0,1
;
; Initialize data.
;
  assigndata(xdra,ydra,npts,xfran)

; Define a data object.  Note that we are naming each object differently
; so we can distinguish them in the resource file.
;
  dataid(j) = create "xyData"+j coordArraysClass defaultapp
    "caXArray": xdra
    "caYArray": ydra
  end create
end do

;
; Create the XyPlot object.
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
