;
;      $Id: xy01n.ncl,v 1.12 1996-05-02 22:27:14 haley Exp $
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

;
; Begin NCL script.
;
begin

;
; Create variable to contain data.
;
npts = 500
ydra = new(npts, float)

;
; Initialize some data for the XyPlot object.
; 
ii = new(npts,float)
do i = 0, npts-1
  ii(i) = i
end do
ydra = 500.+.9*ii*sin(0.031415926535898*ii)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy01.res" in this
; case.
; 
appid = create "xy01" appClass defaultapp
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
  xworkid = create "xy01Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy01n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    xworkid = create "xy01Work" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "xy01Work" psWorkstationClass defaultapp
        "wkPSFileName" : "xy01n.ps"
      end create
    end if
  end if
end if

;
; Define the data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The id
; for this object will later be used as the value for the XyPlot
; object resource, "xyCoordData".
;
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray": ydra
end create

;
; Create the XyPlot object which is created as a child of the
; XWorkstation object.
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
