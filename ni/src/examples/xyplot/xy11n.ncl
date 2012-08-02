;
;      $Id: xy11n.ncl,v 1.2 1998-04-15 21:40:50 ethan Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy11n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:   Tue May  9 16:00:15 MDT 1995
;;
;; Description:        This example program demonstrates how to display
;;                     your graphics to an X window, and then copy it
;;                     into a meta file.
;;

;
; Begin NCL script.
;
begin

;
; Create variables to contain data.
;
npts = 28

temp = (/-13.500000,-10.500000,-5.500000,-3.100000,-1.300000,-6.700000,\
-12.900000,-13.100000,-17.700001,-16.900000,-21.299999,-37.099998,\
-40.099998,-41.900002,-42.099998,-62.700001,-59.299999,-60.700001,\
-52.099998,-55.099998,-55.900002,-63.500000,-57.900002,-60.500000,\
-58.099998,-60.900002,-54.900002,-57.299999 /)

pressure = (/835.000000,832.000000,827.000000,821.000000,791.000000,\
693.000000,627.000000,606.000000,560.000000,555.000000,500.000000,383.000000,\
355.000000,339.000000,314.000000,209.000000,192.000000,143.000000,111.000000,\
100.000000,95.300003,76.400002,62.599998,58.299999,47.299999,30.000000,\
27.200001,21.500000/)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy11.res" in this
; case.
; 
appid = create "xy11" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Create an ncgmWorkstation object.
;
ncgmid = create "xy11Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "xy11n.ncgm"
end create
;
; Create an X11 workstation.
;
xworkid = create "xy11Work" windowWorkstationClass defaultapp
      "wkPause"   : True
end create

;
;   Fill the ResList with the resources for the CoordArrays
;   object - then create the CoordArrays data object.
;   This object is used to describe data to the HLU library.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caXArray": temp
    "caYArray": pressure
end create

;
; Fill the ResList with the resources for the XyPlot object
; including the "Data" which is the object id for the
; CoordArrays object that was just created.
; 
plotid = create "xyPlot" xyPlotClass xworkid
    "vpXF":   0.25
    "vpYF":   0.75
    "vpWidthF":   0.5
    "vpHeightF":   0.5
    "xyCoordData": dataid
    "tiMainOn":   True
    "tiXAxisOn":  True
    "tiYAxisOn":  True
    "trYReverse" : True
end create

;
;   Draw the XyPlot - It was created as a child of the X Workstation
;   so it will draw to the X Workstation's device (Window).  Then
;   Call "Frame" for the X Workstation - this flushes the graphics
;   buffers so all the graphics are displayed - and since we
;   set the wkPause resource to True the program will pause on
;   the frame call until the user "clicks" in the Window.
; 
draw(plotid)
frame(xworkid)

;
;   Move the XyPlot to the Ncgm Workstation - This makes the
;   XyPlot a child of the Ncgm Workstation so the XyPlot
;   will draw to the Ncgm Workstations' device instead of the
;   X Workstations device.
;
NhlChangeWorkstation(plotid,ncgmid)

;
;   Draw the XyPlot - It is now a child of the Ncgm Workstation
;   so it will draw to the Ncgm Workstation's device (ncgm file).

;   Then C  Call "Frame" for the Ncgm Workstation - this flushes
;   the graphics C  buffers so all the graphics are displayed.
;
draw(plotid)
frame(ncgmid)

;
; End NCL script.
;
end


