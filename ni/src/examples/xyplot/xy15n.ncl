;
;      $Id: xy15n.ncl,v 1.2 1996-05-02 22:27:18 haley Exp $
;
;***********************************************************************
;                                                                      *
;                Copyright (C)  1995                                   *
;        University Corporation for Atmospheric Research               *
;                All Rights Reserved                                   *
;                                                                      *
;***********************************************************************
;
;  File:       xy15n.ncl
;
;  Author:     David Brown (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;  Date:       Wed Oct 18 16:38:51 MDT 1995
;
; Description:  
;       This example illustrates the creation of a set of 4
;       of 'stacked' XyPlots. Each plot has the same X axis.
;       By making the top 3 plots into annotations of the 
;       bottom plot, all four plots can be manipulated as
;       a unit. To demonstrate this concept the second frame sets
;       the viewport of the base plot. Because all the annotations
;       have their "amResizeNotify" resource set to true (in the
;       resource file), all the annotation plots resize themselves
;       proportionally to the change in the size of the base plot.
;       Each plot draws a variation of sinusoidal curve.
;

begin

;
; Define the number of points in each curve.
;
NPTS=500
PI100=0.031415926535898
EXP = 2.7182818
ydra1 = new(NPTS,float)
ydra2 = new(NPTS,float)
ydra3 = new(NPTS,float)
ydra4 = new(NPTS,float)
theta = new(NPTS,float)
;
; Initialize data for the XyPlot object.
;
do i = 0,NPTS-1
    theta(i) = PI100*i
end do
ydra1 = sin(theta)
ydra2 = sin(theta * theta)
ydra3 = sin(EXP^theta)
ydra4 = sin(3*sqrt(fabs(theta)))
;
; Create an Application object so we are set up to use the "xy15.res"
; resource file.
;
appid = create "xy15" appClass defaultapp 
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
 xworkid = create "simple" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy15n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Set up an X11 output workstation.
;
    xworkid = create "simple" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "simple" psWorkstationClass defaultapp
        "wkPSFileName" : "xy15n.ps"
      end create
    end if
  end if
end if
;
; Define 4 separate CoordArrays objects - one for each XYPlot. 
;
dataid1 = create "xyData1" coordArraysClass defaultapp
     "caYArray":    ydra1
end create

dataid2 = create "xyData2" coordArraysClass defaultapp
     "caYArray":    ydra2
end create

dataid3 = create "xyData3" coordArraysClass defaultapp
     "caYArray":    ydra3
end create

dataid4 = create "xyData4" coordArraysClass defaultapp
     "caYArray":    ydra4
end create

;
; Create 4 XyPlot objects. 
;

xy1 = create "xy1" xyPlotClass xworkid
     "xyCoordData":    dataid1
     "vpYF":           0.3
end create

xy2 = create "xy2" xyPlotClass xworkid
     "xyCoordData":    dataid2
end create

xy3 = create "xy3" xyPlotClass xworkid
     "xyCoordData":    dataid3
end create

xy4 = create "xy4" xyPlotClass xworkid
     "xyCoordData":    dataid4
end create

am2 = NhlAddAnnotation(xy1,xy2)
am3 = NhlAddAnnotation(xy1,xy3)
am4 = NhlAddAnnotation(xy1,xy4)

;
; Draw the plot.
;
draw(xy1)
frame(xworkid)

;
; Now set the viewport of the base plot only; redrawing the base plot
; causes all the other plots to be redrawn as well. Notice that they
; are all resized and repositioned to match the new size and position
; of the base plot. The whole assemblage functions as a single 
; composite plot object.
;
setvalues xy1
     "vpWidthF":    0.65
     "vpXF":        0.25
     "vpYF":        0.4
     "vpHeightF":   0.16
end setvalues
    
draw(xy1)
frame(xworkid)
;
; Destroy the workstation, all the plots, and the data objects are all
; descended from the App object, so it is only necessary to 
; destroy the App object in order to destroy all the objects created
; in this example.
;
delete(xworkid)

end

