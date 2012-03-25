;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       xy16n.ncl
;
;   Author:     Bob Lackman
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       24 Jan 1996
;
;   Description:    Reads an ASCII file with 4 variables:
;                   lon, u, v, and t.  u, v, and t are plotted
;                   with 3 separate y axes.
;

begin

; Create variables to contain data.
;
ncurve = 3
npts = 129

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy16.res" in this
; case.
; 
appid = create "xy16" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create
;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy16Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy16n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Set up an X11 output workstation.
;
  xworkid = create "xy16Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy16Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy16n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy16Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy16n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy16Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy16n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy16Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy16n"
    "wkFormat" : wks_type
  end create
end if

;
; Read ASCII file xy.asc
;
x1_y3 = asciiread(ncargpath("data")+"/asc/xy.asc",4*129,"float")
;
;  xy.asc has 4 vars of length 129 longitudes, lon, u, v, t
;
;     The data is taken at 43N latitude.  Longitude is an index
;     1-129 standing for 0 deg - 360 deg in steps of 360/128?
;     u and v are in m/s, and t is in deg K.
;
; print(x1_y3)

lon = x1_y3(0:512:4)
u   = x1_y3(1:513:4)
v   = x1_y3(2:514:4)
t   = x1_y3(3:515:4)

;
; Create a coordarrays data object and configure its extents missing
; values and at the same time convert it from Degrees K to Degrees F
;
field1 = create "field1" coordArraysClass defaultapp
     "caXArray": (lon - 1.) * 360./128.
     "caYArray": (t - 273.15) * 9 / 5 + 32.0
end create

;
; Create a coordarrays data object and configure its extents missing
; values and at the same time convert it from Degrees K to Degrees F
;
field2 = create "field2" coordArraysClass defaultapp
     "caXArray": (lon - 1.) * 360./128.
     "caYArray": u
end create

;
; Create a coordarrays data object and configure its extents missing
; values and at the same time convert it from Degrees K to Degrees F
;
field3 = create "field3" coordArraysClass defaultapp
     "caXArray": (lon - 1.) * 360./128.
     "caYArray": v
end create

;
; Create XyPlot object for curve 1 and assign data to it
;
xy1 = create "xy1" xyPlotClass xworkid
    "vpXF": .20
    "vpYF": .80
    "vpWidthF" :  .5
    "vpHeightF" : .6
    "xyCoordData": field1
    "trYReverse" : False
    "trYMaxF" :  -50.
    "trYMinF" :  -90.
    "trXMaxF" :  360.
    "trXMinF" :   0.
    "tmYROn"    : False
    "tmYUseLeft" : False
    "tmYLLabelsOn"    : True
    "tmYLMajorLengthF" : .01
    "tmYLMajorOutwardLengthF" : .01
    "tmYLMode"  : "Explicit"
    "tmYLValues"  : (/-90.,-80.,-70.,-60.,-50./)
    "tmYLLabels"  : (/"-90.","-80.","-70.","-60.","-50."/)
    "tmYLLabelsOn"    : True
    "tmYLLabelFontColor" : "red"
    "tiXAxisString" : "Longitude (Degs)"
    "tiYAxisString" : "Temperature in Deg C"
    "tiXAxisFontHeightF" : 0.02
    "tiYAxisFontHeightF" : 0.02
    "tiXAxisFont" : "helvetica-bold"
    "tiYAxisFont" : "helvetica-bold"
    "tiYAxisFontColor" : "red"
    "tmYRMinorOn" : False
    "tmYLMinorOn" : False
end create

;
; Create XyPlot object for curve 2 and assign data to it
;
xy2 = create "xy2" xyPlotClass xworkid
    "vpXF": .20
    "vpYF": .80
    "vpWidthF" :  .5
    "vpHeightF" : .6
    "xyCoordData": field2
    "trYReverse" : False
    "trYMaxF" :   50.
    "trYMinF" :    0.
    "trXMaxF" :  360.
    "trXMinF" :   0.
    "tmYROn"    : True
    "tmYLOn"    : False
    "tmYUseLeft" : False
    "tmYRLabelsOn"    : True
    "tmYLLabelsOn"    : False
    "tmYRMajorLengthF" : .01
    "tmYRMajorOutwardLengthF" : .01
    "tmYRMode"  : "Explicit"
    "tmYRValues"  : (/0.,10.,20.,30.,40.,50./)
    "tmYRLabels"  : (/"0.","10.","20.","30.","40.","50."/)
    "tmYRLabelFontColor" : "cyan"
    "tiYAxisString" : "U component of wind (m/s)"
    "tiYAxisSide"   : "Right"
    "tiXAxisFontHeightF" : 0.02
    "tiYAxisFontHeightF" : 0.02
    "tiMainFont" : "helvetica-bold"
    "tiXAxisFont" : "helvetica-bold"
    "tiYAxisFont" : "helvetica-bold"
    "tiYAxisFontColor" : "cyan"
    "tmYRMinorOn" : False
    "tmYLMinorOn" : False
end create

;
; Create XyPlot object for curve 3 and assign data to it
;
; Increase the veiwport so the right scale will be about .15 NDC
; right of the other grids.  Plot only the right vertical axis.
; .5NDC = 360 deg lon, thus .65NDC = 360+108 deg lon.
;
xy3 = create "xy3" xyPlotClass xworkid
    "vpXF": .20
    "vpYF": .80
    "vpWidthF" :  .65
    "vpHeightF" : .6
    "xyCoordData": field3
    "trYReverse" : False
    "trYMaxF" :   20.
    "trYMinF" :  -20.
    "trXMaxF" :  468.
    "trXMinF" :   0.
    "tmYROn"    : True
    "tmYLOn"    : False
    "tmYUseLeft" : False
    "tmYRLabelsOn"    : True
    "tmYRMajorLengthF" : .01
    "tmYRMajorOutwardLengthF" : .01
    "tmXBOn"    : False
    "tmXTOn"    : False
    "tmYLOn"    : False
    "tmYROn"    : True
    "tmYRMode"  : "Explicit"
    "tmYRValues"  : (/-20.,-10.,0.,10.,20./)
    "tmYRLabels"  : (/"-20.","-10.","0.","10.","20."/)
    "tmYRLabelFontColor" : "green"
    "tiYAxisString" : "V component of wind (m/s)"
    "tiYAxisSide"   : "Right"
    "tiXAxisFontHeightF" : 0.02
    "tiYAxisFontHeightF" : 0.02
    "tiMainString" : "Three Variables with Individual Scales"
    "tiMainFontHeightF" : 0.030
    "tiMainFont" : "helvetica-bold"
    "tiYAxisFont" : "helvetica-bold"
    "tiYAxisFontColor" : "green"
    "tmYRMinorOn" : False
    "tmYLMinorOn" : False
    "tmXBMinorOn" : False
    "tmXTBorderOn" : False
    "tmXBBorderOn" : False
    "tmYLBorderOn" : False
end create

getvalues xy1
    "xyCoordDataSpec" : spec1
end getvalues

setvalues spec1
   "xyMonoLineColor" : "true"
   "xyLineColor" : "red"
end setvalues

getvalues xy2
    "xyCoordDataSpec" : spec2
end getvalues

setvalues spec2
   "xyMonoLineColor" : "true"
   "xyLineColor" : "cyan"
end setvalues

getvalues xy3
    "xyCoordDataSpec" : spec3
end getvalues

setvalues spec3
   "xyMonoLineColor" : "true"
   "xyLineColor" : "green"
end setvalues


draw(xy1)
draw(xy2)
draw(xy3)
frame(xworkid)

delete(xworkid)

end
