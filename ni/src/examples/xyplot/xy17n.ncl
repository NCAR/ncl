;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       xy17n.ncl
;
;   Author:     Bob Lackman
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       24 Jan 1996
;
;   Description:    Reads an ASCII file with 4 variables:
;                   lon, u, v, and t.  u, v, and t are plotted
;                   with 3 stacked y axes.
;
begin

; Create variables to contain data.
;
ncurve = 3
npts = 129
length = new(ncurve,integer)

;
; Create array to contain dataitem ids.
;
dataid = new(ncurve,graphic)

;
; Initialize some data for the XyPlot object.
; 
;assigndata(y,length,npts,ncurve)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy17.res" in this
; case.
; 
appid = create "xy17" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create
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

; print(lon)
; print(u)
; print(v)
; print(t)

y = (/u, v, (t - 273.15) * 9.0 / 5.0 + 32.0/)

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if(str_lower(wks_type).eq."ncgm") then
;
; Open NCGM workstation.
;
    wks = create "xy17Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy17n.ncgm"
    end create
end if
if(str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "xy17Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if(str_lower(wks_type).eq."oldps") then

;
; Open PS workstation.
;
  wks = create "xy17Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy17n.ps"
  end create
end if
if(str_lower(wks_type).eq."oldpdf") then
;
; Open PDF workstation.
;
  wks = create "xy17Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy17n.pdf"
  end create
end if
if(str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Open cairo PS/PDF workstation.
;
  wks = create "xy17Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy17n"
    "wkFormat" : wks_type
  end create
end if
if(str_lower(wks_type).eq."png") then
;
; Open cairo PNG workstation.
;
  wks = create "xy17Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy17n"
    "wkFormat" : wks_type
  end create
end if

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
xy1 = create "xy1" xyPlotClass wks
    "vpXF": .20
    "vpYF": .80
    "vpWidthF" :  .6
    "vpHeightF" : .2
    "xyCoordData": field1
    "trYReverse" : False
    "trYMaxF" :  -50.
    "trYMinF" :  -90.
    "trXMaxF" :  360.
    "trXMinF" :   0.
    "tmYROn"    : False
    "tmYUseLeft" : False
    "tmXMajorGrid"    : True
    "tmXBLabelsOn"    : False
    "tmYLLabelsOn"    : True
    "tmYLMajorLengthF" : .01
    "tmYLMajorOutwardLengthF" : .0
    "tmYLMode"  : "Explicit"
    "tmYLValues"  : (/-90.,-80.,-70.,-60.,-50./)
    "tmYLLabels"  : (/"-90.","-80.","-70.","-60.","-50."/)
    "tmYLLabelsOn"    : True
    "tmYLLabelFontColor" : "red"
    "tiMainString" : "Temperature, U, V Stacked Plots"
    "tiYAxisString" : "Temp (Deg C)"
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
xy2 = create "xy2" xyPlotClass wks
    "vpXF": .20
    "vpYF": .60
    "vpWidthF" :  .6
    "vpHeightF" : .2
    "xyCoordData": field2
    "trYReverse" : False
    "trYMaxF" :   60.
    "trYMinF" :   10.
    "trXMaxF" :  360.
    "trXMinF" :   0.
    "tmYROn"    : True
    "tmYLOn"    : False
    "tmYUseLeft" : False
    "tmXMajorGrid"    : True
    "tmYLLabelsOn"    : False
    "tmYRLabelsOn"    : True
    "tmYRMode"  : "Explicit"
    "tmYRValues"  : (/10.,20.,30.,40.,50.,60./)
    "tmYRLabels"  : (/"10.","20.","30.","40.","50.","60."/)
    "tmXBLabelsOn"    : False
    "tmYRLabelFontColor" : "green"
    "tiYAxisString" : "U (m/s)"
    "tiXAxisFontHeightF" : 0.02
    "tiYAxisFontHeightF" : 0.02
    "tiXAxisFont" : "helvetica-bold"
    "tiYAxisFont" : "helvetica-bold"
    "tiYAxisFontColor" : "green"
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
xy3 = create "xy3" xyPlotClass wks
    "vpXF": .20
    "vpYF": .40
    "vpWidthF" :  .6
    "vpHeightF" : .2
    "xyCoordData": field3
    "trYReverse" : False
    "trYMaxF" :   20.
    "trYMinF" :  -20.
    "trXMaxF" :  360.
    "trXMinF" :   0.
    "tmYROn"    : False
    "tmYUseLeft" : False
    "tmYLLabelsOn"    : True
    "tmXBLabelsOn"    : True
    "tmXMajorGrid"    : True
    "tmYLMajorLengthF" : .01
    "tmYLMajorOutwardLengthF" : .0
    "tmYLMode"  : "Explicit"
    "tmYLLabelsOn"    : True
    "tmYLLabelFontColor" : "blue"
    "tiYAxisString" : "V (m/s)"
    "tiXAxisString" : "Longitude (Degs)"
    "tiXAxisFontHeightF" : 0.02
    "tiYAxisFontHeightF" : 0.02
    "tiXAxisFont" : "helvetica-bold"
    "tiYAxisFont" : "helvetica-bold"
    "tiYAxisFontColor" : "blue"
    "tmYRMinorOn" : False
    "tmYLMinorOn" : False
    "tmYLValues"  : (/-20.,-10.,0.,10.,20./)
    "tmYLLabels"  : (/"-20.","-10.","0.","10.","20."/)
end create

getvalues xy1
    "xyCoordDataSpec" : spec1
end getvalues

setvalues spec1
   "xyMonoLineColor" : True
   "xyLineColor" : "red"
end setvalues

getvalues xy2
    "xyCoordDataSpec" : spec2
end getvalues

setvalues spec2
   "xyMonoLineColor" : True
   "xyLineColor" : "green"
end setvalues

getvalues xy3
    "xyCoordDataSpec" : spec3
end getvalues

setvalues spec3
   "xyMonoLineColor" : True
   "xyLineColor" : "blue"
end setvalues


draw(xy1)
draw(xy2)
draw(xy3)
frame(wks)

delete(wks)

end
