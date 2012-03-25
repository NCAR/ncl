;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn08n.ncl
;
;   Author:     Ethan Alpert
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Fri Apr 28 11:47:03 MDT 1995
;
;   Description:    draws a vertical profile of temperature for longitudes
;                   separated by 5 degrees
;

begin

; 
; Open data file containing grid of global temperatures.
;
filedir = ncargpath("data")
filename = filedir + "/cdf/contour.cdf"
a = addfile(filename,"r")

;
; set color table
;
cmap = (/ (/0.0, 0.0, 0.0/), \
	 (/1.0, 1.0, 1.0/), \
	 (/0.0, 0.0, 0.0/), \
	 (/1.0, 0.0, 0.0/), \
	 (/0.0, 1.0, 0.0/), \
	 (/0.0, 0.0, 1.0/), \
	 (/1.0, 1.0, 0.0/), \
	 (/0.0, 1.0, 1.0/), \
	 (/1.0, 0.0, 1.0/), \
	 (/0.5, 0.0, 0.0/), \
	 (/0.5, 1.0, 1.0/), \
	 (/0.0, 0.0, 0.5/), \
	 (/1.0, 1.0, 0.5/), \
	 (/0.5, 0.0, 1.0/), \
	 (/1.0, 0.5, 0.0/), \
	 (/0.0, 0.5, 1.0/), \
	 (/0.5, 1.0, 0.0/), \
	 (/0.5, 0.0, 0.5/), \
	 (/0.5, 1.0, 0.5/), \
	 (/1.0, 0.5, 1.0/), \
	 (/0.0, 0.5, 0.0/), \
	 (/0.5, 0.5, 1.0/), \
	 (/1.0, 0.0, 0.5/) /)

wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm")
;
; Open NCGM workstation.
;
  wks = create "cn08Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "cn08n.ncgm"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "cn08Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Open PS workstation.
;
  wks = create "cn08Work" psWorkstationClass defaultapp
    "wkPSFileName" : "cn08n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Open PDF workstation.
;
  wks = create "cn08Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "cn08n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Open cairo PS/PDF workstation.
;
  wks = create "cn08Work" documentWorkstationClass defaultapp
    "wkFileName" : "cn08n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Open cairo PNG workstation.
;
  wks = create "cn08Work" imageWorkstationClass defaultapp
    "wkFileName" : "cn08n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Set up initial scalar field with longitude of
; temperature data. Data is also converted to 
; Degrees F from Degrees K before being assigned to 
; sfDataArray resource
;
field1 = create "field1" scalarFieldClass defaultapp
    "sfDataArray" : (a->T(0,:,:,0) - 273.15) * 9 / 5 + 32.0
    "sfMissingValueV":  a->T@_FillValue
    "sfXArray" : a->lat
    "sfYArray" : a->level
end create

;
; Determine extents of grid
;
lat = a->lat
min_lat = min(lat)
max_lat = max(lat)

level = a->level
min_level = min(level)
max_level = max(level)

;
; Create contour using manual spacing.
;
con1 = create "con1" contourPlotClass wks 
    "vpXF": .2
    "vpYF": .8
    "vpWidthF" : .6
    "vpHeightF" : .6
    "cnFillOn" : True
    "cnScalarFieldData" : field1
    "cnLevelSelectionMode" : "ManualLevels"
    "cnMaxLevelCount" : 25
    "cnMinLevelValF" : -80.0
    "cnMaxLevelValF" : 110.0
    "cnLevelSpacingF" : 10.0
    "trXMinF" : min_lat
    "trXMaxF" : max_lat
    "trYMinF" : min_level
    "trYMaxF" : max_level
    "trYReverse" : True
    "tiMainString" : "Longitude " + a->lon(0) + " Degrees"
end create
;
; Draw first step
;
draw(con1)
frame(wks)

;
; Loop on remaining longitude values and reset the title every iteration
;
do i = 1, filevardimsizes(a,"lon") - 1 
    setvalues field1
        "sfDataArray" : (a->T(0,:,:,i) - 273.15) * 9 / 5 + 32.0
    end setvalues
    setvalues con1
        "tiMainString" : "Longitude " + a->lon(i) + " Degrees"
    end setvalues
    draw(con1)
    frame(wks)
end do

delete(con1)
delete(field1)
delete(wks)

end
