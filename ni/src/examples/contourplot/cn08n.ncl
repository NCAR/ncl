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
; Open data file containing grid of gloval temperatures
;
filedir = ncargpath("data")
filename = filedir + "/cdf/contour.cdf"
a = addfile(filename,"r")

NCGM=1

if (NCGM.eq.1) then
;
; Open NCGM workstation and name the metafile "contour00.ncgm"
;
    wks = create "cn08Work" ncgmWorkstationClass noparent
        "wkMetaName" : "cn08n.ncgm"
    end create
else
;
; Create an X workstation.
;
    wks = create "cn08Work" xWorkstationClass defaultapp
        "wkPause" : "True"
    end create
end if

;
; Set up intial scalar field with longitude of
; temperature data. Data is also converted to 
; Degrees F from Degrees K before being assigned to 
; sfDataArray resource
;
field1 = create "field1" scalarFieldClass noparent
    "sfDataArray" : (a->T(0,:,:,0) - 273.15) * 9 / 5 + 32.0
    "sfMissingValueV":  a->T@_FillValue
    "sfXCStartV" : a->lat(0)
    "sfXCEndV": a->lat(dimsizes(a->lat) - 1)
    "sfXArray" : a->lat
    "sfYArray" : a->level
end create

;
; Determine extents of grid
;
lat = a->lat
if(lat(0) .lt. lat(dimsizes(lat)-1)) then
    min_lat = lat(0)
    max_lat = lat(dimsizes(lat)-1)
else
    max_lat = lat(0)
    min_lat = lat(dimsizes(lat)-1)
end if

level = a->level
if(level(0) .lt. level(dimsizes(level)-1)) then
    min_level = level(0)
    max_level = level(dimsizes(level)-1)
else
    max_level = level(0)
    min_level = level(dimsizes(level)-1)
end if

;
; Create contour using manual spacing.
;
con1 = create "con1" contourPlotClass wks 
    "vpXF": .2
    "vpYF": .8
    "vpWidthF" : .6
    "vpHeightF" : .6
    "cnFillOn" : "True"
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
    "trYReverse" : "True"
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
do i = 1, dimsizes(a->lon) - 1 
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
