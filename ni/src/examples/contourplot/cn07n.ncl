;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn07n.ncl
;
;   Author:     Ethan Alpert
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Fri Apr 28 11:43:39 MDT 1995
;
;   Description:    Reads a netCDF file and produces a series of
;                   contour plots.
;
begin

;
; Open NetCDF file containing Geo-Potential height forecast information
;
filedir = ncargpath("data")
filename = filedir + "/cdf/contour.cdf"
a = addfile(filename,"r")

NCGM=1

if (NCGM.eq.1) then
;
; Open NCGM workstation and name the metafile "contour00.ncgm"
;
        wks = create "cn07Work" ncgmWorkstationClass noparent
                "wkMetaName" : "cn07n.ncgm"
        end create
else
;
; Create an X workstation.
;
        wks = create "cn07Work" xWorkstationClass defaultapp
                "wkPause" : "True"
        end create
end if

;
; Create a scalar field object and configure the missing values and
; the start and end information.
;
field1 = create "field1" scalarFieldClass noparent
    "sfDataArray" : a->Z(0,3,:,:) 
    "sfMissingValueV": a->Z@_FillValue
    "sfXCStartV" : a->lon(0)
    "sfXCEndV": a->lon(dimsizes(a->lon) - 1)
    "sfYCStartV" : a->lat(0)
    "sfYCEndV": a->lat(dimsizes(a->lat) - 1)
end create

;
; Create a contour object 
;
con1 = create "con1" contourPlotClass wks 
    "vpXF": .2
    "vpYF": .8
    "vpWidthF" : .6
    "vpHeightF" : .6
    "cnScalarFieldData" : field1
    "cnLevelSelectionMode" : "ManualLevels"
    "cnMinLevelValF" : 5400.0
    "cnMaxLevelValF" : 5950.0
    "cnLevelSpacingF" : 50.0
    "cnFillOn" : "True"
    "trXMinF" : -140.0
    "trXMaxF" : -52.5
    "trYMinF" : 20.0
    "trYMaxF" : 60.0
    "tiMainString" : "Geo-potential height @500mb"
    "tiXAxisString" : "Lon"
    "tiYAxisString" : "Lat"
    "tmXBMode" : "EXPLICIT"
    "tmXBValues" : (/-60, -75, -90, -105, -120, -135 /)
    "tmXBLabels" : (/"60W", "75W", "90W", "105W", "120W", "135W"/)
    "tmYLMode" : "EXPLICIT"
    "tmYLValues" : (/60, 50, 40 , 30, 20/)
    "tmYLLabels" : (/"60N", "50N", "40N", "30N", "20N"/)
    "tmXMajorGrid" : "True"
    "tmYMajorGrid" : "True"
    "tmXBMinorOn" : "False"
    "tmYLMinorOn" : "False"
end create
draw(con1)
frame(wks)

;
; Loop on remaing time steps
;
do i = 1, dimsizes(a->frtime) - 1
    setvalues field1
        "sfDataArray" : a->Z(i,3,:,:) 
    end setvalues
    draw(con1)
    frame(wks)
end do
end
