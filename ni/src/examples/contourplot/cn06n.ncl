;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn06n.ncl
;
;   Author:     Ethan Alpert
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Fri Apr 28 11:37:28 MDT 1995
;
;   Description:    Reads a netCDF file and produces a series of
;                   temperature contour plots.
;

begin

;
; Open NetCDF file containing temperature forecast information.
;
filedir = ncargpath("data")
filename = filedir + "/cdf/contour.cdf"
a = addfile(filename,"r")

;
; Set the color map.
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
	(/0.5, 1.0, 0.0/) /)

wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm")
;
; Open NCGM workstation.
;
  wks = create "cn06Work" ncgmWorkstationClass defaultapp 
      "wkMetaName" : "cn06n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "cn06Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Open PS workstation.
;
  wks = create "cn06Work" psWorkstationClass defaultapp 
    "wkPSFileName" : "cn06n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Open PDF workstation.
;
  wks = create "cn06Work" pdfWorkstationClass defaultapp 
    "wkPDFFileName" : "cn06n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Open cairo PS/PDF workstation.
;
  wks = create "cn06Work" documentWorkstationClass defaultapp 
    "wkFileName" : "cn06n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Open cairo PNG workstation.
;
  wks = create "cn06Work" imageWorkstationClass defaultapp 
    "wkFileName" : "cn06n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Create a scalar field data object and configure its extents missing
; values and at the same time convert it from Degrees K to Degrees F
;
field1 = create "field1" scalarFieldClass defaultapp
    "sfDataArray" : (a->T(0,0,:,:) - 273.15) * 9 / 5 + 32.0
    "sfMissingValueV":  a->T@_FillValue
    "sfXCStartV" : a->lon(0)
    "sfXCEndV": a->lon(filevardimsizes(a,"lon") - 1)
    "sfYCStartV" : a->lat(0)
    "sfYCEndV": a->lat(filevardimsizes(a,"lat") - 1)
end create

;
; Create contour object and assign scalar field data to it
;
con1 = create "con1" contourPlotClass wks 
    "vpXF": .2
    "vpYF": .8
    "vpWidthF" : .6
    "vpHeightF" : .6
    "cnScalarFieldData" : field1
    "cnLevelSelectionMode" : "ManualLevels"
    "cnMinLevelValF" : -40.0
    "cnMaxLevelValF" : 110.0
    "cnLevelSpacingF" : 10.0
    "cnFillOn" : True
    "cnMonoFillPattern" : True
    "cnFillPatterns" : 0
    "trXMinF" : -140.0
    "trXMaxF" : -52.5
    "trYMinF" : 20.0
    "trYMaxF" : 60.0
    "tiMainFuncCode": "#"
    "tiMainString" : a@title + "#C#" + a@history
    "tiMainFontHeightF" : 0.02
    "tiXAxisString" : a->lon@long_name
    "tiYAxisString" : a->lat@long_name
    "tmXBMode" : "EXPLICIT"
    "tmXBValues" : (/-60, -75, -90, -105, -120, -135 /)
    "tmXBLabels" : (/"60W", "75W", "90W", "105W", "120W", "135W"/)
    "tmYLMode" : "EXPLICIT"
    "tmYLValues" : (/60, 50, 40 , 30, 20/)
    "tmYLLabels" : (/"60N", "50N", "40N", "30N", "20N"/)
    "cnLowLabelsOn" : True
    "cnHighLabelsOn" : True
    "tmXMajorGrid" : True
    "tmYMajorGrid" : True
    "tmXBMinorOn" : False
    "tmYLMinorOn" : False
end create

draw(con1)
frame(wks)

;
; Loop on remaining fields of data and draw contour
;
do i = 1, filevardimsizes(a,"frtime") - 1
    setvalues field1
        "sfDataArray" : (a->T(i,0,:,:) - 273.15) * 9 / 5 + 32.0
    end setvalues
    draw(con1)
    frame(wks)
end do

delete(i)
delete(con1)
delete(field1)

end
