;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn16n.ncl
;
;   Author:     Bob Lackman
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Wed Mar 19, 1996
;
;   Description:    Combines a vector fill contour plot, a raster
;                   contour plot, and a map plot on a single frame.
;
begin
  filedir = ncargpath("data")
  file1 = addfile(filedir + "/cdf/sstdata_netcdf.nc","R")
;
; The input sea surface temperature array of 0 to 360 longitude is
; shifted to 30E to 390 (30E) by array index manipulation.
;
  dims = filevardimsizes(file1,"sst")
  b = new((/dims(1),dims(2)/),float)
  b(:,0:165) = file1->sst(0,:,15:180)
  b(:,165:180) = file1->sst(0,:,0:15)
;
; create an sst data object
;
  sstjan = create "sf" scalarFieldClass noparent
    "sfDataArray" : b
    "sfXCStartV"  : 30.
    "sfXCEndV"    : 390.
    "sfYCStartV"  : file1->lat(0)
    "sfYCEndV"    : file1->lat(filevardimsizes(file1,"lat")-1)
  end create
;
; Output to an X11 workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an ncgmWorkstation object.
;
  wid = create "cn16Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn16n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
 wid = create "cn16Work" windowWorkstationClass defaultapp
   "wkPause" : True
 end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "cn16Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./cn16n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "cn16Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./cn16n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "cn16Work" documentWorkstationClass defaultapp
    "wkFileName" : "./cn16n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "cn16Work" imageWorkstationClass defaultapp
    "wkFileName" : "./cn16n"
    "wkFormat" : wks_type
  end create
end if
;
; Create a ContourPlot Object
;
  cn = create "cn" contourPlotClass wid
    "cnScalarFieldData" : sstjan
    "vpXF" : .10
    "vpYF" : .80
    "vpWidthF" : .80
    "vpHeightF" : .40
    "cnInfoLabelOn" : False
    "cnHighLabelsOn" : False
    "cnLowLabelsOn" : False
    "cnMonoLineColor" : False
    "cnLineDrawOrder" : "predraw"
    "cnFillDrawOrder" : "predraw"
    "cnLabelDrawOrder" : "predraw"
    "cnLineLabelInterval" : 2
    "cnLineLabelPlacementMode" : "computed"
    "tiMainOn" : True
    "tiMainString" : "STR:: JANUARY SST CLIMATOLOGY"
    "tiMainFontHeightF" : .020
    "tiMainFont" : 25
    "tmXBMode" : "EXPLICIT"
    "tmXBValues" : (/30,60,90,120,150,180,210,240,270,300,330,360,390/)
    "tmXBLabels" : (/"30E","60E","90E","120E","150E","180","150W","120W","90W","60W","30W","0","30E"/)
    "tmYLMode" : "EXPLICIT"
    "tmYLValues" : (/-90, -60,-30,0,30,60,90/)
    "tmYLLabels" : (/"90S","60S","30S","0","30N","60N","90N"/)
    "tmXTLabelsOn" : True
    "tmYRLabelsOn" : True
    "tmXBLabelFontHeightF" :  .015
    "tmYLLabelFontHeightF" :  .015
    "tmXBMajorOutwardLengthF" : .006
    "tmXBMajorLengthF" : .006
    "tmYLMajorOutwardLengthF" : .006
    "tmYLMajorLengthF" : .006
    "tmXBMinorOn" : False
    "tmXTMinorOn" : False
    "tmYLMinorOn" : False
    "tmYRMinorOn" : False
  end create
;
; The ice field is added as a raster contour.  Areas without ice
; are colored transparent.
;
  cellsize = .8/360.
  ice = create "ice" contourPlotClass wid
    "cnScalarFieldData" : sstjan
    "vpXF" : .10
    "vpYF" : .80
    "vpWidthF" : .80
    "vpHeightF" : .40
    "tmXBOn" : False
    "tmYLOn" : False
    "tmXBMinorOn" : False
    "tmYLMinorOn" : False
    "tmXBLabelsOn"   : False
    "tmYLLabelsOn"   : False
    "tiMainOn" : False
    "tmXBMajorLengthF" : 0.
    "tmYLMajorLengthF" : 0.
    "cnFillOn" : True
    "cnFillMode" : "RasterFill"
    "cnRasterCellSizeF" : cellsize
    "cnMinLevelValF" : -2.0
    "cnFillColors"   : (/2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/)
    "cnLineLabelsOn" : False
    "cnLinesOn" : False
    "cnMonoFillColor" : False
    "cnInfoLabelOn" : False
    "cnHighLabelsOn" : False
  end create
;
; Create a MapPlot object.
;
  mp = create "mp" mapPlotClass wid
    "vpXF" : .10
    "vpYF" : .80
    "vpWidthF" : .80
    "vpHeightF" : .40
    "mpFillOn" : True
    "mpLabelsOn" : False
    "mpDefaultFillColor" : 11
    "mpLandFillColor" : 11
    "mpOutlineOn" : False
    "mpAreaMaskingOn" : True
    "mpMaskAreaSpecifiers" : "Oceans"
    "mpGridAndLimbOn" : False
    "mpLimitMode" : "latlon"
    "mpMinLonF" : 30.
    "mpMaxLonF" : 390.
    "mpCenterLonF" : 210.
  end create
  draw(ice)
  draw(cn)
  draw(mp)
  frame(wid)
  delete(wid)
end



