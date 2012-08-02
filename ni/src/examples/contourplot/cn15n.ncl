;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn15n.ncl
;
;   Author:     Bob Lackman
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Wed Jan 24, 1996
;
;   Description:    Combines a contour plot and an xy plot on a
;                   single frame.  Output goes to an X11 window,
;                   an NCGM, PostScript, *and* a PDF file.
;
begin
  filedir = ncargpath("data")
  file1 = addfile(filedir + "/cdf/sstdata_netcdf.nc","R")
  nlat = filevardimsizes(file1,"lat")
  mlon = filevardimsizes(file1,"lon")
  sstjan = file1->sst(0,:,:)
;
; This section flips the field to starting at 30E.
;
;  a = file1->sst(0,:,:)
;  b = a
;  b(:,0:165) = a(:,15:180)
;  b(:,165:180) = a(:,0:15)
;
; Create an sst data object.
;
  jan = create "sf" scalarFieldClass noparent
    "sfDataArray": sstjan
    "sfXCStartV" : 0.
    "sfXCEndV"   : 360.
    "sfYCStartV" : file1->lat(0)
    "sfYCEndV"   : file1->lat(filevardimsizes(file1,"lat")-1)
  end create
;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
  ncolors = 16
  cmap = (/ (/1.00,1.00,1.00/), \  ; white
  	 (/0.00,0.00,0.00/), \  ; black
  	 (/0.00,0.00,1.00/), \  ; blue
  	 (/0.20,0.56,0.80/), \  ; sky blue
  	 (/0.00,1.00,1.00/), \  ; cyan
  	 (/0.50,0.00,1.00/), \  ; blue magenta
  	 (/0.00,1.00,0.00/), \  ; green
  	 (/0.14,0.56,0.14/), \  ; forest green
  	 (/1.00,1.00,0.00/), \  ; yellow
  	 (/1.00,0.50,0.00/), \  ; orange
  	 (/1.00,0.00,1.00/), \ ; magenta
  	 (/1.00,0.00,0.00/), \ ; red
  	 (/0.65,0.16,0.16/), \ ; brown
  	 (/0.86,0.58,0.44/), \ ; tan
  	 (/0.66,0.66,0.66/), \ ; light gray
  	 (/0.40,0.40,0.40/) /) ; dark gray
;
; Create an ncgmWorkstation object.
;
  ncgm = create "cn15Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn15n.ncgm"      ; name the ncgm
    "wkColorMap" : cmap                ; assign the new color map
  end create
;
; Create an X11 workstation.
;
   x = create "cn15Work" windowWorkstationClass defaultapp
    "wkColorMap" : cmap                     ; assign the new color map
   end create
;
; Create an older-style PostScript workstation.
;
   ps1 = create "cn15Work" psWorkstationClass defaultapp
    "wkVisualType" : "color"
    "wkPSFileName" : "./cn15n.ps"      ; name the PostScript file
    "wkColorMap"   : cmap              ; assign the new color map
;
; Since the plots are beside each other, use landscape mode and the
; PostScript/PDF resources for positioning the plot on the paper.
;
    "wkOrientation"  : "landscape"
    "wkDeviceLowerX" :   0
    "wkDeviceLowerY" :  60  
    "wkDeviceUpperX" : 600
    "wkDeviceUpperY" : 700
  end create
;
; Create an older-style PDF workstation.
;
   pdf1 = create "cn15Work" pdfWorkstationClass defaultapp
    "wkVisualType"  : "color"
    "wkPDFFileName" : "./cn15n.pdf"      ; name the PDF file
    "wkColorMap"    : cmap               ; assign the new color map
;
; Since the plots are beside each other, use landscape mode and the
; PostScript/PDF resources for positioning the plot on the paper.
;
    "wkOrientation"  : "landscape"
    "wkDeviceLowerX" :   0
    "wkDeviceLowerY" :  60  
    "wkDeviceUpperX" : 600
    "wkDeviceUpperY" : 700
  end create
;
; Create a cairo PS Workstation object.
;
   ps2 = create "cn15Work" documentWorkstationClass defaultapp
    "wkFileName"   : "./cn15n.cairo"   ; name the PostScript file
    "wkFormat"     : "ps"
    "wkColorMap"   : cmap              ; assign the new color map
;
; Since the plots are beside each other, use landscape mode and the
; PostScript/PDF resources for positioning the plot on the paper.
;
    "wkOrientation"  : "landscape"
    "wkDeviceLowerX" :   0
    "wkDeviceLowerY" :  60  
    "wkDeviceUpperX" : 600
    "wkDeviceUpperY" : 700
  end create
;
; Create a cairo PDF Workstation object.
;
   pdf2 = create "cn15Work" documentWorkstationClass defaultapp
    "wkFileName"   : "./cn15n.cairo"   ; name the PDF file
    "wkFormat"     : "pdf"
    "wkColorMap"   : cmap              ; assign the new color map
;
; Since the plots are beside each other, use landscape mode and the
; PostScript/PDF resources for positioning the plot on the paper.
;
    "wkOrientation"  : "landscape"
    "wkDeviceLowerX" :   0
    "wkDeviceLowerY" :  60  
    "wkDeviceUpperX" : 600
    "wkDeviceUpperY" : 700
  end create
;
; Create a cairo PNG Workstation object.
;
   png1 = create "cn15Work" imageWorkstationClass defaultapp
    "wkFileName"   : "./cn15n.cairo"   ; name the PNG file
    "wkFormat"     : "png"
    "wkColorMap"   : cmap              ; assign the new color map
  end create
;
; Create a ContourPlot object.
;
  cn = create "cn" contourPlotClass x
    "cnScalarFieldData"        : jan
    "vpXF"                     : .06
    "vpYF"                     : .65
    "vpWidthF"                 : .60
    "vpHeightF"                : .30
    "cnInfoLabelOn"            : False
    "cnHighLabelsOn"           : False
    "cnLowLabelsOn"            : False
    "cnMonoLineColor"          : False
    "cnLineColors"             : (/12,13,14,7,2,0,8,11,6,9,4,3,5,7,10/)
    "cnLineDrawOrder"          : "predraw"
    "cnFillDrawOrder"          : "predraw"
    "cnLabelDrawOrder"         : "predraw"
    "cnLineLabelInterval"      : 2
    "cnLineLabelPlacementMode" : "computed"
    "tmXBMode"                 : "EXPLICIT"
    "tmXBValues"               : (/0,60,120,180,240,300,360/)
    "tmXBLabels"               : (/"0","60E","120E","180","120W","60W","0"/)
    "tmYLMode"                 : "EXPLICIT"
    "tmYLValues"               : (/-90, -60,-30,0,30,60,90/)
    "tmYLLabels"               : (/"90S","60S","30S","EQ","30N","60N","90N"/)
    "tmXTLabelsOn"             : False
    "tmYRLabelsOn"             : True
    "tmXBLabelFontHeightF"     :  .010
    "tmYLLabelFontHeightF"     :  .010
    "tmXBMajorOutwardLengthF"  : .006
    "tmXBMajorLengthF"         : .006
    "tmXTMajorLengthF"         : 0.
    "tmXTMajorOutwardLengthF"  : 0.
    "tmYLMajorOutwardLengthF"  : .006
    "tmYLMajorLengthF"         : .006
    "tmXBMinorOn"              : False
    "tmXTMinorOn"              : False
    "tmYLMinorOn"              : False
    "tmYRMinorOn"              : False
  end create
;
; Create another ContourPlot object.
;
  cellsize = .8/360.
  ice = create "ice" contourPlotClass x
    "cnScalarFieldData" : jan
    "vpXF"              : .06
    "vpYF"              : .65
    "vpWidthF"          : .60
    "vpHeightF"         : .30
    "tmXBOn"            : False
    "tmYLOn"            : False
    "tmXBMinorOn"       : False
    "tmYLMinorOn"       : False
    "tmXBLabelsOn"      : False
    "tmYLLabelsOn"      : False
    "tmXBMajorLengthF"  : 0.
    "tmYLMajorLengthF"  : 0.
    "cnFillOn"          : True
    "cnRasterModeOn"    : True
    "cnRasterCellSizeF" : cellsize
    "cnMinLevelValF"    : -2.0
    "cnFillColors"      : (/0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2/)
    "cnLineLabelsOn"    : False
    "cnLinesOn"         : False
    "cnMonoFillColor"   : False
    "cnInfoLabelOn"     : False
    "cnHighLabelsOn"    : False
  end create
;
; Create a MapPlot object.
;
  mp = create "mp" mapPlotClass x
    "vpXF"                   : .06
    "vpYF"                   : .65
    "vpWidthF"               : .60
    "vpHeightF"              : .30
    "mpFillOn"               : True
    "mpLabelsOn"             : False
    "mpDefaultFillColor"     : "DarkSalmon"
    "mpLandFillColor"        : "DarkSalmon"
    "mpOutlineOn"            : False
    "mpAreaMaskingOn"        : True
    "mpMaskAreaSpecifiers"   : "Oceans"
    "mpInlandWaterFillColor" : 2
    "mpGridAndLimbOn"        : False
    "mpLimitMode"            : "latlon"
    "mpMinLonF"              : 0.
    "mpMaxLonF"              : 360.
    "mpCenterLonF"           : 180.
  end create
;
; Create a TextItem object.
;
  tx = create "tx" textItemClass x
    "txPosXF" : 0.5
    "txPosYF" : 0.8
    "txJust"    : "CENTERCENTER"
    "txString" : "January Climatological Surface Temperature"
    "txFontHeightF" : .030
    "txFont" : 25
  end create

;
; Start with the Jan data.  Do zonal averages over water.
;
; Use 1.e+36 to denote a missing value.
;
  sstmsg = 1.e+36
;  zoneave = new( (/nlat/), float, sstmsg)
;
;  Read the ocean(1)/land(2) mask ascii dataset created by Areas/Ezmap.
;
  ocean1 = asciiread(filedir + "/asc/oceanland30e.asc",(/91,181/),"integer")
  sstjan = mask(sstjan,ocean1.eq.1,True)
  zoneave = dim_avg(sstjan)

;
; Create a coordarrays data object.
;
  zonal = create "zonal" coordArraysClass defaultapp
    "caYArray": file1->lat
    "caXArray": zoneave
    "caXMissingV" : 1.e36
  end create
;
; Create XyPlot object and assign data to it.
;
  xy_plot = create "xy_plot" xyPlotClass x
    "vpXF": .73
    "vpYF": .65
    "vpWidthF" :  .25
    "vpHeightF" : .30
    "xyCoordData": zonal
    "trYMaxF" :  90.
    "trYMinF" : -90.
    "trXMaxF" :  30.
    "trXMinF" :   0.
    "tmXTLabelsOn" : False
    "tmYRLabelsOn" : False
    "tmYLLabelsOn" : False
    "tmXBMajorLengthF" : .006
    "tmXBMajorOutwardLengthF" : .006
    "tmXBLabelFontHeightF" :  .010
    "tmYLMajorOutwardLengthF" : .006
    "tmYLMajorLengthF" : .006
    "tmXTMajorLengthF" : 0.
    "tmXTMajorOutwardLengthF" : 0.
    "tmYRMajorLengthF" : 0.
    "tmYRMajorOutwardLengthF" : 0.
    "tmXBMinorOn" : False
    "tmYLMinorOn" : False
    "tmXBMode" : "EXPLICIT"
    "tmXBValues" : (/0,3,6,9,12,15,18,21,24,27,30/)
    "tmXBLabels" : (/"0","3","6","9","12","15","18","21","24","27","30"/)
  end create
;
; Draw all objects to X11 window.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(x)
;
; Reassign the workstation to save an ncgm.
;
  NhlChangeWorkstation (ice,ncgm)
  NhlChangeWorkstation (cn,ncgm)
  NhlChangeWorkstation (mp,ncgm)
  NhlChangeWorkstation (xy_plot,ncgm)
  NhlChangeWorkstation (tx,ncgm)
;
; Draw all objects to the NCGM.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(ncgm)
;
; Reassign the workstation to save PS.
;
  NhlChangeWorkstation (ice,ps1)
  NhlChangeWorkstation (cn,ps1)
  NhlChangeWorkstation (mp,ps1)
  NhlChangeWorkstation (xy_plot,ps1)
  NhlChangeWorkstation (tx,ps1)
;
; Draw all objects to PostScript.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(ps1)
;
; Reassign the workstation to save PDF.
;
  NhlChangeWorkstation (ice,pdf1)
  NhlChangeWorkstation (cn,pdf1)
  NhlChangeWorkstation (mp,pdf1)
  NhlChangeWorkstation (xy_plot,pdf1)
  NhlChangeWorkstation (tx,pdf1)
;
; Draw all objects to PDF.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(pdf1)

;
; Reassign the workstation to save cairo PS
;
  NhlChangeWorkstation (ice,ps2)
  NhlChangeWorkstation (cn,ps2)
  NhlChangeWorkstation (mp,ps2)
  NhlChangeWorkstation (xy_plot,ps2)
  NhlChangeWorkstation (tx,ps2)
;
; Draw all objects to cairo PS.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(ps2)

;
; Reassign the workstation to save cairo PDF
;
  NhlChangeWorkstation (ice,pdf2)
  NhlChangeWorkstation (cn,pdf2)
  NhlChangeWorkstation (mp,pdf2)
  NhlChangeWorkstation (xy_plot,pdf2)
  NhlChangeWorkstation (tx,pdf2)
;
; Draw all objects to cairo PDF.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(pdf2)
;
; Reassign the workstation to save cairo PNG
;
  NhlChangeWorkstation (ice,png1)
  NhlChangeWorkstation (cn,png1)
  NhlChangeWorkstation (mp,png1)
  NhlChangeWorkstation (xy_plot,png1)
  NhlChangeWorkstation (tx,png1)
;
; Draw all objects to cairo PNG.
;
  draw(ice)
  draw(cn)
  draw(mp)
  draw(xy_plot)
  draw(tx)
  frame(png1)
end
