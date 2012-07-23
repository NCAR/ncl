;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn10n.ncl
;
;   Author:     Fred Clare
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Fri Jun  9 15:14:04 MDT 1995
;
;   Description:    Reads a netCDF file and produces five plots:
;
;                      1.)  A quick-and-dirty contour plot.
;                      2.)  An improved contour plot.
;                      3.)  A contour plot of a sub-area of plot 2.)
;                      4.)  An XyPlot that is a slice through the
;                           the contoured area in 2.)
;                      5.)  An overlay of the contour plot in 2.) on
;                           an MapPlot object.
;

begin

;
; Open up a data file containing a mound.
;

file1 = addfile(ncargpath("data") + "/cdf/cn10n.cdf","r")

;
;  Set up color map.
;
;
;  Colors for contour fill areas varying from blue to red.
;
cmap = (/ (/1.00, 1.00, 1.00/), \
	 (/0.00,0.00, 0.00/), \
	 (/0.00, 0.15, 1.00/), \
	 (/0.00, 0.05, 1.00/), \
	 (/0.04, 0.00, 1.00/), \
	 (/0.14, 0.00, 1.00/), \
	 (/0.24, 0.00, 1.00/), \
	 (/0.34, 0.00, 1.00/), \
	 (/0.43, 0.00, 1.00/), \
	 (/0.53, 0.00, 1.00/), \
	 (/0.63, 0.00, 1.00/), \
	 (/0.73, 0.00, 1.00/), \
	 (/0.83, 0.00, 1.00/), \
	 (/0.92, 0.00, 1.00/), \
	 (/1.00, 0.00, 0.98/), \
	 (/1.00, 0.00, 0.88/), \
	 (/1.00, 0.00, 0.78/), \
	 (/1.00, 0.00, 0.68/), \
	 (/1.00, 0.00, 0.59/), \
	 (/1.00, 0.00, 0.49/), \
	 (/1.00, 0.00, 0.39/), \
	 (/1.00, 0.00, 0.29/), \
	 (/1.00, 0.00, 0.20/), \
	 (/1.00, 0.00, 0.10/), \
	 (/1.00, 0.00, 0.00/), \
;				\
;  Colors used for labels.	\
; 				\
	 (/0.00, 0.00, 0.00/), \
	 (/0.40, 0.00, 0.40/) /)
;
; Default is to display output to an X workstation
;

wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  workstation_id = create "moundn" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "cn10n.ncgm"
      "wkColorMap"         : cmap
      "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  workstation_id = create "moundx"  windowWorkstationClass defaultapp
    "wkPause"            : True
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an PSWorkstation object.
;
  workstation_id = create "moundp" psWorkstationClass defaultapp
    "wkPSFileName"       : "cn10n.ps"
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an PDFWorkstation object.
;
  workstation_id = create "moundp" pdfWorkstationClass defaultapp
    "wkPDFFileName"       : "cn10n.pdf"
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  workstation_id = create "moundp" documentWorkstationClass defaultapp
    "wkFileName"       : "cn10n"
    "wkFormat" : wks_type
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  workstation_id = create "moundp" imageWorkstationClass defaultapp
    "wkFileName"       : "cn10n"
    "wkFormat" : wks_type
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if

;
; Create a data field.
;

field1 = create "field1" scalarFieldClass defaultapp
  "sfDataArray" : file1->mound
  "sfMissingValueV":  file1->mound@_FillValue
  "sfXCStartV" : file1->ydim(0)
  "sfXCEndV": file1->ydim(filevardimsizes(file1,"ydim") - 1)
  "sfYCStartV" : file1->xdim(0)
  "sfYCEndV": file1->xdim(filevardimsizes(file1,"xdim") - 1)
end create

;
; Create a ContourPlot object using the above data field.
;

con1 = create "con1" contourPlotClass workstation_id
  "vpXF": .15
  "vpYF": .9
  "vpWidthF" : .79
  "vpHeightF" : .79
  "cnScalarFieldData" : field1
end create

;
; Picture 1:  A quick-and-dirty contour plot.
;

draw(con1)
frame(workstation_id)

;
; Picture 2:  An improved contour plot.
;

setvalues con1
  "cnFillOn": True
  "cnMonoFillColor": False
  "cnLevelSpacingF" : 2.0
  "cnSmoothingOn": True
  "cnMaxPointDistanceF" : 0.0
  "cnLineColor": 0
  "cnLineLabelFontColor": 25
  "cnLineLabelFont": 22
  "cnLineLabelFontHeightF": .02
  "cnLineLabelInterval": 3
  "cnLineLabelPlacementMode": "COMPUTED"
  "cnHighLabelFont": 22
  "cnHighLabelFontHeightF": .025
  "cnHighLabelFontColor": 25
  "cnInfoLabelOn": False

  "tmXBMinorPerMajor": 2

  "tmYLMajorLineColor": 26
  "tmYLMinorLineColor": 26
  "tmYLMinorThicknessF": 2.
  "tmXBMajorLineColor": 26
  "tmXBMinorLineColor": 26
  "tmXBMinorThicknessF": 2.

  "tmXBLabelFont": 21
  "tmXBLabelFontHeightF": .03
  "tmXBLabelFontColor": 26

  "tmYLLabelFont": 21
  "tmYLLabelFontHeightF": .03
  "tmYLLabelFontColor": 26

  "tmBorderLineColor": 26

  "tiMainOn": True
  "tiMainFont": 26
  "tiMainFontHeightF": .04
  "tiMainString": "The Hot Zone"
  "tiMainOffsetYF": -0.025
  "tiMainFontColor": 26
end setvalues

draw(con1)
frame(workstation_id)

;
; Picture 3:  Zero in on the top of the mound.
;

setvalues field1
  "sfDataArray": file1->mound({-20:20},{-20:20})
  "sfXCStartV" : -20.
  "sfXCEndV": 20.
  "sfYCStartV" : -20.
  "sfYCEndV": 20.
end setvalues

setvalues con1
  "tiMainString": "A closer look"
  "tmXBMinorPerMajor": 4
  "tmYLMinorPerMajor": 4
  "cnLevelSpacingF" : 0.15
  "cnFillColors": (/9,10,11,12,13,14,15,16,17,18,19,21,23/)
end setvalues

draw(con1)
frame(workstation_id)

;
; Picture 4:  Plot an XyPlot of X values for a specific Y value.
;

y_dataid = create "xyData" coordArraysClass defaultapp
  "caXArray": file1->ydim(:)
  "caYArray": file1->mound({10},:)
end create

xy_id = create "XyPlotData" xyPlotClass workstation_id
  "vpXF": .2
  "vpYF": .85
  "vpWidthF" : .7
  "vpHeightF" : .7

  "xyComputeYMax": False
  "xyComputeYMin": False

  "tiMainOn": True
  "tiMainFont": 26
  "tiMainFontHeightF": .0275
  "tiMainString": "For X = 10."
  "tiMainOffsetYF": -0.00
  "tiMainFontColor": 26
  "tiXAxisString": "Y"
  "tiXAxisFont": 26
  "tiXAxisFontColor": 26
  "tiXAxisFontHeightF": .025
  "tiYAxisString": "F(Y)"
  "tiYAxisFont": 26
  "tiYAxisFontColor": 26
  "tiYAxisFontHeightF": .025

  "tmXTBorderOn": False
  "tmYRBorderOn": False
  "tmXTOn": False
  "tmYROn": False
  "tmYLMajorLineColor": 26
  "tmYLMinorLineColor": 26
  "tmYLMinorThicknessF": 2.
  "tmXBMajorLineColor": 26
  "tmXBMinorLineColor": 26
  "tmXBMinorThicknessF": 2.

  "tmXBMinorPerMajor": 2
  "tmYLMinorPerMajor": 2
  "tmBorderLineColor": 26

  "tmXBLabelFont": 21
  "tmXBLabelFontHeightF": .025
  "tmXBLabelFontColor": 26
  "tmYLLabelFont": 21
  "tmYLLabelFontHeightF": .025
  "tmYLLabelFontColor": 26
end create

dataspec = NhlAddData(xy_id,"xyCoordData",y_dataid)
setvalues dataspec
  "xyMonoLineColor": True
  "xyLineColor": 24
  "xyLineThicknessF": 2.
end setvalues

getvalues xy_id
  "tmYLTickStartF": ymin
  "tmYLTickEndF": ymax
end getvalues
setvalues xy_id
  "trYMinF": ymin
  "trYMaxF": ymax
end setvalues

draw(xy_id)
frame(workstation_id)

;
; Picture 5:  Overlay the mound on top of Colorado.
;

mapid = create "Map0" mapPlotClass workstation_id
  "pmTitleDisplayMode" : "always"

  "tiMainOn": True
  "tiMainFont": 26
  "tiMainFontHeightF": .037
  "tiMainOffsetYF": -0.00
  "tiMainFontColor": 26
  "tiMainString" : "Mound over Colorado"

  "vpXF": .1
  "vpYF": .9
  "vpWidthF" : .79
  "vpHeightF" : .79

  "mpFillOn" : False
  "mpOutlineBoundarySets" : "allBoundaries"
  "mpProjection" : "LambertConformal"
  "mpLambertParallel1F": 30.
  "mpLambertParallel2F": 45.
  "mpLambertMeridianF": -100.
  "mpLimitMode": "LatLon"
  "mpMinLatF": 33.
  "mpMaxLatF": 44.
  "mpMinLonF": -115.
  "mpMaxLonF": -93.
  "mpUSStateLineColor": 25
  "mpUSStateLineThicknessF": 2.
  "mpGridAndLimbOn": False
  "mpPerimOn": True
  "mpPerimLineColor": 26
  "mpPerimLineThicknessF": 2.
end create

field2 = create "field2" scalarFieldClass defaultapp
        "sfDataArray" : (file1->mound)
        "sfXCStartV" : -109.05
        "sfXCEndV": -102.05
        "sfYCStartV" : 37.
        "sfYCEndV": 41.
end create

con2 = create "con2" contourPlotClass workstation_id
  "cnScalarFieldData" : field2
  "cnFillOn": True
  "cnMonoFillColor": False
  "cnLevelSpacingF" : 8.0
  "cnSmoothingOn": True
  "cnMaxPointDistanceF" : 0.0
  "cnLineColor": 0
  "cnLineLabelsOn": False
  "cnHighLabelsOn": False
  "cnLowLabelsOn": False
  "cnInfoLabelOn": False
  "cnFillColors": (/3,7,11,15,19,23/)
; "cnFillColors": (/2,5,8,11,14,17,20,23/)
end create

overlay(mapid,con2)
draw(mapid)
frame(workstation_id)

end
