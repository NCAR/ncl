;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1996                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc08n.ncl
;
;   Author:     Bob Lackman
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Wed Jan 24, 1996
;
;   Description:    Plots wind vectors at a grid stride of 3.
;                   Vectors are colored by wind speed.
;
begin
;
; Direct the output to an X11 window.
;
  NCGM=0
  X11=1
  PS=0

  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
      wid = create "vc08Work" ncgmWorkstationClass defaultapp
	"wkColorMap" : "uniform"
	"wkMetaName" : "./vc08n.ncgm"
      end create
  else
    if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
      wid = create "vc08Work" xWorkstationClass defaultapp
	"wkColorMap" : "uniform"
	"wkPause" : True
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
	wid = create "vc08Work" psWorkstationClass defaultapp
	  "wkColorMap" : "uniform"
	  "wkPSFileName" : "./vc08n.ps"
	end create
      end if
    end if
  end if
;
; Read the input data and create a data object.
;
; Get current location of data directory in NG install
;
datadir= ncargpath("data")
datafile=datadir + "/cdf/ex01B1_uv300.hs.nc"

vf =  addfile (datafile,"r")    ; Load netCDF file "vf"
;
; create a VectorField data object, U and V are 4-D arrays which are a
; function of time, level, latitude and longitude.  Use the 1st time & level.
;
vfield = create "VectorField" vectorFieldClass defaultapp
    "vfUDataArray" : vf->U(0,0,:,:)
    "vfVDataArray" : vf->V(0,0,:,:)
;
; Specify longitude coordinates 
;
    "vfXArray" : vf->lon
;
; Specify latitude coordinates
;
    "vfYArray" : vf->lat
;
; Specify a stride of 3 in both dimensions
;
    "vfXCStride" : 3          ; i.e., plot a vector at every 4th point.
    "vfYCStride" : 3
  end create
;
; Create a VectorPlot object "vcid" and connect the data object "vfield".
;
vcid = create "vectorplot" vectorPlotClass wid
    "vcVectorFieldData":      vfield
    "vcMinFracLengthF" : 0.33
    "vcRefLengthF" : 0.025
    "vcRefAnnoString1" : "$VMG$ " + vf@sub_title_rhs
    "vcMonoLineArrowColor" :  False
    "pmLabelBarDisplayMode" : "always"
    "pmLabelBarSide" : "bottom"
    "lbOrientation" : "horizontal" 
    "lbTitleString" : vf@sub_title_rhs
    "vcLevelColors" : (/ 26,28,30,33,36,39,42,45,48,51,54,56,58,60/)
;    "vcLineArrowThicknessF" : 1.75
  end create
;
; Create a map object
;
  mapid = create "map" mapPlotClass wid 
    "vpXF" : 0.05
    "vpWidthF" : 0.9
    "vpYF" : 0.85
    "mpGridAndLimbDrawOrder" : "predraw"
  end create
  overlay(mapid,vcid)
  draw(mapid)
;
; Create a text item object as a main title
;
  txid1 = create "main" textItemClass wid
    "txPosXF" : 0.5
    "txPosYF" : 0.85
    "txJust"    : "CENTERCENTER"
    "txString" : vf@title
    "txFontHeightF" : .030
    "txFont" : 25
  end create

;
; Create a subheader text item object
;
  txid2 = create "text" textItemClass wid
    "txPosXF" : 0.5
    "txPosYF" : 0.80
    "txJust"    : "CENTERCENTER"
    "txString" : "Wind                                                          300mb                                                       (m/s)"
    "txFontHeightF" : .015
    "txFont" : 25
  end create
  draw(txid1)
  draw(txid2)
  frame(wid)
;
; Destroy the workstation object and exit.
;
  delete(wid)
end
