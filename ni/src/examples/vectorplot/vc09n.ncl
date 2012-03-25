;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1996                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc09n.ncl
;   Author:     David Brown
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Wed Jan 24, 1996
;
;   Description:    Does an animation of the January 1996 snow storm.
;                   Wind vectors colored by temperature are animated
;                   over a pressure field contour plot.
;

begin

;
; Create an application object.  It will look for a resource file
; named vc09.res
;

  appid = create "vc09" appClass defaultapp
      "appUsrDir" : "./"
      "appDefaultParent" : True
  end create

;
; If zoom = 0 then this script will animate the original map.
; If zoom = 1 then this script will animate a zoomed map.
;

  ZOOM = 0

;
; Output to an ncgm.
;
  wks_type = "ncgm"

;
; Create an ncgmWorkstation object.
;
  if (str_lower(wks_type).eq."ncgm") then
      wid = create "vc09Work" ncgmWorkstationClass defaultapp
        "wkMetaName" : "./vc09n.ncgm"
        "wkColorMap" : "temp1"
      end create
  end if
;
; Create an X11 workstation.
;
  if (str_lower(wks_type).eq."x11") then
    wid = create "vc09Work" windowWorkstationClass defaultapp
          "wkPause" : True
          "wkColorMap" : "temp1"
    end create
  end if
;
; Create an older-style PostScript workstation.
;
  if (str_lower(wks_type).eq."oldps") then
    wid = create "vc09Work" psWorkstationClass defaultapp
      "wkPSFileName" : "vc09n.ps"
      "wkColorMap" : "temp1"
    end create
  end if
;
; Create an older-style PDF workstation.
;
  if (str_lower(wks_type).eq."oldpdf") then
    wid = create "vc09Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "vc09n.pdf"
      "wkColorMap" : "temp1"
    end create
  end if
;
; Create a cairo PS/PDF Workstation object.
;
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
    wid = create "vc09Work" documentWorkstationClass defaultapp
      "wkFileName" : "vc09n"
      "wkFormat" : wks_type
      "wkColorMap" : "temp1"
    end create
  end if
;
; Create a cairo PNG Workstation object.
;
  if (str_lower(wks_type).eq."png") then
    wid = create "vc09Work" imageWorkstationClass defaultapp
      "wkFileName" : "vc09n"
      "wkFormat" : wks_type
      "wkColorMap" : "temp1"
    end create
  end if

dir = "./"
dir = ncargpath("data")
uf = addfile(dir+"/cdf/Ustorm.cdf","r")
vf = addfile(dir+"/cdf/Vstorm.cdf","r")
pf = addfile(dir+"/cdf/Pstorm.cdf","r")
tf = addfile(dir+"/cdf/Tstorm.cdf","r")

vfield = create "VectorField" vectorFieldClass appid
    "vfUDataArray" : uf->u(0,:,:)
    "vfVDataArray" : vf->v(0,:,:)
    "vfXCStartV"   : uf->lon(0)
    "vfYCStartV"   : uf->lat(0)
    "vfXCEndV"   : uf->lon(filevardimsizes(uf,"lon")-1)
    "vfYCEndV"   : uf->lat(filevardimsizes(uf,"lat")-1)
    "vfXCStride"   : 2
    "vfYCStride"   : 2
    "vfMissingUValueV" : -9999.0
  end create

sfield = create "ScalarField" scalarFieldClass appid
    "sfDataArray" : pf->p(0,:,:) / 100.0
    "sfXCStartV"   : pf->lon(0)
    "sfYCStartV"   : pf->lat(0)
    "sfXCEndV"   : pf->lon(filevardimsizes(pf,"lon")-1)
    "sfYCEndV"   : pf->lat(filevardimsizes(pf,"lat")-1)
    "sfXCStride"  : 2
    "sfYCStride"  : 2
    "sfMissingValueV" : -9999.0
  end create

sfield2 = create "ScalarField2" scalarFieldClass appid
    "sfDataArray" : (tf->t(0,:,:)  - 273.15)*9.0/5.0 +32.0
    "sfXArray"  : tf->lon
    "sfYArray"  : tf->lat
    "sfXCStartV"   : tf->lon(0)
    "sfYCStartV"   : tf->lat(0)
    "sfXCEndV"   : tf->lon(filevardimsizes(tf,"lon")-1)
    "sfYCEndV"   : tf->lat(filevardimsizes(tf,"lat")-1)
    "sfXCStride"  : 2
    "sfYCStride"  : 2
    "sfMissingValueV" : -9999.0
  end create

;
;  To zoom in on a certain area of the first plot adjust the following
;  four numbers.
;
;  The following four numbers will cause the plots to display the
;  entire United States.
;

 if (ZOOM .eq. 0) then
     MinLat = 18.0
     MaxLat = 65.
     MinLon = -128.
     MaxLon = -58.
 else

;
;  The Following four numbers will zoom in on the great lakes region of 
;  the United States.
;

   if (ZOOM .eq. 1) then
       MinLat = 40.0
       MaxLat = 60.
       MinLon = -100.
       MaxLon = -58.
   end if
 end if

;
; Create a map object
;
  mapid = create "map" mapPlotClass wid 
        "vpXF"          : 0.03
        "vpYF"          : 0.85
        "vpWidthF"      : 0.8
        "vpHeightF"     : 0.8
        "vpUseSegments" : True
        "mpMinLatF"     : MinLat
        "mpMaxLatF"     : MaxLat
        "mpMinLonF"     : MinLon
        "mpMaxLonF"     : MaxLon
        "mpCenterLonF"  : -100.0
        "mpCenterLatF"  : 40.0
    "mpGridAndLimbDrawOrder" : "predraw"
  end create

  cnid = create "contourplot" contourPlotClass wid
        "cnFillOn"              : True
        "cnLinesOn"             : False
        "cnFillDrawOrder"       : "predraw"
        "cnScalarFieldData"     : sfield
        "pmLabelBarDisplayMode" : "always"
        "pmLabelBarHeightF"     : 0.075
        "pmLabelBarWidthF"      : 0.6
        "lbOrientation"         : "horizontal"
        "lbPerimOn"             : False
        "pmLabelBarSide"        : "top"
  end create
;
; Create a VectorPlot object using the above data field.
;
  vcid = create "vectorplot" vectorPlotClass wid
        "vcUseScalarArray"      : True
        "vcVectorFieldData"     : vfield
        "vcScalarFieldData"     : sfield2
        "vcMinFracLengthF"      : 0.33
        "vcMonoLineArrowColor"  : False
        "vcVectorDrawOrder"     : "predraw"
        "pmLabelBarDisplayMode" : "always"
        "pmLabelBarWidthF"      : 0.1
        "lbPerimOn"             : False
  end create

  title_id1 = create "Titles" titleClass wid
      "vpXF"      : 0.03
      "vpYF"      : 0.85
      "vpWidthF"  : 0.8
      "vpHeightF" : 0.8
      "tiMainFuncCode" : "~"
      "tiMainFont" : 25
      "tiMainString" : vf->reftime  + " + " + vf->timestep(0)
  end create

  title_id2 = create "Titles" titleClass wid
      "vpXF"      : 0.03
      "vpYF"      : 0.9
      "vpWidthF"  : 0.8
      "vpHeightF" : 0.8
      "tiMainFont" : 25
      "tiMainString" : "January 1996 Snow Storm"
  end create

  txid1 = create "text" textItemClass wid
      "txPosXF"      : 0.25
      "txPosYF"      : 0.08
      "txFontHeightF"    : 0.015
      "txString" : "Contours represent pressure field.:C:Vectors represent wind direction:C:colored by temperature."
  end create

  overlay(mapid,cnid)
  overlay(mapid,vcid)

  time = vf->timestep

  do i = 2*(dimsizes(time) - 1)/3,(dimsizes(time) - 1)

    if (time(i) .ne.102 .and. time(i) .ne.222 .and. time(i) .ne.216) then
      setvalues vfield
        "vfUDataArray" : uf->u(i,:,:)
        "vfVDataArray" : vf->v(i,:,:)   
      end setvalues

      setvalues sfield
        "sfDataArray" : pf->p(i,:,:) / 100.0
      end setvalues

      setvalues sfield2
        "sfDataArray" : (tf->t(i,:,:)  - 273.15)*9.0/5.0 +32.0
      end setvalues

          setvalues title_id1
            "tiMainString" : vf->reftime + " + " + vf->timestep(i)
          end setvalues
    
          draw(mapid)
          draw(title_id1)
          draw(title_id2)
          draw(txid1)
          frame(wid)

    end if
  end do

;
; Destroy the workstation object and exit.
;

  delete(wid)
end
