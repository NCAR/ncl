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
      "appDefaultParent" : "True"
  end create

;
; If zoom = 0 then this script will animate the original map.
; If zoom = 1 then this script will animate a zoomed map.
;

  ZOOM = 0

;
; Output to an ncgm.
;

  NCGM=1
  X11=0
  PS=0

;
; Create an ncgmWorkstation object.
;
  if (NCGM .eq. 1) then
      wid = create "vc09Work" ncgmWorkstationClass defaultapp
        "wkMetaName" : "./vc09n.ncgm"
        "wkColorMap" : "default"
      end create
  else
;
; Create an XWorkstation object.
;
    if (X11 .eq. 1) then
      wid = create "vc09Work" xWorkstationClass defaultapp
            "wkPause" : "True"
        "wkColorMap" : "default"
      end create
    else
;
; Create a PSWorkstation object.
;
      if (PS .eq. 1) then
        wid = create "vc09Work" psWorkstationClass defaultapp
          "wkPSFileName" : "vc09n.ps"
        end create
      end if
    end if
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
    "vfXCEndV"     : uf->lon(dimsizes(uf->lon) - 1)
    "vfYCEndV"     : uf->lat(dimsizes(uf->lat) - 1)
    "vfXCStride"   : 2
    "vfYCStride"   : 2
    "vfMissingUValueV" : -9999.0
  end create

sfield = create "ScalarField" scalarFieldClass appid
    "sfDataArray" : pf->p(0,:,:) / 100.0
    "sfXCStartV"  : pf->lon(0)
    "sfYCStartV"  : pf->lat(0)
    "sfXCEndV"    : pf->lon(dimsizes(pf->lon) - 1)
    "sfYCEndV"    : pf->lat(dimsizes(pf->lat) - 1)
    "sfXCStride"  : 2
    "sfYCStride"  : 2
    "sfMissingValueV" : -9999.0
  end create

sfield2 = create "ScalarField2" scalarFieldClass appid
    "sfDataArray" : (tf->t(0,:,:)  - 273.15)*9.0/5.0 +32.0
    "sfXCStartV"  : tf->lon(0)
    "sfYCStartV"  : tf->lat(0)
    "sfXCEndV"    : tf->lon(dimsizes(tf->lon) - 1)
    "sfYCEndV"    : tf->lat(dimsizes(tf->lat) - 1)
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
        "vpYF"          : 0.9
    "vpWidthF"      : 0.8
    "vpHeightF"     : 0.8
        "vpUseSegments" : "true"
        "mpMinLatF"     : MinLat
        "mpMaxLatF"     : MaxLat
        "mpMinLonF"     : MinLon
        "mpMaxLonF"     : MaxLon
        "mpCenterLonF"  : -100.0
        "mpCenterLatF"  : 40.0
    "mpGridAndLimbDrawOrder" : "predraw"
  end create

  cnid = create "contourplot" contourPlotClass wid
        "cnFillOn"              : "true"
        "cnLinesOn"             : "false"
        "cnFillDrawOrder"       : "predraw"
        "cnScalarFieldData"     : sfield
        "pmLabelBarDisplayMode" : "always"
        "pmLabelBarHeightF"     : 0.075
        "pmLabelBarWidthF"      : 0.6
        "lbOrientation"         : "horizontal"
        "lbPerimOn"             : "False"
        "pmLabelBarSide"        : "top"
  end create
;
; Create a VectorPlot object using the above data field.
;
  vcid = create "vectorplot" vectorPlotClass wid
        "vcUseScalarArray"      : "true"
        "vcVectorFieldData"     : vfield
        "vcScalarFieldData"     : sfield2
        "vcMinFracLengthF"      : 0.33
        "vcMonoLineArrowColor"  : "false"
        "vcVectorDrawOrder"     : "predraw"
        "pmLabelBarDisplayMode" : "always"
        "pmLabelBarWidthF"      : 0.1
        "lbPerimOn"             : "False"
  end create

  tiid = create "Titles" titleClass wid
    "vpXF"      : 0.03
        "vpYF"      : 0.9
    "vpWidthF"  : 0.8
    "vpHeightF" : 0.8
        "tiMainFuncCode" : "~"
        "tiMainString" : vf->reftime  + " + " + vf->timestep(0)
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

          setvalues tiid
            "tiMainString" : vf->reftime + " + " + vf->timestep(i)
          end setvalues
    
          draw(mapid)
          draw(tiid)
          frame(wid)

    end if
  end do

;
; Destroy the workstation object and exit.
;

  delete(wid)
end
