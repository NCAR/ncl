;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  2002                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp07n.ncl
;
;   Author:     Mary Haley
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Mar  4 10:42:13 MST 2002
;
;
;   Description: Shows how to draw high-resolution coastlines using the
;   RANGS (Regionally Accessible Nested Global Shorelines), developed
;   by Rainer Feistel from Wessel and Smith's GSHHS (Global
;   Self-consistent Hierarchical High-resolution Shoreline) database.
;   To access the RANGS/GSHHS database, you must first download it from
;   Rainer Feistel's web site at
;   http://www.io-warnemuende.de/homepages/rfeistel/index.html.  Right
;   before the section entitled "Some WWW Links", you should see a
;   little table with ten *.zip files to download:
;
;     rangs(0).zip gshhs(0).zip
;     rangs(1).zip         gshhs(1).zip
;     rangs(2).zip         gshhs(2).zip
;     rangs(3).zip         gshhs(3).zip
;     rangs(4).zip         gshhs(4).zip
;
;   You must download all ten of these files, unzip them, and either
;   put them in the default directory
;   "$NCARG_ROOT/lib/ncarg/database/rangs", or put them somewhere else
;   and set the environment variable NCARG_RANGS to this directory. The
;   files take up about 140 megabytes, unzipped. Once you have the
;   files in the appropriate location, then set the mpDataBaseVersion
;   resource to "RANGS_GSHHS" to create maps using this database. You
;   should not use this database to plot maximal area plots, because 1)
;   you will get horizontal lines through your plot, and 2) it takes a
;   long time.
;

begin
;
; Default is to display output to an X workstation
;
  NCGM=0
  X11=1
  PS=0

  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
      wks = create "mp07Work" ncgmWorkstationClass defaultapp
        "wkMetaName"          : "./mp07n.ncgm"
      end create
  else
    if (X11 .eq. 1) then 
;
; Create an XWorkstation object.
;
      wks = create "mp07Work" xWorkstationClass defaultapp
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
        wks = create "mp07Work" psWorkstationClass defaultapp
          "wkPSFileName"      : "./mp07n.ps"
        end create
      end if
    end if
  end if

  mapid = create "map"  mapPlotClass wks
    "mpProjection"         : "CylindricalEquidistant"
;
; "LowRes" is the default database, also sometimes known
; as "Ncarg4_0".
;
    "mpDataBaseVersion"    : "LowRes"     ; This is the default

    "vpWidthF"             : 0.80 ; Make map larger in view port.
    "vpHeightF"            : 0.80
    "vpXF"                 : 0.15
    "vpYF"                 : 0.90

    "mpLimitMode"          : "LatLon"    ; Zoom in on map.

    "mpMinLatF"            :  30.
    "mpMaxLatF"            :  60.
    "mpMinLonF"            : -15.
    "mpMaxLonF"            :  15.

    "pmTickMarkDisplayMode": "Always"   ; Display map tickmarks (lat/lon
                                        ; labels)
  end create

  draw(mapid)     ; Draw map
  frame(wks)      ; Advance frame

;
; Set the resource indicating you want to use the high-resolution
; RANGS/GSHHS database.
;
  setvalues mapid
    "mpDataBaseVersion"    : "RANGS_GSHHS" 
  end setvalues

  draw(mapid)     ; Draw map
  frame(wks)      ; Advance frame
end
