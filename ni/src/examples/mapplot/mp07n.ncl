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
  wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
      wks = create "mp07Work" ncgmWorkstationClass defaultapp
        "wkMetaName"          : "./mp07n.ncgm"
      end create
  end if
  if (str_lower(wks_type).eq."x11") then 
;
; Create an X11 workstation.
;
    wks = create "mp07Work" windowWorkstationClass defaultapp
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wks = create "mp07Work" psWorkstationClass defaultapp
      "wkPSFileName"      : "./mp07n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wks = create "mp07Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "./mp07n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wks = create "mp07Work" documentWorkstationClass defaultapp
      "wkFileName" : "./mp07n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wks = create "mp07Work" imageWorkstationClass defaultapp
      "wkFileName" : "./mp07n"
      "wkFormat" : wks_type
    end create
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
