;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp01n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Translated to NCL by Ed Stautler
;
;   Date:       Fri Oct 14 11:42:41 MDT 1994
;
;   Description:    Demonstrates basic MapPlot capabilities
;

begin

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the Contour resources that remain fixed
; throughout the life of the Contour object.
;
    appid = create "mp01" appClass defaultapp
        "appUsrDir" : "."
        "appDefaultParent" : True
    end create
;
; Default is to display output to an X workstation
;
wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "mp01Work" ncgmWorkstationClass defaultapp
          "wkMetaName" : "mp01n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
     wid = create "mp01Work" windowWorkstationClass defaultapp
       "wkPause" : True
     end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "mp01Work" psWorkstationClass defaultapp
      "wkPSFileName" : "mp01n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "mp01Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "mp01n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "mp01Work" documentWorkstationClass defaultapp
      "wkFileName" : "mp01n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "mp01Work" imageWorkstationClass defaultapp
      "wkFileName" : "mp01n"
      "wkFormat" : wks_type
    end create
  end if

;
; Draw the default MapPlot object
;
    mapid = create "Map0" mapPlotClass wid
        "pmTitleDisplayMode" : "always"
        "tiMainString" : "mp01n - Frame 1"
    end create

    draw(mapid)
    frame(wid)
;
; Change some projection resources, add color fill, and
; all the outlines (Geophysical, National, and US States).
;
    setvalues mapid
        "tiMainString" : "mp01n - Frame 2"
        "vpYF" : 0.9
        "vpHeightF" : 0.8
        "mpFillOn" : True
        "mpOutlineBoundarySets" : "allBoundaries"
        "mpProjection" : "orthographic"
        "mpPerimOn" : True
        "mpCenterLatF" : 10.0
        "mpCenterLonF" : -90.0
        "mpCenterRotF" : 45.0
    end setvalues

    draw(mapid)
    frame(wid)
;
; Use the national color set and limit the projection, 
; using latlon boundaries.
;
    setvalues mapid
        "tiMainString" : "mp01n - Frame 3"
        "mpFillBoundarySets" : "national"
        "mpLimitMode" : "latlon"
        "mpMinLatF" : -60.0
        "mpMaxLatF" : 60.0
        "mpMinLonF" : -135.0
        "mpMaxLonF" : -45.0
    end setvalues

    draw(mapid)
    frame(wid)
;
; Polar stereographic projection, change the grid spacing to 10 degrees
;
    setvalues mapid
        "tiMainString" : "mp01n - Frame 4"
        "mpProjection" : "stereographic"
        "mpGridSpacingF" : 10.
        "mpMinLatF" : 20.0
        "mpMaxLatF" : 90.0
        "mpMinLonF" : 0.0
        "mpMaxLonF" : 360.0
        "mpCenterLatF" : 90.0
    end setvalues

    draw(mapid)
    frame(wid)
;
; Satellite projection using the angle limit method;
; color US States only individually.
;
    setvalues mapid
        "tiMainString" : "mp01n - Frame 5"
        "mpFillBoundarySets" : "geophysicalAndUSStates"
        "mpProjection" : "satellite"
        "mpLimitMode" : "angles"
        "mpLeftAngleF" : 45.0
        "mpRightAngleF" : 45.0
        "mpBottomAngleF" : 45.0
        "mpTopAngleF" : 45.0
        "mpCenterLatF" : 20.0
        "mpSatelliteDistF" : 1.75
    end setvalues

    draw(mapid)
    frame(wid)
;
; Destroy the objects created, close the HLU library and exit.
;

    delete(mapid)
    delete(wid)
    delete(appid)

end
