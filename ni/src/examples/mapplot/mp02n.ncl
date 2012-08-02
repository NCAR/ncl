;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp02n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Translated to NCL by Ed Stautler
;
;   Date:       Fri Oct 14 11:42:41 MDT 1994
;
;   Description:    Demonstrates individual control of MapPlot areas
;

begin

;
; String arrays for specifying areas
;
    fill_specs = (/ "mexico","bolivia","brazil","nicaragua",  \
                "cuba","haiti","canada"                   /)

    outline_specs = (/ "argentina","paraguay","colombia",     \
               "us-colorado","us-texas","us-kentucky" /)

    mask_specs = (/ "us-colorado","us-texas","us-kentucky",   \
            "bolivia","paraguay","nicaragua","oceans" /)
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the Contour resources that remain fixed
; throughout the life of the Contour object.
;
    appid = create "mp02" appClass defaultapp
        "appUsrDir" : "./"
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
        wid = create "mp02Work" ncgmWorkstationClass defaultapp
            "wkMetaName" : "./mp02n.ncgm"
        end create
    end if
    if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
      wid = create "mp02Work" windowWorkstationClass defaultapp
          "wkPause" : True
      end create
    end if
    if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
      wid = create "mp02Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./mp02n.ps"
      end create
    end if
    if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
      wid = create "mp02Work" pdfWorkstationClass defaultapp
        "wkPDFFileName" : "./mp02n.pdf"
      end create
    end if
    if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
      wid = create "mp02Work" documentWorkstationClass defaultapp
        "wkFileName" : "./mp02n"
        "wkFormat" : wks_type
      end create
    end if
    if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
      wid = create "mp02Work" imageWorkstationClass defaultapp
        "wkFileName" : "./mp02n"
        "wkFormat" : wks_type
      end create
    end if

;
; Create a plot focusing on North and South America.
; Outlines are on by default, turn fill on.
; By default the geophysical boundary set is used both for outline and fill.
;
    mapid = create "Map0" mapPlotClass wid
        "pmTitleDisplayMode" : "Always"
        "tiMainString" : "mp02n - Frame 1"
        "mpFillOn" : True
        "mpProjection" : "Orthographic"
        "mpPerimOn" : True
        "mpCenterLatF" : 10.0
        "mpCenterLonF" : -90.0
        "mpCenterRotF" : 45.0
        "mpLimitMode" : "LatLon"
        "mpMinLatF" : -60.0
        "mpMaxLatF" : 60.0
        "mpMinLonF" : -135.0
        "mpMaxLonF" : -45.0
;
; Highlight selected countries using their "political" color.
;
        "mpFillAreaSpecifiers" : fill_specs
    end create

    draw(mapid)
    frame(wid)
;
; Individually outline some other countries and some US states.
;
    setvalues mapid
        "tiMainString" : "mp02n - Frame 2"
        "mpOutlineSpecifiers" : outline_specs
    end setvalues

    draw(mapid)
    frame(wid)
;
; Turn off the base geophysical set for outlines and fill, leaving only
; the specified areas.
; Also change the specification, 'canada' to 'canada*', 
; in order to draw all areas belonging to Canada.
; Note that another color, mpDefaultFillColor, is used for all areas
; within the map projection that are otherwise not drawn, including the
; oceans. If you look closely, you will see that the Canadian lakes 
; are not drawn in the color used in the previous frame for the ocean.
; The wild card specification, 'canada*', picks up all the lakes of Canada. 
; Lakes are drawn using mpInlandWaterFillColor, which is, by default, 
; set to the same color as mpOceanFillColor.
;
    fill_specs(6) = "canada*"
    setvalues mapid
        "tiMainString" : "mp02n - Frame 3"
        "mpFillBoundarySets" : "NoBoundaries"
        "mpOutlineBoundarySets" : "NoBoundaries"
        "mpFillAreaSpecifiers" : fill_specs
    end setvalues

    draw(mapid)
    frame(wid)
;
; You can also specify area groupings using certain predefined 
; string constants: set 'continents' on to demonstrate.
; Masking an area is different from not explicitly drawing it. To enable
; masking you must explicitly turn area masking and then create an area
; mask specification list containing the name of each area to be masked.
; There is an order of precedence for fill and masking. Explicitly 
; named areas take precedence over area groupings, and small areas take 
; precedence over enclosing larger areas. Otherwise masking takes 
; precedence over filling.
; >>> Masking or filling individual US states causes processing time and
; >>> memory requirements to increase substantially. Hopefully the 
; >>> performance can be improved before the release.
;
    fill_specs(0) = "continents";
    fill_specs(1) = "us";
    setvalues mapid
        "tiMainString" : "mp02n - Frame 4"
        "mpFillBoundarySets" : "NoBoundaries"
        "mpFillAreaSpecifiers" : fill_specs
        "mpAreaMaskingOn" : True
        "mpMaskAreaSpecifiers" : mask_specs
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
