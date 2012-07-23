;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp05n.ncl
;
;   Author:     Mary Haley
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Thu Sep 28 13:38:43 MDT 1995
;
;   Description:  Draws each of the ten map projections, with and 
;                 without fills.
;

begin

   mapid = new(10,graphic)
;
; Create an application object.
;
    appid = create "mp05" appClass defaultapp
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
        wid = create "mp05Work" ncgmWorkstationClass defaultapp
           "wkMetaName" : "./mp05n.ncgm"
        end create
    end if
    if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
      wid = create "mp05Work" windowWorkstationClass defaultapp
          "wkPause" : True
      end create
    end if
    if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
      wid = create "mp05Work" psWorkstationClass defaultapp
       "wkPSFileName" : "./mp05n.ps"
      end create
    end if
    if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
      wid = create "mp05Work" pdfWorkstationClass defaultapp
       "wkPDFFileName" : "./mp05n.pdf"
      end create
    end if
    if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
      wid = create "mp05Work" documentWorkstationClass defaultapp
       "wkFileName" : "./mp05n"
       "wkFormat" : wks_type
      end create
    end if
    if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
      wid = create "mp05Work" imageWorkstationClass defaultapp
       "wkFileName" : "./mp05n"
       "wkFormat" : wks_type
      end create
    end if
;
; Create a textItem object.
;
    txid = create "TextItem" textItemClass wid
      "vpXF":       .2
      "vpYF":       .95
      "txFont":     26
      "txString":   "Maximal-area projections of all types"
    end create
;
; Create and draw ten different map objects and display them in the
; same frame.
;
    do i = 0,9
      mapid(i) = create "map" + i mapPlotClass wid end create
      draw(mapid(i))
    end do
    draw(txid)
    frame(wid)  
;
; Draw each projection individually and fill the countries.
; 
    setvalues mapid
        "mpEllipticalBoundary":  True
        "mpFillOn":              True
        "mpLabelsOn":            False
        "mpGridMaskMode":        "MaskNotOcean"
        "mpGridLineThicknessF":  1.1
        "vpXF":                  0.1
        "vpYF":                  0.9
        "vpWidthF":              0.8
        "vpHeightF":             0.8
    end setvalues
    do i = 0,9
      draw(mapid(i))
      frame(wid)
    end do
;
; Destroy the objects created, close the HLU library and exit.
;
    delete(wid)
end
