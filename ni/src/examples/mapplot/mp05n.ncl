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
    NCGM=0
    X11=1
    PS=0

    if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
        wid = create "mp05Work" ncgmWorkstationClass defaultapp
           "wkMetaName" : "./mp05n.ncgm"
        end create
    else
      if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
        wid = create "mp05Work" xWorkstationClass defaultapp
            "wkPause" : True
        end create
      else
        if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
          wid = create "mp05Work" psWorkstationClass defaultapp
           "wkPSFileName" : "./mp05n.ps"
          end create
        end if
      end if
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
