;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp03n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Translated to NCL by Ed Stautler
;
;   Date:       Fri Oct 14 11:42:41 MDT 1994
;
;   Description:    Demonstrates MapPlot masking; loosely emulates the
;           LLU example 'colcon'
;


;
; Read data from input file 'mp03n.asc'. 
;
function getdat (
        m : integer,
        n : integer
)
begin
	return(asciiread("mp03n.asc",(/m,n/),"float"))
end


begin

    M = 50
    N = 50

    mask_specs = (/ "oceans" /)
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the Contour resources that remain fixed
; throughout the life of the Contour object.
;
    appid = create "mp03" appClass defaultapp
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
        wid = create "mp03Work" ncgmWorkstationClass defaultapp
           "wkMetaName" : "./mp03n.ncgm"
        end create
    else
      if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
        wid = create "mp03Work" xWorkstationClass defaultapp
            "wkPause" : True
        end create
      else
        if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
          wid = create "mp03Work" psWorkstationClass defaultapp
           "wkPSFileName" : "./mp03n.ps"
          end create
        end if
      end if
    end if

;
; Call the routine 'getdat' to create the first array of contour
; data. Create a ScalarField data object and hand it the data created by
; 'getdat'. Define the extent of the data coordinates as the whole globe 
; 
    dataid = create "Gendat" scalarFieldClass appid
        "sfDataArray" : getdat(M,N)
        "sfXCStartV" : -180
        "sfXCEndV" : 180
        "sfYCStartV" : -90
        "sfYCEndV" : 90
    end create
;
; Create a Contour object, supplying the ScalarField object as data,
; and setting the size of the viewport.
;
    cnid = create "Contour1" contourPlotClass wid
        "cnScalarFieldData" : dataid
        "cnLabelDrawOrder" : "postdraw"
    end create
;
; Create a MapPlot object, setting the fill to draw over the main draw,
; and masking out the oceans.
;
    mapid = create "Map1" mapPlotClass wid
        "mpFillOn" : True
        "pmTitleDisplayMode" : "always"
        "tiMainString" : "mp03n"
        "mpFillDrawOrder" : "postdraw"
        "mpAreaMaskingOn" : True
        "mpMaskAreaSpecifiers" : mask_specs
    end create
;
; Overlay the Contour object on the MapPlot object
;
    overlay(mapid, cnid)
    
    draw(mapid)
    frame(wid)
;
; Destroy the objects created, close the HLU library and exit.
;
    delete(mapid)
    delete(wid)
end
