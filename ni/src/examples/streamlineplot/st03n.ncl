;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       st03n.ncl
;
;   Author:     David Brown
;           	National Center for Atmospheric Research
;           	PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description: 
;                This plot shows a StreamlinePlot overlaid on a polar
;                stereographic map projection. It illustrates some of
;                the problems with streamlines when the transformation to
;                NDC results in grid cells that vary widely in size.
;                Adjustment of certain parameters may improve the
;                appearance somewhat, but not as much as might be
;                desired. For this and other reasons, StreamlinePlot is
;                still undergoing development and its output may be
;                expected to change in the next release.
;                The data is extracted from an NMC forecast dataset for 
;                11/10/1994.
;

begin

cdfpath = ncargpath("data") + "/cdf/"
uv = addfile(cdfpath + "941110_UV.cdf","r")
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
appid = create "st03" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "st03Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./st03n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "st03Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "st03Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./st03n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "st03Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./st03n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "st03Work" documentWorkstationClass defaultapp
    "wkFileName" : "./st03n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "st03Work" imageWorkstationClass defaultapp
    "wkFileName" : "./st03n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a VectorField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

dataid = create "vfield" vectorFieldClass appid
	"vfUDataArray" : uv->u
	"vfVDataArray" : uv->v
	"vfXCStartV" : -180.0
	"vfXCEndV" : 180.0
	"vfYCStartV" : -90.0
	"vfYCEndV" : 90.0
	"vfYCStartSubsetV" : 0.0
	"vfYCEndSubsetV" : 87.5
end create

;
; Create a StreamlinePlot object, supplying the VectorField object as data
;

stid = create "streamlineplot" streamlinePlotClass wid
    "tiMainString" : "StreamlinePlot Overlaying MapPlot"
    "stVectorFieldData" : dataid
end create

mpid = create "mapplot" mapPlotClass wid
end create

overlay(mpid,stid)
draw(mpid)
frame(wid)

getvalues stid
  "stStepSizeF" : stepsize
  "stMinLineSpacingF" : spacing
end getvalues

; 
; Set the minimum arrow spacing
; 

setvalues stid
   "tiMainString" : "Setting the Minimum Arrow Spacing"
   "stMinArrowSpacingF" : 0.025
end setvalues

draw(mpid)
frame(wid)

    
; 
; Set the minimum line spacing
; 

setvalues stid
   "tiMainString" : "Smaller Line Spacing"
   "stMinLineSpacingF" : spacing * 0.5
end setvalues

draw(mpid)
frame(wid)
    
; 
; Set the step size
; 

setvalues stid
   "tiMainString" : "Smaller Step Size"
   "stStepSizeF" : stepsize * 0.5
end setvalues

draw(mpid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
