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
NCGM=0
X11=1
PS=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "st03Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./st03n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "st03Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "st03Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./st03n.ps"
      end create
    end if
  end if
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
