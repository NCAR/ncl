;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc03n.ncl
;
;   Author:     David Brown
;           	National Center for Atmospheric Research
;           	PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description: Manipulates the FillArrow resources to demonstrate some
;                of the possible stylistic variations on the appearance 
;                of the filled vector arrows.
;                The data is extracted from an NMC forecast dataset for 
;                11/10/1994.
;

begin

path = ncargpath("data") + "/asc/"
data = asciiread(path + "uvdata0.asc",(/2,37,37/),"float")

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
appid = create "vc03" appClass defaultapp
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
  wid = create "vc03Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc03n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "vc03Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "vc03Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./vc03n.ps"
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
        "vfDataArray" : data
	"vfXCStartV" : -180.0
	"vfXCEndV" : 0.0
	"vfYCStartV" : 0.0
	"vfYCEndV" : 90.0
	"vfYCStartSubsetV" : 20.0
	"vfYCEndSubsetV" : 80.0
end create

;
; Create a VectorPlot object, supplying the VectorField object as data
;

vcid = create "vectorplot" vectorPlotClass wid
    "vcVectorFieldData" : dataid
    "vcRefMagnitudeF" : 20.0
    "vcFillArrowsOn" : True
    "vcMinFracLengthF" : 0.2
    "tiMainString" : "Filled Arrow VectorPlot"
end create

getvalues vcid
  "vcRefLengthF" : reflen
end getvalues

setvalues vcid
   "vcRefLengthF" : 1.5 * reflen
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "Variation #1:: Constant Width"
   "vcFillArrowWidthF" : 0.15
   "vcFillArrowMinFracWidthF" : 1.0
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "Variation #2"
   "vcFillArrowMinFracWidthF" : 0.25
   "vcFillArrowHeadMinFracXF" : 0.0
   "vcFillArrowWidthF" : 0.2
   "vcFillArrowHeadXF" : 0.8
   "vcFillArrowHeadInteriorXF" : 0.7
   "vcFillArrowHeadYF" : 0.2
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "Variation #3"
   "vcRefLengthF" : 1.2 * reflen
   "vcFillArrowWidthF" : 0.3
   "vcFillArrowHeadXF" : 0.4
   "vcFillArrowHeadInteriorXF" : 0.35
   "vcFillArrowHeadYF" : 0.3
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "Variation #4"
   "vcRefLengthF" : 1.2 * reflen
   "vcFillArrowWidthF" : 0.2
   "vcFillArrowHeadXF" : 1.0
   "vcFillArrowHeadInteriorXF" : 1.0
   "vcFillArrowHeadYF" : 0.2
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "Variation #5"
   "vcRefLengthF" : 0.8 * reflen
   "vcFillArrowWidthF" : 0.2
   "vcFillArrowHeadXF" : 1.5
   "vcFillArrowHeadInteriorXF" : 1.0
   "vcFillArrowHeadYF" : 0.5
end setvalues

draw(vcid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
