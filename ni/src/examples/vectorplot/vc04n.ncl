;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc04n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description:  This example demonstrates how to rotate a VectorPlot
;                 90 degrees left or right, while preserving the 
;                 original relationship of the data elements. 
;                 There are three parameters that must be set - two 
;                 belonging to the VectorField and one belonging to
;                 the VectorPlot. They may be set in any order. Here
;                 they are set one at a time and the intermediate result 
;                 displayed in order to show the effect of each
;                 resource.
;            

begin

path = ncargpath("data") + "/asc/"
data = asciiread(path + "uvdata0.asc",(/2,37,37/),"float")

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
appid = create "vc04" appClass defaultapp
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
  wid = create "vc04Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc04n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "vc04Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc04Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./vc04n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc04Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./vc04n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc04Work" documentWorkstationClass defaultapp
    "wkFileName" : "./vc04n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc04Work" imageWorkstationClass defaultapp
    "wkFileName" : "./vc04n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a ScalarField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vfield" vectorFieldClass appid
        "vfDataArray" : data
	"vfXCStartV" : -180.0
	"vfXCEndV" : 0.0
	"vfYCStartV" : 0.0
	"vfYCEndV" : 90.0
	"vfYCStartSubsetV" : 20.0
	"vfYCEndSubsetV" : 80.0
end create

;
; Create a VectorPlot object, supplying the ScalarField object as data
; Setting vcMonoFillArrowFillColor False causes VectorPlot to color the
; vector arrows individually based, by default, on the vector magnitude.
;

vcid = create "vectorplot" vectorPlotClass wid
    "vcVectorFieldData" : vfid
    "vcRefMagnitudeF" : 20.0
    "vcMonoFillArrowFillColor" : False
    "vcFillArrowsOn" : True
    "vcMinFracLengthF" : 0.25
end create

getvalues vcid
  "vcRefLengthF" : reflen
  "tiMainFontHeightF" : tiheight
end getvalues

setvalues vcid
   "tiMainString" : "How to Rotate a VectorPlot 90:F34:0:F:"
   "vcRefLengthF" : 1.75 * reflen
   "tiMainFontHeightF" : 0.9 * tiheight
end setvalues

draw(vcid)
frame(wid)

setvalues vfid
   "vfExchangeDimensions" : True
end setvalues

setvalues vcid
   "tiMainString" : "1:: Exchange the Dimensions"
end setvalues

draw(vcid)
frame(wid)

setvalues vfid
   "vfExchangeUVData" : True
end setvalues

setvalues vcid
   "tiMainString" : "2:: Exchange the U and V Data"
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "3a:: Reverse the Y-Axis for Clockwise Rotation"
   "trYReverse" : True
end setvalues

draw(vcid)
frame(wid)

setvalues vcid
   "tiMainString" : "3b:: Or the X-Axis for Counter-Clockwise Rotation"
   "trYReverse" : False
   "trXReverse" : True
end setvalues

draw(vcid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
