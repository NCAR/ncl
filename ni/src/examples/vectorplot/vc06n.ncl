;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc06n.ncl
;
;   Author:     David Brown
;           	National Center for Atmospheric Research
;           	PO 3000, Boulder, Colorado
;
;
;   Date:       Mon May 20 17:29:59 MDT 1996
;
;   Description:  This example demonstrates several features of 
;                 VectorPlot:
;                 1) Use of the vcMinDistanceF resource to reduce 
;                    the crowding of vector arrows in regions where
;                    the transformation compresses the distance 
;                    between adjacent grid points.
;                 2) Use of a scalarfield to determine the color
;                    of the vector arrow fill.
;                 3) VectorPlot as an overlay of MapPlot.
;                 Successive frames show the result of increasing the
;                 vcMinDistanceF in small steps. At the same time the
;                 MapTransformation mpCenterLonF resource is decreased in
;                 steps, causing the orthographic projection of the
;                 northern hemisphere to appear to rotate when the 
;                 output is animated. Increasing the value of the
;                 FRAME_COUNT variable will result in a smoother
;                 animation.
;            

begin

FRAME_COUNT=13

cdfpath = ncargpath("data") + "/cdf/"
uv = addfile(cdfpath + "941110_UV.cdf","r")
p = addfile(cdfpath + "941110_P.cdf","r")

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
appid = create "vc06" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

;
; Default is to display output to an X workstation
;
wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "vc06Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc06n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "vc06Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc06Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./vc06n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc06Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./vc06n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc06Work" documentWorkstationClass defaultapp
    "wkFileName" : "./vc06n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc06Work" imageWorkstationClass defaultapp
    "wkFileName" : "./vc06n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a ScalarField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vfield" vectorFieldClass appid
	"vfUDataArray" : uv->u
	"vfVDataArray" : uv->v
	"vfXCStartV" : -180.0
	"vfXCEndV" : 180.0
	"vfYCStartV" : -90.0
	"vfYCEndV" : 90.0
end create

sfid = create "sfield" scalarFieldClass appid
	"sfDataArray" : p->Psl
	"sfXCStartV" : -180.0
	"sfXCEndV" : 180.0
	"sfYCStartV" : -90.0
	"sfYCEndV" : 90.0
end create

;
; Create a VectorPlot object, supplying the VectorField object as data.
; Also supply the ScalarField object that will be used to determine the
; color of each individual vector arrow. 
; Setting vcMonoVectorLineColor False causes VectorPlot to color the
; vector arrows individually and setting vcUseScalarArray True results
; in VectorPlot applying the colors based on the contents of the scalarfield.
;

vcid = create "vectorplot" vectorPlotClass wid
    "vcVectorFieldData" : vfid
    "vcScalarFieldData" : sfid
    "vcRefMagnitudeF" : 20.0
    "vcFillArrowsOn" : True
    "vcMinFracLengthF" : 0.25
    "vcUseScalarArray" : True
    "vcMonoFillArrowFillColor" : False
end create

mpid = create "mapplot" mapPlotClass wid
    "mpProjection" : "orthographic"
    "mpCenterLatF" : 50
end create

overlay(mpid,vcid)

;
; Strings used to create fixed length numbers
;

smindist0 = "0.000"
smindist1 = "0.00"
smindist2 = "0.0"

;
; Create FRAME_COUNT frames, increasing the value of vcMinDistanceF
; and decreasing the value of mpCenterLonF at each successive frame.
; Note that the first frame and the last frame are equivalent in
; longitude.
;

do i = (FRAME_COUNT-1),0,1
	print("longitude: "+i * 360./(FRAME_COUNT-1))
	setvalues mpid
		"mpCenterLonF" :  i * 360./(FRAME_COUNT-1)
	end setvalues
;
; create fixed length strings representing the value of vcMinDistanceF
;
	longitudeval = floattoint(i * 360./(FRAME_COUNT-1) + 0.5)

    slongitude = longitudeval+":S:o:N:"

	val = ((FRAME_COUNT-1) - i) * 0.0175/(FRAME_COUNT-1)
	mindistval = floattoint(10000*val + 0.5)
	if (mindistval .lt. 10) then
		smindist = smindist0+mindistval
	else 
		if (mindistval .lt. 100) then
			smindist = smindist1+mindistval
		else	
			smindist = smindist2+mindistval
		end if
	end if

	setvalues vcid
	"tiMainString" : "Varying vcMinDistanceF :: " + smindist 
	"vcMinDistanceF" : ((FRAME_COUNT-1) - i) * 0.0175/(FRAME_COUNT-1)
	"tiXAxisString" : slongitude
	end setvalues

	draw(mpid)	
	frame(wid)
end do
    
;
; Clean up
;

delete(appid)

end
