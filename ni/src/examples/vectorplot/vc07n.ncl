;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1996                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc07n.ncl
;
;   Author: David Brown (converted by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:  Mon Jul  1 15:03:55 MST 1996
;
;   Description: This example emulates the LLU example "fcover", overlaying
;                contours and vectors on a map plot.
;
begin

MSIZE = 73
NSIZE = 73
NROWS = 11

; 
; Define our own colormap.
;
cmap = (/ (/1.0,1.0,1.0/), \
	 (/0.0,0.0,0.0/), \
	 (/0.9,0.9,0.9/), \
	 (/0.6,0.6,0.6/), \
	 (/0.3,0.3,0.3/), \
	 (/0.8,0.9,1.0/), \
	 (/0.5,0.0,0.5/), \
	 (/0.0,0.5,0.7/), \
	 (/0.0,0.0,0.0/), \
	 (/0.00000,1.00000,0.00000/), \
	 (/0.14286,1.00000,0.00000/), \
	 (/0.28571,1.00000,0.00000/), \
	 (/0.42857,1.00000,0.00000/), \
	 (/0.57143,1.00000,0.00000/), \
	 (/0.71429,1.00000,0.00000/), \
	 (/0.85714,1.00000,0.00000/), \
	 (/1.00000,1.00000,0.00000/), \
	 (/1.00000,0.85714,0.00000/), \
	 (/1.00000,0.71429,0.00000/), \
	 (/1.00000,0.57143,0.00000/), \
	 (/1.00000,0.42857,0.00000/), \
	 (/1.00000,0.28571,0.00000/), \
	 (/1.00000,0.14286,0.00000/), \
	 (/1.00000,0.00000,0.00000/) /)

;
; Create an application object.
;
appid = create "vc07" appClass defaultapp
    "appUsrDir" : "./"
    "appDefaultParent" : True
end create
;
; Default is to display output to an X workstation.
;
wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "vc07Work" ncgmWorkstationClass defaultapp
       "wkMetaName" : "./vc07n.ncgm"
       "wkColorMap" : cmap
    end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  wid = create "vc07Work" windowWorkstationClass defaultapp
      "wkPause" : True
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc07Work" psWorkstationClass defaultapp
   "wkPSFileName" : "./vc07n.ps"
   "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc07Work" pdfWorkstationClass defaultapp
   "wkPDFFileName" : "./vc07n.pdf"
   "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc07Work" documentWorkstationClass defaultapp
   "wkFileName" : "./vc07n"
   "wkFormat" : wks_type
   "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc07Work" imageWorkstationClass defaultapp
   "wkFileName" : "./vc07n"
   "wkFormat" : wks_type
   "wkColorMap" : cmap
  end create
end if

;
; Read the data file.
;
;   data(0,:,:) is U
;   data(1,:,:) is V
;   data(2,:,:) is P
;
path = ncargpath("data")
data = asciiread(path + "/asc/fcover.dat",(/3,73,73/),"float")
;
; Massage the data to eliminate surplus of vectors near the pole
;
ithin = (/90,15,5,5,4,4,3,3,2,2,2/)
do j=1,NROWS
      data(0,NSIZE-j,ind(ispan(1,MSIZE,1) % ithin(j-1) .ne. 0)) = -9999.0
end do
data@_FillValue = -9999.0

;
; Create a MapPlot object.
; 
mpid = create "mapplot" mapPlotClass wid end create

;
; Create a ScalarField object.
;
sfield = create "ScalarField" scalarFieldClass appid
    "sfDataArray" : data(2,:,:)
    "sfXCStartV" : -180.
    "sfYCStartV" : -90.
    "sfXCEndV" : 180.
    "sfYCEndV" : 90.
    "sfMissingValueV" : data@_FillValue
end create
;
; Create a VectorField object.
;
vfield = create "VectorField" vectorFieldClass appid
    "vfUDataArray" : data(0,:,:)
    "vfVDataArray" : data(1,:,:)
    "vfXCStartV" : -180.
    "vfYCStartV" : -90.
    "vfXCEndV" : 180.
    "vfYCEndV" : 90.
    "vfMissingUValueV" : -9999.0
    "vfMissingVValueV" : -9999.0
end create

;
; Create a VectorPlot object.
;
vcid = create "vectorplot" vectorPlotClass wid
    "vcUseScalarArray" :  True
    "vcVectorFieldData":  vfield
    "vcScalarFieldData":  sfield
end create

getvalues vcid
  "vcMinMagnitudeF": vmin
  "vcMaxMagnitudeF": vmax
end getvalues

setvalues vcid
  "vcMinMagnitudeF": vmin + 0.1 * (vmax - vmin)
end setvalues

;
;
; Create a ContourPlot object.
;
cnid = create "contourplot" contourPlotClass wid
    "cnScalarFieldData":  sfield
end create

;
; Overlay the vectors and the contours on the map and draw
; everything.
;

draw(mpid)
frame(wid)

draw(cnid)
frame(wid)

draw(vcid)
frame(wid)

overlay(mpid,vcid)
overlay(mpid,cnid)
draw(mpid)
frame(wid)

end
