;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn04n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Wed Oct 12 13:18:13 MDT 1994
;
;   Description:    Emulates the output of the Conpack example 'cpex02.f'
;           using the HLU library.
;


;
; This program emulates the output of cpex02 with a few differences:
; 1. Because the information label is implemented as an HLU Annotation
;    object, Conpack is unaware of its existence, much less its location.
;    Therefore it is not possible to have Conpack remove the high/low
;    labels that occupy the same space as the info label.
; 2. Line labels do not appear in the same positions.
;

begin

PLOTS = 4
N = 33
M = 33

z = new((/PLOTS,N,M/),float)
len_dims = new((/3/),integer)

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; The resource file sets most of the ContourPlot resources that remain fixed
; throughout the life of the ContourPlot object.
;
appid = create "cn04" appClass defaultapp
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
  wid = create "cn04Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn04n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "cn04Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "cn04Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./cn04n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "cn04Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./cn04n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "cn04Work" documentWorkstationClass defaultapp
    "wkFileName" : "./cn04n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "cn04Work" imageWorkstationClass defaultapp
    "wkFileName" : "./cn04n"
    "wkFormat" : wks_type
  end create
end if

;
; Read the input data
;
len_dims(0) = PLOTS
len_dims(1) = N
len_dims(2) = M
z = asciiread("cn04n.asc",len_dims,"float")

dataid = create "Gendat" scalarFieldClass appid
        "sfDataArray" : z(0,:,:)
end create

;
; Create a ContourPlot object, supplying the ScalarField object as data,
; and setting the size of the viewport.
;
cnid = create "ContourPlot1" contourPlotClass wid
    "cnScalarFieldData" : dataid
    "tiMainString" : "EXAMPLE 2-1"
    "vpWidthF" : 0.4625
    "vpHeightF" : 0.4625
end create

;
; In order to set the contour array resources of interest, you must 
; allocate memory for the arrays and fill in the correct value for each
; element. But by calling GetValues for the arrays the ContourPlot object 
; allocates the space and fills in the current values for you. Then all
; that is necessary is to modify the values that you want to change. 
; Remember, however, that you are responsible for freeing the memory
; after you are done with it. Note also that a GetValues resource list is
; different that a SetValues list.
;
getvalues cnid
    "cnLevelFlags" : lvlflags
    "cnFillPatterns" : pats
    "cnLevels" : levels
    "cnLineThicknesses" : thicknesses
end getvalues

; 
; Depending on the level flag for each contour line, widen the line if
; there is a label on the line. Also set the fill style to pattern #6
; if the level is between certain values. Note that there is always one
; more element in the fill resource arrays than there are ContourPlot line 
; levels: the first element of these arrays specifies the attributes
; of areas less than the minimum contour level and the last element 
; specifies attributes of areas greater than the maximum contour level. 
;
thicknesses(ind(lvlflags.eq.3)) = 2.0
pats(:) = -1
pats(ind(levels.ge. .000045 .and. levels.lt. .000055)) = 6
pats(dimsizes(pats)-1) = -1

;
; Now that the arrays are correctly filled in set the arrays that have
; been modified. Also set the position of the first ContourPlot and
; the label scaling mode.
;
setvalues cnid
    "cnFillPatterns" : pats
    "cnLineThicknesses" : thicknesses
    "vpXF" : 0.0250
    "vpYF" : 0.9750
        "cnLabelScalingMode" : "ConfineToRange"
        "cnLabelScaleValueF" : 10.0
end setvalues

draw(cnid)

;
; Plot 2 - Set the Scalar Field object with a newly generated data set;
; Set the ContourPlot object with a new title, position, and a new label
; scaling mode.
;

setvalues dataid
        "sfDataArray" : z(1,:,:)
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-2"
    "vpXF" : 0.5125
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "MaxSigDigitsLeft"
end setvalues

draw(cnid)

;
; Plot 3 - Set the Scalar Field object with a newly generated data set;
; Set the ContourPlot object with a new title, position, and a new label
; scaling mode.
;

setvalues dataid
        "sfDataArray" : z(2,:,:)
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-3"
    "vpXF" : 0.0250
    "vpYF" : 0.4875
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "TrimZeros"
end setvalues

draw(cnid)

;
; Plot 4 - Set the Scalar Field object with a newly generated data set;
; Set the ContourPlot object with a new title, position, and a new label
; scaling mode.
;

setvalues dataid
        "sfDataArray" : z(3,:,:)
end setvalues

setvalues cnid
    "tiMainString" : "EXAMPLE 2-4"
    "vpXF" : 0.5125
    "cnScalarFieldData" : dataid
        "cnLabelScalingMode" : "IntegerLineLabels"
end setvalues

draw(cnid)

;
; Label the plot as an emulation
;
txid = create "TextItem1" textItemClass wid
       end create

draw(txid)
frame(wid)

;
; Free dynamic arrays, destroy the objects created, 
; close the HLU library and exit.
;
delete(lvlflags)
delete(pats)
delete(levels)
delete(thicknesses)
delete(dataid)
delete(cnid)
delete(wid)
delete(appid)

end
