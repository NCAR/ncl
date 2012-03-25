;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn02n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Tue Oct  4 18:48:08 MDT 1994
;
;   Description:    Demonstrates basic features of the ContourPlot object
;

begin

;
; create a simple bull's eye pattern test data set
;
;
; create a simple bull's eye pattern test data set
;
M=25
N=25
 
T = new((/N,M/),float)
len_dims = new((/2/),integer)
 
;
; create a simple bull's eye pattern test data set
;
PI = 3.14159
 
jspn = ispan(-M/2,M/2,1)^2
ispn = ispan(-N/2,N/2,1)^2
do i = 0, dimsizes(ispn)-1
        T(i,:) = ispn(i) + jspn
end do
T = 100.0 - sqrt(8^2 * T)


;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "cn02" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : "True"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "cn02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "cn02Work" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "cn02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./cn02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "cn02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./cn02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "cn02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./cn02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "cn02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./cn02n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a ScalarField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

dataid = create "bullseye" scalarFieldClass appid
        "sfDataArray" : T
end create

;
; Create a ContourPlot object, supplying the ScalarField object as data
;
cnid = create "ContourPlot1" contourPlotClass wid
    "cnScalarFieldData" : dataid
end create

;
; In the ContourPlot object, many resources that apply to the lines 
; representing the contour levels and the fill areas between the levels
; have both a scalar and an array form. You control which applies by 
; setting an associated boolean flag, identified by the prefix "Mono".
; As an illustration, set NhlNcnMonoLineDashPattern and NhlNcnMonoLineColor
; false to use a different line dash pattern and line color at each level.
; At the same time set the line thickness of all lines to twice the
; default thickness.
;
setvalues cnid
    "cnMonoLineDashPattern" : False
    "cnMonoLineColor" : False
    "cnLineThicknessF" : 2.0
end setvalues

draw(cnid)
frame(wid)

;
; Change back to a single solid line color and use pattern fill
;
setvalues cnid
    "cnLineThicknessF" : 1.0
    "cnMonoLineDashPattern" : True
    "cnMonoLineColor" : True
    "cnFillOn" : True
    "cnMonoFillColor" : True
    "cnMonoFillPattern" : False
end setvalues

draw(cnid)
frame(wid)

;
; Get the fill scale array to illustrate how you would modify the
; values of an array resource. By default all elements of this array
; are set to 1.0, resulting in all fill patterns appearing at its
; 'standard' size.
; The user is responsible for freeing the memory allocated for this array.
; Modify the array to range from sparse (2.5) at the low data values 
; to dense (0.5) at high values, and set the new values, turning off
; the "mono" flag resource at the same time. 
;
getvalues cnid
    "cnFillScales" : fscales
end getvalues

fscales = 2.5 - (2.0 * ispan(0,dimsizes(fscales)-1,1)) / (dimsizes(fscales) -1)

setvalues cnid
    "cnMonoFillScale" : False
    "cnFillScales" : fscales
end setvalues

draw(cnid)
frame(wid)

;
; Use solid multi-colored fill instead of single-colored pattern fill.
; Change the contour lines to use the background color using the scalar
; form of the line color resource.
;
setvalues cnid
    "cnLineColor" : 0
    "cnMonoFillColor" : False
    "cnMonoFillPattern" : True
end setvalues

draw(cnid)
frame(wid)

;
; Invert the fill colors.
; First get the current array contents, reverse their order, 
; then re-set the resource using the modified array. Note that the user 
; is responsible for freeing the memory allocated for the array. 
; Turn lines off altogether and also turn off the line and high/low labels.
;
getvalues cnid
    "cnFillColors" : colors
end getvalues


setvalues cnid
    "cnFillColors" : colors(::-1)
    "cnLinesOn" : False
    "cnLineLabelsOn" : False
    "cnHighLabelsOn" : False
    "cnLowLabelsOn" : False
end setvalues

draw(cnid)
frame(wid)
    
;
; Destroy the objects created, close the HLU library and exit.
;
delete(colors)
delete(fscales)
delete(dataid)
delete(cnid)
delete(wid)
delete(appid)

end
