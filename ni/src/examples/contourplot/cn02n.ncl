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
;   Description:    Demonstrates basic features of the Contour object
;

begin

M=25
N=25

T = new((/N,M/),float)
len_dims = new((/2/),integer)

;
; create a simple bull's eye pattern test data set
;
PI = 3.14159

do i= -N/2,N/2
    do j= -M/2,M/2
        x = 8.0 * i
        y = 8.0 * j
        T(i+N/2,j+M/2) = 100.0 - sqrt(x*x + y*y);
    end do
end do

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "cn02" appLayerClass noparent
    "appUsrDir" : "./"
        "appDefaultParent" : "True"
end create

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "cn02Work" ncgmWorkstationLayerClass appid
    "wkMetaName" : "./cn02n.ncgm"
  end create
else
;
; Create an X workstation.
;
  wid = create "cn02Work" xWorkstationLayerClass noparent
    "wkPause" : "True"
  end create
end if

;
; Create a ScalarField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;
len_dims(0) = N
len_dims(1) = M

dataid = create "bullseye" scalarFieldLayerClass appid
        "sfDataArray" : T
end create

;
; Create a Contour object, supplying the ScalarField object as data
;
cnid = create "Contour1" contourPlotLayerClass wid
    "cnScalarFieldData" : dataid
end create

;
; Draw the plot, notice that it illustrates the basic default behavior of
; the Contour object. The contours appear as solid lines with unboxed
; labels in a linear coordinate system with the origin at the lower left. 
; Tickmarks with labels show the data coordinate range, and an 
; informational label at the lower right gives the minimum and maximum
; data values and the contour interval spacing. The title is NOT a default
; item, it appears because it is defined in the resource file.
;
draw(cnid)
frame(wid)

;
; In the Contour object, many resources that apply to the lines 
; representing the contour levels and the fill areas between the levels
; have both a scalar and an array form. You control which applies by 
; setting an associated boolean flag, indentified by the prefix "Mono".
; As an illustration, set NhlNcnMonoLineDashPattern and NhlNcnMonoLineColor
; false to use a different line dash pattern and line color at each level.
; At the same time set the line thickness of all lines to twice the
; default thickness.
;
setvalues cnid
    "cnLineThicknessF" : 2.0
    "cnMonoLineDashPattern" : "false"
    "cnMonoLineColor" : "false"
end setvalues

draw(cnid)
frame(wid)

;
; Change back to a single solid line color and use pattern fill
;
setvalues cnid
    "cnLineThicknessF" : 1.0
    "cnMonoLineDashPattern" : "true"
    "cnMonoLineColor" : "true"
    "cnFillOn" : "true"
    "cnMonoFillColor" : "true"
    "cnMonoFillPattern" : "false"
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

count = dimsizes(fscales)
do i= 0,count-1
    fscales(i) = 2.5 - 2.0 * i / (count - 1)
end do

setvalues cnid
    "cnMonoFillScale" : "false"
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
    "cnMonoFillColor" : "false"
    "cnMonoFillPattern" : "true"
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

do i= 0,count/2
    itmp = colors(i)
    colors(i) = colors(count - 1 - i)
    colors(count - 1 - i) = itmp
end do

setvalues cnid
    "cnFillColors" : colors
    "cnLinesOn" : "false"
    "cnLineLabelsOn" : "false"
    "cnHighLabelsOn" : "false"
    "cnLowLabelsOn" : "false"
end setvalues

draw(cnid)
frame(wid)
    
;
; Destroy the objects created, close the HLU library and exit.
;
delete(dataid)
delete(cnid)
delete(wid)
delete(appid)

end
