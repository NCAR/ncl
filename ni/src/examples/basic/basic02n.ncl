; $Id: basic02n.ncl,v 1.13 2010-03-15 22:49:23 haley Exp $
;
; The first frame in this example demonstrates how to set the view port 
; for a contour plot.
; Note: no data is used in this example, so the output appears
; only as a bounding box with tickmarks.
;
; The second frame in this example demonstrates how to produce multiple
; plots on a single frame.
; 
; Begin the ncl script.

begin

; ###########
; # FRAME 1 #
; ###########
; Choose the type of output you want to create.  You may write your
; output to an NCGM, file, X workstation window, or a PostScript file. 

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wks = create "wks" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./basic02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "wks" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wks = create "wks" psWorkstationClass defaultapp
    "wkPSFileName" : "./basic02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wks = create "wks" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./basic02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wks = create "wks" documentWorkstationClass defaultapp
    "wkFileName" : "basic02n"
    "wkFormat"   : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wks = create "wks" imageWorkstationClass defaultapp
    "wkFileName" : "basic02n"
    "wkFormat"   : wks_type
  end create
end if

; Create a plot object.  In this example, we will create a contour plot.
;
; Four view class resources, vpXF, vpYF, vpWidthF, and vpHeightF, are
; assigned values in the following create call.  The combination of
; these four resources determines where the plot will display in the
; output window.  The values of these resources are specified in 
; Normalized Device Coordinates (NDCs).  In this two-dimensional coordinate 
; system (0,0) specifies the lower-left corner and (1,1) specifies the 
; upper-right corner of a plot.

con1 = create "con1" contourPlotClass wks 
	"vpXF"       : 0.05   ;The left edge of the viewport bounding box
 	"vpYF"       : 0.95   ;The top edge of the viewport bounding box
 	"vpWidthF"   : 0.4    ;The width of the viewport bounding box
	"vpHeightF"  : 0.4    ;The height of the viewport bounding box
end create

; Draw the plot. 

draw(con1)

; The frame call updates and then clears the workstation.
; Anything written to the workstation after a frame call is made will be
; drawn in a subsequent frame. 

frame(wks)

; ###########
; # FRAME 2 #
; ###########

; This example demonstrates drawing multiple plots in a single frame.

; Calling draw again will produce the identical plot that was drawn in the
; first frame.

draw(con1)

; To add another plot to the same frame, we first need to reset the 
; viewport resources so that the next plot does not overwrite the first
; one.  The setvalues expression is used to set resources after an object
; has already been created.  The first argument, "con1", in the setvalues
; expression specifies an object id of a plot that was generated earlier
; with the create call.  This is then followed by a list of resource value
; pairs that apply to the object.

setvalues con1
	"vpXF"       : 0.55   ;The left edge of the viewport bounding box
	"vpYF"       : 0.45   ;The top edge of the viewport bounding box
 	"vpWidthF"   : 0.2    ;The width of the viewport bounding box
	"vpHeightF"  : 0.2    ;The height of the viewport bounding box
end setvalues

; Because of the new viewport resource settings, calling draw produces 
; a plot in the lower-right quadrant of the frame.

draw(con1)

; Updates and clear the workstation.

frame(wks)

; Clean up (deleting the parent object recursively deletes all of its 
; children).

delete(wks)

; End the ncl script.

end
