; $Id: basic05n.ncl,v 1.1 1995-02-13 23:46:55 scheitln Exp $
;
; This example demonstrates how to read and manipulate colormaps.

; The first frame displays the default colormap.
; The second frame shows how to alter an entry in the colormap.
; The third frame shows how to create a completely new colormap.

; ##########
; # Fame 1 #
; ##########
; Display the default colormap.

; Initialize labels for the color map entries

colorindices=new(114,integer)
do i = 0, 113
   colorindices(i) = i
end do

; Create an XWorkstation object.

wks = create "wks" xWorkstationLayerClass noparent
end create

; Create a labelbar object. 

lbar = create "LabelBar" labelBarLayerClass wks
	"lbLabelStrings" : colorindices       ; Assign the labels
	"lbLabelStride" : 17                  ; Label every 17th entry
	"lbMonoFillPattern" : "TRUE"          ; Single pattern used for fill
	"lbFillPattern" : "SolidFill"         ; Set fill pattern to solid
	"lbBoxLinesOn" : "FALSE"              ; Turn off lines between colors
	"lbBoxCount" : 114                    ; Display 114 entries
	"lbPerimOn" : "FALSE"                 ; Turn off labelbar perimeter
	"lbTitleString" : "Default Colormap"  ; Plot title
	"lbTitleFont" : "Helvetica-bold"      ; Title font
	"lbLabelFont" : "Helvetica"           ; Label font
	"vpXF" : 0.                           ; Set the viewport to max size
	"vpYF" : 1.
	"vpWidthF" : 1.
	"vpHeightF" : 1.
end create

; Draw and frame the labelbar

draw(lbar)
frame(wks)

; ##########
; # Fame 2 #
; ##########
; Alter a single entry in the colormap.

; Get the current colormap for the workstation pointed to by wks.
; The colormap is stored in a 3xN variable where N is the length of
; the colormap.  Each entry in the color map consists of a vector
; of 3 normalized red-green-blue color values.

; Note:  At the time of writing this script, most of the NCL functions
; for color map manipulation were not yet available.  So, the rest
; of this example may no longer be the most effective and simplest
; way to change color map entries.

getvalues wks
  "wkColorMap" : cmap
end getvalues

; Change the first entry in the colormap array to red.

cmap(0,:) = (/1.0,0.0,0.0/)

; Assign the new color map to the workstation object.

setvalues wks
   	"wkColorMap" : cmap
end setvalues

; Add a different title.

setvalues lbar
	"lbTitleString" : "Entry 0 set to Red"
end setvalues
   
; Draw and frame the labelbar.

draw(lbar)
frame(wks)

; ##########
; # Fame 3 #
; ##########
; Create and assign a new colormap.

; Create an array that will contain the new colormap.

newcmap = new((/114,3/), float)

; Assign new RGB values to each entry of the colormap.

do i = 0, 113
   newcmap(i,:) = (/1.-(i/113.),(i/113.), (i/113.)/)
end do

; Assign the new color map to the workstation object.

setvalues wks
   	"wkColorMap" : newcmap
end setvalues

; Assign a new title.

setvalues lbar
	"lbTitleString" : "New colormap"
end setvalues

; Draw and frame the labelbar

draw(lbar)
frame(wks)
