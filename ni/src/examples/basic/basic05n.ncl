; $Id: basic05n.ncl,v 1.15 2010-03-15 22:49:23 haley Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       basic05n.ncl
;
;   Author: Tim Scheitlin
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date: Thu Apr 27 13:03:39 MDT 1995 
;
;   Description: This example demonstrates how to read, display, 
;                and manipulate colormaps.

; The NG 4.x HLU software supports several different predefined
; colormaps of various sizes.  This example demonstrates how to
; display each of those colormaps using the labelbar utility.
; This example also shows how to change entries in a colormap
; and create a completely new colormap.

begin

;
; Set the display. Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object with default colormap.
;
  wks = create "wks" ncgmWorkstationClass defaultapp
    "wkMetaName" : "basic05n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation with default colormap.
;
  wks = create "wks" windowWorkstationClass defaultapp
    "wkPause" : "True"
;    "wkXColorMode" : "private"      ; Set color mode to private to avoid
                                     ; contention for colors from other apps.
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wks = create "wks" psWorkstationClass defaultapp
    "wkPSFileName" : "basic05n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wks = create "wks" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "basic05n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wks = create "wks" documentWorkstationClass defaultapp
    "wkFileName" : "basic05n"
    "wkFormat"   : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wks = create "wks" imageWorkstationClass defaultapp
    "wkFileName" : "basic05n"
    "wkFormat"   : wks_type
  end create
end if


; Initialize numeric labels for the labelbar annotation.
colorindices = ispan(1,255,1)

; Create a labelbar object. 
lbar = create "LabelBar" labelBarClass wks
    "lbLabelStrings" : colorindices       ; Assign the labels
    "lbLabelStride" :  5                  ; Label every 5th entry
    "lbMonoFillPattern" : True          ; Single pattern used for fill
    "lbFillPattern" : "SolidFill"         ; Set fill pattern to solid
    "lbBoxLinesOn" : False              ; Turn off lines between colors
    "lbBoxCount" : 255                  ; Display 255 entries
    "lbPerimOn" : False                 ; Turn off labelbar perimeter
    "lbTitleString" : "(New) Default Colormap"  ; Plot title
    "lbTitleFont" : "Helvetica-bold"      ; Title font
    "lbLabelFont" : "Helvetica"           ; Label font
    "vpXF" : 0.                           ; Set the viewport to max size
    "vpYF" : 1.
    "vpWidthF" : 1.
    "vpHeightF" : 1.
end create
delete(colorindices)

; Initialize numeric labels for the labelbar annotation.
colorindices = ispan(1,232,1)

; Create a text label
text = create "textitem" textItemClass lbar
        "txFont" : "Helvetica-bold" ; Set font
        "txPosXF" : .5              ; Set position and height
        "txPosYF" : .03
        "txFontHeightF" : .035
        "txFuncCode" : "*"   ; This is set, so the colon character, which is
                             ; the default function code character, can be
                             ; used in the "txString" resource.
        "txString" : "Note: Entry 0 is the background color"
end create

; Draw the labelbar displaying the default colormap and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to the old default color map
setvalues wks
    "wkColorMap" : "default"
end setvalues
setvalues lbar
    "lbBoxCount" : 31                  ; Display 31 entries
    "lbTitleString" : "(Old) Default Colormap"  ; Plot title
end setvalues

; Draw the labelbar displaying the old default colormap and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "cyclic"
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Cyclic Colormap"   ; Labelbar title
    "lbLabelStride" :  1                  ; Label every entry
    "lbBoxCount"    :  7                  ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "gscyclic"   ; gray scale cyclic
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Gscyclic Colormap" ; Labelbar title
    "lbLabelStride" :  1                  ; Label every entry
    "lbBoxCount"    :  7                  ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "gsltod"       ;gray scale light to dark
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Gsltod Colormap"   ; Labelbar title
    "lbLabelStride" :  2                  ; Label every other entry
    "lbBoxCount"    :  32                 ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "gsdtol"       ; 
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Gsdtol Colormap"   ; Labelbar title
    "lbLabelStride" :  2                  ; Label every other entry
    "lbBoxCount"    :  32                 ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "uniform"          ; 
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Uniform Colormap"   ; Labelbar title
    "lbLabelStride" :  10                  ; Label every 10th entry
    "lbBoxCount"    :  112                 ; Number of entries to display
end setvalues
  
; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "temp1"        ; temperature (cold to warm)
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Temp1 Colormap"    ; Labelbar title
    "lbLabelStride" :  5                  ; Label every 5th entry
    "lbBoxCount"    :  62                 ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "psgcap"       ; PostScript graphcap
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Psgcap Colormap"    ; Labelbar title
    "lbLabelStride" :  15                  ; Label every 15th entry
    "lbBoxCount"    :  230                 ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; Change the colormap to one of the predefined colormaps
setvalues wks
    "wkColorMap" : "example"          ; Example colormap
end setvalues

; Change the labelbar title, annotation, and number of entries.
setvalues lbar
    "lbTitleString" : "Example Colormap"    ; Labelbar title
    "lbLabelStride" :  10                  ; Label every 10th entry
    "lbBoxCount"    :  114                 ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
draw(text)
frame(wks)

; This next example changes three entries in the colormap.  Changing the
; first entry (colormap index 0) in the colormap, sets the background 
; color for a plot. The second entry (color index 1) sets the foreground
; color for a plot.

; The colormap is stored in a 3xN variable where N is the length of
; the colormap.  Each entry in the color map consists of a vector
; of 3 normalized red-green-blue color values.

; Assign a new colormap
setvalues wks
    "wkColorMap" : "gscyclic" ; Assign grayscale colormap         
end setvalues

; Copy the colormap rgb values into the variable cmap
getvalues wks
  "wkColorMap" : cmap
end getvalues

; Change the first entry in the colormap array to blue, the
; second to green, and the fourth to red.

cmap((/0,1,3/),:) = (/ (/0.0,0.0,1.0/), \   ; Background color
		(/0.0,1.0,0.0/), \   ; Foreground color
		(/1.0,0.0,0.0/) /)   ; Colormap entry 3

; Reassign the colormap 
setvalues wks
    "wkColorMap" : cmap
end setvalues

; Set the annotation for the label bar
setvalues lbar
    "lbTitleString" : "Changing colormap entries"
    "lbLabelStride" :  1                  ; Label every entry
    "lbBoxCount"    :  7                  ; Number of entries to display
end setvalues

setvalues text
    "txString" : "Entry 0 (background) set to Blue"
end setvalues
   
; Draw the labelbar and textual annotation.
draw(lbar)
draw(text)
frame(wks)

; This next example demonstrates how to create and assign a new colormap.

; Create an array that will contain the new colormap.  For this example,
; we are creating a colormap with 100 (arbitrary size) rgb entries.
newcmap = new((/100,3/), float)

; Assign new RGB values to each entry of the colormap.

; Set the first entry (background color) to black

ff = fspan(.0,1.0,dimsizes(newcmap(:,0)))
newcmap(:,0) = 1.-ff
newcmap(:,1) = ff
newcmap(:,2) = ff
newcmap(0,:) = 0.0

; Assign the new color map to the workstation object.
setvalues wks
    "wkColorMap" : newcmap
end setvalues

; Assign a new title.
setvalues lbar
    "lbTitleString" : "New colormap"       ; Labelbar title
    "lbLabelStride" :  10                  ; Label every 10th entry
    "lbBoxCount"    :  99                  ; Number of entries to display
end setvalues

; Draw the labelbar and textual annotation
draw(lbar)
frame(wks)

; Clean up
delete(lbar)
delete(text)
delete(wks)
delete(colorindices)
delete(newcmap)

; End the ncl script.
end
