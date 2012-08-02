; $Id: basic01n.ncl,v 1.13 2010-03-15 22:49:23 haley Exp $
;
; This example demonstrates how to draw a contour plot using mostly
; defaults.  Note: no data is used in this example, so the output appears
; only as a bounding box with tickmarks.
; 
;   1. Choose the type of output 
;   2. Create a plot object
;   3. Draw the plot
;   4. Call frame
;   5. Clean up memory

; Begin ncl script.

begin

; ##########
; # STEP 1 #
; ##########
; Choose the type of output you want to create.  You may write your
; output to an NCAR Computer Graphics Metafile (NCGM) and view it later using
; the NCAR Graphics utilities ctrans or idt.  You may also write your
; output directly into a window of a workstation running the X Window system.
; Another option is to write your ouput into a PostScript file.  
;
; The first argument, '"wks"', to the create call sets the name of the 
; object being created.
; The second argument, "xWorkstationClass", identifies the type or class 
; of the object to create. In this case an X workstation.
; The third argument, "defaultapp", specifies the id of the objects parent.
; In this case, the constant "defaultapp" is used to specify the default 
; parent. 

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wks = create "wks" ncgmWorkstationClass defaultapp 
    "wkMetaName" : "./basic01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "wks" xWorkstationClass defaultapp 
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wks = create "wks" psWorkstationClass defaultapp 
    "wkPSFileName" : "./basic01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wks = create "wks" pdfWorkstationClass defaultapp 
    "wkPDFFileName" : "./basic01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wks = create "wks" documentWorkstationClass defaultapp
    "wkFileName" : "basic01n"
    "wkFormat"   : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wks = create "wks" imageWorkstationClass defaultapp
    "wkFileName" : "basic01n"
    "wkFormat"   : wks_type
  end create
end if


; ##########
; # STEP 2 #
; ##########
; Create a plot object.  In this example, we will create a contour plot,
; but we could have just as easily created any other type of plot such as
; an Xy plot, or a Map plot.
;
; The first create call argument, '"con1"', sets the name of the object.
; This is an arbitrary name and does not have to match the variable name
; on the left hand side of the call.
; The second argument, "contourPlotClass", identifies the type or class
; of the object to create.  In this case, the type is a contour plot. 
; The third argument, "wks", specifies the id of the object's parent.  By 
; specifying the id of the X workstation created earlier, the plot will
; be drawn into an X window.

con1 = create "con1" contourPlotClass wks 
       end create

; ##########
; # STEP 3 #
; ##########
; This step draws the plot into the X workstation window.  The argument to
; the draw function is the variable name of the object that you want to
; draw.

draw(con1)

; ##########
; # STEP 4 #
; ##########
; The frame call updates and then clears the workstation.

frame(wks)

; ##########
; # STEP 5 #
; ##########
; This is the final step used for cleanup.  The delete
; function deletes variables from the NCL and frees the
; symbol name from the symbol table.  Deleting a parent object
; automatically deletes all of its children.

delete(wks)

; End ncl script.

end
