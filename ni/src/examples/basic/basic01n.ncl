; $Id: basic01n.ncl,v 1.7 1995-06-14 15:08:48 stautler Exp $
;
; This example demonstrates how to draw a contour plot using mostly
; defaults.  Note: no data is used in this example, so the output appears
; only as a bounding box with tickmarks.
; 
; The minimum set of steps needed for creating any plot involve the following:
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
; output directly into a window of a workstation running the X Window system
; (as demonstrated in this example), or you can write your ouput into 
; a PostScript file.  
;
; The first argument, '"wks"', to the create call sets the name of the 
; object being created.
; The second argument, "xWorkstationClass", identifies the type or class 
; of the object to create. In this case an X workstation.
; The third argument, "defaultapp", specifies the id of the objects parent.
; In this case, the object has no parent, so the constant "defaultapp" is used. 

wks = create "wks" xWorkstationClass defaultapp 
      end create

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
