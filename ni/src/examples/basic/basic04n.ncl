; $Id: basic04n.ncl,v 1.7 1995-06-29 17:16:04 scheitln Exp $
;
; This example demonstrates how to select and change the workstation
; device for drawing your output to an NCGM file or an X workstation window.
; using the following steps.
;
;   1. Create output workstation objects.
;   2. Create the data for the plots.
;   3. Create the contour object.
;   4. Draw the contour object.
;   5. Call frame.
;   6. Change workstations.
;   7. Draw the contour object.
;   8. Call frame.
;   9. Clean up memory.

; Begin the ncl script.

begin

; ##########
; # STEP 1 #
; ##########
; For each type of output you must create a workstation object using create.

; The first argument, '"wks"', to the create call sets the name of the 
; object being created. The second argument, "windowWorkstationClass", or 
; "ncgmWorkstationClass" identifies the type or class of the object 
; to create. In this case an X workstation or an NCGM workstation.
; The third argument, "defaultapp", specifies the id of the objects parent.
; In this case, the object has no parent, so the constant "defaultapp" is used. 

xwks = create "xwks" windowWorkstationClass defaultapp 
end create

; The resource, wkMetaName, lets you specify the name of the output NCGM
; file.  In this example, it is called basic04n.ncgm.  If omitted, the 
; default name, gmeta,  will be used.

nwks = create "nwks" ncgmWorkstationClass defaultapp 
        "wkMetaName" : "basic04n.ncgm"
end create

; ##########
; # STEP 2 #
; ##########
; Create the data for the plot.

; Create a 5x5 integer variable.

data1=new((/5,5/), integer)

; Initialize the variable with data.

data1=(/(/3,4,4,5,5/), \
        (/2,3,5,5,4/), \
        (/2,4,5,4,4/), \
        (/3,4,4,4,3/), \
        (/3,3,3,3,3/)/)

; Create a scalar field object that will be used as a data set for a
; contour object.  The sfDataArray resource is used to assign a data
; array to a scalar field data object.

field1 = create "field1" scalarFieldClass defaultapp
        "sfDataArray" : data1
end create

; ##########
; # STEP 3 #
; ##########
; Create the object you want to draw.

; Create a contour object to draw into the X workstation.
; Assign data using the cnScalarFieldData resource.

xcon = create "xcon" contourPlotClass xwks
   "cnScalarFieldData" : field1
end create

; ##########
; # STEP 4 #
; ##########
; Draw the object

draw(xcon)

; ##########
; # STEP 5 #
; ##########
; Call frame to update and clear the workstation

frame(xwks)

; ##########
; # STEP 6 #
; ##########
; Change the workstation

NhlChangeWorkstation(xcon,nwks)

; ##########
; # STEP 7 #
; ##########
; Draw the object

draw(xcon)

; ##########
; # STEP 8 #
; ##########
; Call frame to update and clear the workstation

frame(nwks)

; ##########
; # STEP 9 #
; ##########
; Clean up memory.

delete(xwks)
delete(nwks)

; End the ncl script.

end
