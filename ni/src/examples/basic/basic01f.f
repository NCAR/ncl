C
C This example demonstrates how to draw a contour plot using mostly
C defaults.  Note: no data is used in this example, so the output appears
C only as a bounding box with tickmarks.
C 
C The mininum set of steps needed for creating any plot involve the following:
C
C   1. Initialize the graphics libraries
C   2. Choose the type of output 
C   3. Create a plot object
C   4. Draw the plot
C   5. Call frame
C   6. Clean up memory
C

	program basic01f
	implicit none

        external NhlFappLayerClass
        external NhlFResListLayerClass
        external NhlFXWorkstationLayerClass
        external NhlFContourLayerClass

	integer appid,wks,con1,rlist,ierr

C ##########
C # STEP 1 #
C ##########
C Initialize the graphics libraries and create a resource list that
C is normally used to assign name/value pairs within objects.  Then
C clear (empty) this list, and create an application object.  This
C object manages multiple resource databases used by seperate objects.  
C
C The first argument, appid, is a variable that identifies the object.
C The second argument, '"basic01"', sets the name of the object being created.
C The third argument, "NhlFappLayerClass", identifies the type or class 
C of the created object. 
C The fourth argument, "0", specifies the id of the objects parent.
C In this case, the object has no parent, so the value 0 is used. 
C The fifth argument, "rlist", is the resource list modifiers to be used
C when creating the object.  In this example, no modifications are made to
C default values.
C The sixth argument, "ierr", is used to return an error code.
C
C These steps are always required when writing an HLU application program.

        call NhlFInitialize
        call NhlFRLCreate(rlist,'SETRL')

        call NhlFRLClear(rlist)
        call NhlFCreate(appid,"basic01",NhlFappLayerClass,0,rlist,ierr)

C ##########
C # STEP 2 #
C ##########
C Choose the type of output you want to create.  You may write your
C output to an NCAR Computer Graphics Metafile (NCGM) and view it later using
C the NCAR Graphics utilities ctrans or idt.  You may also write your
C output directly into a window of a workstation running the X Window system
C (as demonstrated in this example), or you can write your ouput into 
C a PostScript file.  
C
C The first argument, wks, is a variable that identifies the object.
C The second argument, '"wks"', sets the name of the object being created.
C The third argument, "NhlFxWorkstationLayerClass", identifies the type 
C or class of the object to create.  In this case an X workstation.
C The fourth argument, "0", specifies the id of the objects parent.
C In this case, the object has no parent, so the constant 0 is used. 
C The fifth argument, "rlist", is the resource list modifiers to be used
C when creating the object.  In this example, no modifications are made to
C default values.
C The sixth argument, "ierr", is used to return an error code.

        call NhlFRLClear(rlist)
        call NhlFCreate(wks,"wks",NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)

C ##########
C # STEP 3 #
C ##########
C Create a plot object.  In this example, we will create a contour plot,
C but we could have just as easily created any other type of plot such as
C an Xy plot, or a Map plot.
C
C The first argument, con1, is a variable that identifies the object.
C The second create call argument, '"con1"', sets the name of the object.
C This is an arbitrary name and does not have to match the variable object
C identifier used in the first parameter.
C The third argument, "NhlFcontourLayerClass", identifies the type or class
C of the object to create.  In this case, the type is a contour plot. 
C The third argument, "wks", specifies the id of the object's parent.  By 
C specifying the id of the X workstation created earlier, the plot will
C be drawn into an X window.
C The fifth argument, "rlist", is the resource list modifiers to be used
C when creating the object.  In this example, no modifications are made to
C default values.
C The sixth argument, "ierr", is used to return an error code.

        call NhlFRLClear(rlist)
        call NhlFCreate(con1,"con1",NhlFcontourLayerClass,wks,
     $       rlist,ierr)

C ##########
C # STEP 4 #
C ##########
C This step draws the plot into the X workstation window.  The first argument 
C to the draw function is the variable name of the object that you want to
C draw.  The second argument is for returning error codes.  

	call NhlFDraw(con1,ierr)

C ##########
C # STEP 5 #
C ##########
C The frame call updates and then clears the workstation.

	call NhlFFrame(wks,ierr)

C ##########
C # STEP 6 #
C ##########
C This is the final step used for cleanup.  The delete
C function deletes variables from the NCL and frees the
C symbol name from the symbol table.  Deleting a parent object
C automatically deletes all of its children.  The close function
C is used to tell the HLU library that the programmer is done 
C using it, and to free up any memory that it can. 

	call NhlFDestroy(con1,ierr)
        call NhlFClose

	stop
	end
