C This example demonstrates how to select and change the workstation
C device for drawing your output to an NCGM file or an X workstation window.
C using the following steps.
C
C   1. Create output workstation objects.
C   2. Create the data for the plots.
C   3. Create the contour objects.
C   4. Draw the contour objects.
C   5. Call frame.
C   6. Clean up memory.

	program basic04f
	implicit none

        external NhlFappLayerClass
        external NhlFXWorkstationLayerClass
        external NhlFNcgmWorkstationLayerClass
        external NhlFContourLayerClass
        external NhlFScalarFieldLayerClass

        integer appid,nwks,xwks,ncon,xcon,field1,rlist,ierr

        integer data1(5,5)
        data data1 / 3,4,4,5,5,
     $               2,3,5,5,4,
     $               2,4,5,4,4,
     $               3,4,4,4,3,
     $               3,3,3,3,3 /

        integer dims(2)
        data dims / 5, 5 /

C ##########
C # STEP 1 #
C ##########
C Initialize the graphics libraries and create a resource list that
C is normally used to assign name/value pairs within objects.  Then
C clear (empty) this list, and create an application object.  This
C object manages multiple resource databases used by seperate objects.

        call NhlFInitialize
        call NhlFRLCreate(rlist,'SETRL')

        call NhlFRLClear(rlist)
        call NhlFCreate(appid,"appid",NhlFappLayerClass,0,
     $       rlist,ierr)

C ##########
C # STEP 2 #
C ##########
C For each type of output you must create a workstation object using create.
C
C The first argument, xwks, is a variable that identifies the object.
C The second argument, "xwks", to the create call sets the name of the
C object being created. The third argument, "NhlFxWorkstationLayerClass", or
C "NhlFncgmWorkstationLayerClass" identifies the type or class of the object
C to create. In this case an X workstation or an NCGM workstation.
C The fourth argument, 0, specifies the id of the objects parent.  In this 
C case, the object has no parent, so the constant 0 is used.  
C The fifth argument, "rlist", is the resource list modifiers to be used 
C when creating the object.
C The final argument, ierr, is used to return an error code.


        call NhlFRLClear(rlist)
        call NhlFCreate(xwks,"xwks",NhlFxWorkstationLayerClass,0,
     $       rlist,ierr)

C The resource, wkMetaName, lets you specify the name of the output NCGM
C file.  In this example, it is called basic04.ncgm.  If omitted, the 
C default name, gmeta,  will be used.

        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,"wkMetaName","basic04.ncgm",ierr)
        call NhlFCreate(nwks,"nwks",NhlFncgmWorkstationLayerClass,0,
     $       rlist,ierr)

C Create a scalar field object that will be used as a data set for a
C contour object.  The sfDataArray resource is used to assign a data
C array to a scalar field data object.

        call NhlFRLClear(rlist)
        call NhlFRLSetMDIntegerArray(rlist,"sfDataArray",data1,2,
     $       dims,ierr)
        call NhlFCreate(field1,"field1",NhlFscalarFieldLayerClass,0,
     $       rlist,ierr)

C ##########
C # STEP 3 #
C ##########
C Create the object(s) you want to draw.
C
C Create a contour object to draw into the X workstation.
C Assign data using the cnScalarFieldData resource.

        call NhlFRLClear(rlist)
        call NhlFRLSetInteger(rlist,"cnScalarFieldData",field1,ierr)
        call NhlFCreate(xcon,"xcon",NhlFcontourLayerClass,xwks,
     $       rlist,ierr)

C Create an empty contour object to draw into the ncgm workstation.
C Assign data using the cnScalarFieldData resource.

        call NhlFRLClear(rlist)
        call NhlFRLSetInteger(rlist,"cnScalarFieldData",field1,ierr)
        call NhlFCreate(ncon,"ncon",NhlFcontourLayerClass,nwks,
     $       rlist,ierr)

C ##########
C # STEP 4 #
C ##########
C Draw the objects

	call NhlFDraw(xcon,ierr)
	call NhlFDraw(ncon,ierr)

C ##########
C # STEP 5 #
C ##########
C Call frame to update and clear the workstations

	call NhlFFrame(xwks,ierr)
	call NhlFFrame(nwks,ierr)

C ##########
C # STEP 6 #
C ##########
C Clean up memory.

	call NhlFDestroy(xwks,ierr)
	call NhlFDestroy(nwks,ierr)

	call NhlFClose

	stop
	end
