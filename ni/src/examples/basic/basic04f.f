C
C $Id: basic04f.f,v 1.9 1995-06-29 00:06:53 scheitln Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            basic04f.f
C
C      Author:          Tim Scheitlin (converted by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar 20 10:43:42 MST 1995
C
C      Description:     This example demonstrates how to select and
C                       change the workstation device for drawing your
C                       output to an NCGM file or an X workstation
C                       window using the following steps.
C
C                       1. Create output workstation objects.
C                       2. Create the data for the plots.
C                       3. Create the contour object.
C                       4. Draw the contour object.
C                       5. Call frame.
C                       6. Change workstations.
C                       7. Draw the contour object.
C                       8. Call frame.
C                       9. Clean up memory.
C
      program basic04f
      implicit none

      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFContourPlotClass
      external NhlFScalarFieldClass

      integer appid,nwks,xwks,xcon,field1,rlist,ierr

      integer data1(5,5)
      data data1 / 3,4,4,5,5,
     $     2,3,5,5,4,
     $     2,4,5,4,4,
     $     3,4,4,4,3,
     $     3,3,3,3,3 /

      integer dims(2)
      data dims / 5, 5 /
C
C ##########
C # STEP 1 #
C ##########
C Initialize the graphics libraries and create a resource list that
C is normally used to assign name/value pairs within objects.  Then
C clear (empty) this list, and create an application object.  This
C object manages multiple resource databases used by separate objects.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFCreate(appid,'appid',NhlFAppClass,0,
     $     rlist,ierr)
C
C ##########
C # STEP 2 #
C ##########
C For each type of output you must create a workstation object using
C create.  The first argument, xwks, is a variable that identifies the
C object. The second argument, "xwks", to the create call sets the
C name of the object being created. The third argument, 
C "NhlFCairoWindowWorkstationClass" or "NhlFNcgmWorkstationClass"
C identifies the type or class of the object to create. In this case
C an X workstation or an NCGM workstation. The fourth argument, 0,
C specifies the id of the objects parent.  In this  case, the object
C has no parent, so the constant 0 is used.   The fifth argument,
C "rlist", is the resource list modifiers to be used  when creating
C the object. The final argument, ierr, is used to return an error code.
C
      call NhlFRLClear(rlist)
      call NhlFCreate(xwks,'xwks',NhlFCairoWindowWorkstationClass,0,
     $     rlist,ierr)
C
C The resource, wkMetaName, lets you specify the name of the output NCGM
C file.  In this example, it is called basic04f.ncgm.  If omitted, the 
C default name, gmeta,  will be used.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkMetaName','basic04f.ncgm',ierr)
      call NhlFCreate(nwks,'nwks',NhlFNcgmWorkstationClass,0,
     $     rlist,ierr)
C
C Create a scalar field object that will be used as a data set for a
C contour object.  The sfDataArray resource is used to assign a data
C array to a scalar field data object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDIntegerArray(rlist,'sfDataArray',data1,2,
     $     dims,ierr)
      call NhlFCreate(field1,'field1',NhlFScalarFieldClass,0,
     $     rlist,ierr)
C
C ##########
C # STEP 3 #
C ##########
C Create the object(s) you want to draw.
C
C Create a contour object to draw into the X workstation.
C Assign data using the cnScalarFieldData resource.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'cnScalarFieldData',field1,ierr)
      call NhlFCreate(xcon,'xcon',NhlFContourPlotClass,xwks,
     $     rlist,ierr)
C
C ##########
C # STEP 4 #
C ##########
C Draw the object
C
      call NhlFDraw(xcon,ierr)
C
C ##########
C # STEP 5 #
C ##########
C Call frame to update and clear the workstations
C
      call NhlFFrame(xwks,ierr)
C
C ##########
C # STEP 6 #
C ##########
C Change workstations.
      call NhlFChangeWorkstation(xcon, nwks, ierr)
C
C ##########
C # STEP 7 #
C ##########
C Draw the object.
      call NhlFDraw(xcon,ierr)
C
C ##########
C # STEP 8 #
C ##########
C Call frame.
      call NhlFFrame(nwks,ierr)
C
C ##########
C # STEP 9 #
C ##########
C Clean up memory.
C
      call NhlFDestroy(xwks,ierr)
      call NhlFDestroy(nwks,ierr)

      call NhlFClose

      stop
      end

