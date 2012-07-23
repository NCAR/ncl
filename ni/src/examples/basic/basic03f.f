C
C $Id: basic03f.f,v 1.10 1995-06-20 15:19:52 stautler Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            basic03f.f
C
C      Author:          Tim Scheitlin (converted by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar 20 10:43:42 MST 1995
C
C      Description:     This example demonstrates how to:
C                       1. Create a scalar field data object and assign
C                          it to a plot.
C                       2. Set resources using a resource file.
C                       3. Set resources during object creation.
C                       4. Set resources after object creation.
C
      program basic03f
      implicit none

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFContourPlotClass
      external NhlFScalarFieldClass

      integer appid1,appid2,wks,wks2,con1,con2,con3,field1,rlist,ierr

      integer data1(5,5) 
      data data1 / 3,4,4,5,5,
     1      2,3,5,5,4,
     2      2,4,5,4,4,
     3      3,4,4,4,3,
     4      3,3,3,3,3 /

      integer dims(2) 
      data dims / 5, 5 /
C
C Initialize the graphics libraries and create a resource list that
C is normally used to assign name/value pairs within objects.  Then
C clear (empty) this list, and create an application object.  This
C object manages multiple resource databases used by separate objects.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFCreate(appid1,"appid1",NhlFAppClass,0,
     1      rlist,ierr)
C
C ###########
C # FRAME 1 #
C ###########
C This frame demonstrates how to create and assign data to a contour
C plot.
C

C
C Create an X workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'wkPause','True',ierr)
      call NhlFCreate(wks,"wks",
     +        NhlFCairoWindowWorkstationClass,0,rlist,ierr)
C
C Create a scalar field object that will be used as a data set for a 
C contour object.  The sfDataArray resource is used to assign a data
C array to a scalar field data object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDIntegerArray(rlist,"sfDataArray",data1,2,
     1      dims,ierr)
      call NhlFCreate(field1,"field1",NhlFScalarFieldClass,0,
     1      rlist,ierr)
C
C Create a contour plot object and assign the data using the
C cnScalarFieldData resource.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,"cnScalarFieldData",field1,ierr)
      call NhlFCreate(con1,"con1",NhlFContourPlotClass,wks,
     1      rlist,ierr)
C
C Draw the plot. 
C
      call NhlFDraw(con1,ierr)
C
C Update and clear the workstation.
C
      call NhlFFrame(wks,ierr)
C
C ###########
C # FRAME 2 #
C ###########
C This frame demonstrates how to set resources using a resource file.
C
C Resources are read from a resource file called basic03.res because
C the first argument in the create call is "basic03". This resource file
C is only read at the time an application object is created.
C The resource file contains resource assignments that control the
C characteristics of a plot.
C
      call NhlFRLClear(rlist)
      call NhlFCreate(appid2,"basic03",NhlFAppClass,0,
     1      rlist,ierr)
C
C Create another workstation window and make it a child of the
C new application object by using the appid2 variable as the argument
C for the parent id.  By making this a child of the application
C object, the resources that are set in the basic03.res resource
C file will apply to this object and its children.
C
      call NhlFRLClear(rlist)
      call NhlFCreate(wks2,"wks2",NhlFCairoWindowWorkstationClass,
     1      appid2,rlist,ierr)
C
C Create another contour plot object and assign the data.
C Notice that the parent id is wks2, making the contour object
C a child of the new workstation.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,"cnScalarFieldData",field1,ierr)
      call NhlFCreate(con2,"con2",NhlFContourPlotClass,wks2,
     1      rlist,ierr)
C
C The contour object is drawn with filled contours because there is
C a resource in basic03.res that specifies that contour fill is on.
C
      call NhlFDraw(con2,ierr)
C
C Updates and clear the workstation.
C
      call NhlFFrame(wks2,ierr)
C
C ###########
C # FRAME 3 #
C ###########
C This frame demonstrates how resources can be set when an object is
C created.  
C
C A variable length list of resource name/value pairs specifies
C a resource and its value.  In this example contour line labels are
C turned off by setting the "cnLineLabelsOn" resource to "False".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,"cnScalarFieldData",field1,ierr)
      call NhlFRLSetString(rlist,"cnLineLabelsOn","False",ierr)
      call NhlFCreate(con3,"con3",NhlFContourPlotClass,wks2,
     1      rlist,ierr)
C
C Draw the contour object.
C
      call NhlFDraw (con3,ierr)
C
C Update and clear the workstation.
C
      call NhlFFrame(wks2,ierr)
C
C ###########
C # FRAME 4 #
C ###########
C This frame demonstrates how to use the setvalues expression to set 
C resources for an object that has already been created.
C
C The setvalues expression is used to assign values to the resources
C of a object whose id is given as the first argument in the expression.
C In this example, that argument is "con3."
C
C Any resource that is valid for the con3 object can be set in the
C following expression.  In this example, setting "cnFillOn" to
C "False" turns contour fill off.  By default, cnFillOn is "False",
C but since it is set to "True" in the resource file, we can override
C that value by using the setvalues expression.  
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,"cnFillOn","False",ierr)
      call NhlFSetValues(con3,rlist,ierr)
C
C Draw the contour object.
C
      call NhlFDraw (con3,ierr)
C
C Update and clear the workstation
C
      call NhlFFrame(wks2,ierr)
C
C Clean up (deleting the parent object recursively deletes all of its 
C children).
C
      call NhlFDestroy(wks,ierr)
      call NhlFDestroy(appid1,ierr)
      call NhlFDestroy(appid2,ierr)

      call NhlFClose

      stop
      end
