C
C $Id: basic02f.f,v 1.10 1995-06-22 21:07:25 haley Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            basic02f.f
C
C      Author:          Tim Scheitlin (converted by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Mar 20 10:43:42 MST 1995
C
C      Description:     The first frame in this example demonstrates how
C                       to set the view port for a contour plot.
C                       Note: no data is used in this example, so the
C                       output appears only as a bounding box with
C                       tickmarks.
C
C                       The second frame in this example demonstrates how
C                       to produce multiple plots on a single frame.
C
      program basic02f
      implicit none

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFContourPlotClass

      integer appid,wks,con1,rlist,ierr
      integer NCGM, X11, PS	
C
C Initialize the graphics libraries and create a resource list that
C is normally used to assign name/value pairs within objects.  Then
C clear (empty) this list, and create an application object.  This
C object manages multiple resource databases used by separate objects.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFCreate(appid,"basic02",NhlFAppClass,0,rlist,ierr)
C
C ###########
C # FRAME 1 #
C ###########
C Choose the type of output you want to create.  You may write your
C output to an NCGM, file, X workstation window, or a PostScript file. 
C This example writes to a meta file or an X Workstation.
C 

C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./basic02f.ncgm',ierr)
         call NhlFCreate(wks,"wks",NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wks,"wks",NhlFXWorkstationClass,0,
     1        rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./basic02f.ps',ierr)
         call NhlFCreate(wks,"wks",NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      endif
C
C Create a plot object.  In this example, we will create a contour plot.
C
C Four view class resources, vpXF, vpYF, vpWidthF, and vpHeightF, are
C assigned values in the following create call.  The combination of
C these four resources determines where the plot will display in the
C output window.  The values of these resources are specified in 
C Normalized Device Coordinates (NDCs).  In this two-dimensional
C coordinate system (0,0) specifies the lower-left corner and (1,1)
C specifies the upper-right corner of a plot.

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,"vpXF",0.05,ierr) 
      call NhlFRLSetFloat(rlist,"vpYF",0.95,ierr) 
      call NhlFRLSetFloat(rlist,"vpWidthF",0.4,ierr) 
      call NhlFRLSetFloat(rlist,"vpHeightF",0.4,ierr) 
      call NhlFCreate(con1,"con1",NhlFContourPlotClass,wks,
     $     rlist,ierr)
C
C Draw the plot. 
C
      call NhlFDraw(con1,ierr)
C
C The frame call updates and then clears the workstation.
C Anything written to the workstation after a frame call is made will be
C drawn in a subsequent frame. 
C
      call NhlFFrame(wks,ierr)
C
C ###########
C # FRAME 2 #
C ###########
C
C This example demonstrates drawing multiple plots in a single frame.
C
C Calling draw again will produce the identical plot that was drawn in
C the first frame.
C
      call NhlFDraw(con1,ierr)
C
C To add another plot to the same frame, we first need to reset the 
C viewport resources so that the next plot does not overwrite the first
C one.  The setvalues expression is used to set resources after an object
C has already been created.  The first argument, "con1", in the setvalues
C expression specifies an object id of a plot that was generated earlier
C with the create call.  This is then followed by a list of resource
C value pairs that apply to the object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,"vpXF",0.55,ierr) 
      call NhlFRLSetFloat(rlist,"vpYF",0.45,ierr) 
      call NhlFRLSetFloat(rlist,"vpWidthF",0.2,ierr) 
      call NhlFRLSetFloat(rlist,"vpHeightF",0.2,ierr) 
      call NhlFSetValues(con1,rlist,ierr)
C
C Because of the new viewport resource settings, calling draw produces 
C a plot in the lower-right quadrant of the frame.
C
      call NhlFDraw(con1,ierr)
C
C Updates and clear the workstation.
C
      call NhlFFrame(wks,ierr)
C
C Clean up (deleting the parent object recursively deletes all of its 
C children).
C
      call NhlFDestroy(con1,ierr)
      call NhlFClose

      stop
      end
