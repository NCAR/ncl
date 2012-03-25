C
C     $Id: xy11f.f,v 1.12 1998-11-24 17:06:24 haley Exp $
C
C****************************************************************
C                                                               *
C           Copyright (C)  1995                                 *
C   University Corporation for Atmospheric Research             *
C           All Rights Reserved                                 *
C                                                               *
C****************************************************************
C
C      File:            xy11f.f
C
C      Author:          Jeff Boote (converted to Fortran by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Wed Mar 30 18:09:45 MST 1994
C
C      Description:     This example program demonstrates how to display
C                       your graphics to an X window, and then copy it
C                       into a meta file.
C
C
C Extern declarations for Types of objects that will be used
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass

      integer appid
C
C Create data arrays to be plotted
C

      real Temp(28)
      real Pressure(28)

      data (Temp(i), i = 1,28) /-13.500000,-10.500000,-5.500000,
     %-3.100000,-1.300000,-6.700000,-12.900000,-13.100000,-17.700001,
     %-16.900000,-21.299999,-37.099998,-40.099998,-41.900002,-42.099998,
     %-62.700001,-59.299999,-60.700001,-52.099998,-55.099998,-55.900002,
     %-63.500000,-57.900002,-60.500000,-58.099998,-60.900002,-54.900002,
     %-57.299999 /

      data (Pressure(i), i = 1,28) /835.000000,832.000000,827.000000,
     %821.000000,791.000000,693.000000,627.000000,606.000000,560.000000,
     %555.000000,500.000000,383.000000,355.000000,339.000000,314.000000,
     %209.000000,192.000000,143.000000,111.000000,100.000000,95.300003,
     %76.400002,62.599998,58.299999,47.299999,30.000000,27.200001,
     %21.500000/
C
C   Initialize the error return variable
C
      data ierr / 0 /
C
C   Initialize the HLU library
C
      call NhlFInitialize
      call NhlFCreate(appid,'xy11',NhlFAppClass,0,0,ierr)
C
C   Create a ResList - This is filled with resource "Names" and
C   "Values" to apply to an HLU object.
C
      call NhlFRLCreate(list,'setrl')
C
C   Fill the ResList with the resources for the X Workstation
C   object - then create the X Workstation object.
C
      call NhlFRLClear(list)
      call NhlFRLSetInteger(list,'wkPause',1,ierr)
      call NhlFCreate(ixwork,'myxwork',
     +        NhlFCairoWindowWorkstationClass,0,
     %  list,ierr)

C
C   Fill the ResList with the resources for the NCGM Workstation
C   object - then create the NCGM Workstation object.
C
      call NhlFRLClear(list)
      call NhlFRLSetString(list,'wkMetaName','xy11f.ncgm',ierr)
      call NhlFCreate(incgmwork,'myncgmwork',
     %  NhlFNcgmWorkstationClass,0,list,ierr)

C
C   Fill the ResList with the resources for the CoordArrays
C   object - then create the CoordArrays data object.
C   This object is used to describe data to the HLU library.
C
      call NhlFRLClear(list)
      call NhlFRLSetFloatArray(list,'caXArray',Temp,28,ierr)
      call NhlFRLSetFloatArray(list,'caYArray',Pressure,28,ierr)
      call NhlFCreate(idata,'mydata',NhlFCoordArraysClass,0,list,
     %  ierr)
C
C   Fill the ResList with the resources for the XyPlot object
C   including the "Data" which is the object id for the
C   CoordArrays object that was just created.
C
      call NhlFRLClear(list)
      call NhlFRLSetFloat(list,'vpXF',0.25,ierr)
      call NhlFRLSetFloat(list,'vpYF',0.75,ierr)
      call NhlFRLSetFloat(list,'vpWidthF',0.5,ierr)
      call NhlFRLSetFloat(list,'vpHeightF',0.5,ierr)

      call NhlFRLSetInteger(list,'xyCoordData',idata,ierr)

      call NhlFRLSetInteger(list,'tiMainOn',1,ierr)
      call NhlFRLSetInteger(list,'tiXAxisOn',1,ierr)
      call NhlFRLSetInteger(list,'tiYAxisOn',1,ierr)

      call NhlFCreate(ixyplot,'myxyplot',NhlFXyPlotClass,ixwork,
     %  list,ierr)
C
C   We don't need the ResList any more - so destroy it.  Memory
C   is allocated for each reslist so it is important to destroy
C   them when you are done with them.
C
      call NhlFRLDestroy(list)
C
C   Draw the XyPlot - It was created as a child of the X Workstation
C   so it will draw to the X Workstation's device (Window).  Then
C   Call "Frame" for the X Workstation - this flushes the graphics
C   buffers so all the graphics are displayed - and since we
C   set the wkPause resource to True the program will pause on
C   the frame call until the user "clicks" in the Window.
C
      call NhlFDraw(ixyplot,ierr)
      call NhlFFrame(ixwork,ierr)
C
C   Move the XyPlot to the Ncgm Workstation - This makes the
C   XyPlot a child of the Ncgm Workstation so the XyPlot
C   will draw to the Ncgm Workstations' device instead of the
C   X Workstations device.
C
      call NhlFChangeWorkstation(ixyplot,incgmwork,ierr)
C
C   Draw the XyPlot - It is now a child of the Ncgm Workstation
C   so it will draw to the Ncgm Workstation's device (ncgm file).
C   Then C  Call "Frame" for the Ncgm Workstation - this flushes
C   the graphics C  buffers so all the graphics are displayed.
C
      call NhlFDraw(ixyplot,ierr)
      call NhlFFrame(incgmwork,ierr)
C
C   Destroy the X Workstation object - this also pop's down
C   the X Window. (Also free's all the memory associated with
C   the X Workstation)
C
      call NhlFDestroy(ixwork,ierr)
C
C   Destroy the Ncgm Workstation object - this closes the metafile
C   and puts the ENDMETAFILE marker in the file.
C   This also destroys the XyPlot object since the XyPlot is a
C   child of the Ncgm Workstation.
C
      call NhlFDestroy(incgmwork,ierr)

C
C   Close the library.
C
      call NhlFClose

      end
