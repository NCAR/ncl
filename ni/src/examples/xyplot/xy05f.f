C     
C      $Id: xy05f.f,v 1.7 1995-04-01 16:24:53 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy05f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Thu Feb  9 15:17:35 MST 1995
C
C  Description:    This example shows how to create an XyPlot object
C                  with multiple lines using the CoordArrays and
C                  multiple Data objects.  Using multiple Data
C                  objects allows you to have a different number of
C                  points in each line.
C
C                  Some of the XyPlot marker resources are tweaked in
C                  the resource file to show how to change the
C                  appearance of these multiple lines.  This example
C                  also shows you how to use the xyYIrregularPoints
C                  resource to define your own Y axis values.
C
C                  The "CoordArrays" object is used to set up the data.
C
      external NhlFAppLayerClass
      external NhlFXWorkstationLayerClass
      external NhlFNcgmWorkstationLayerClass
      external NhlFCoordArraysLayerClass
      external NhlFXyPlotLayerClass

      parameter(NPTS=500,NCURVE=4)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid
      integer dataid(ncurve)
      integer rlist, i, j, len(NCURVE)
      real explicit_values(10)
      character*10 datastr
      real ydra(NPTS,NCURVE), theta
      data len/500,200,400,300/
      integer NCGM
C
C Default is to an X workstation.
C
      NCGM=0
C
C Initialize some data for the XyPlot object.
C
      do 20 j = 1,NCURVE
         do 10 i = 1,len(j)
            theta = PI100*real(i-1)
            ydra(i,j) = j*100.+.9*real(i-1)*sin(theta)
 10      continue
 20   continue
C
C Set up the array of points we want to use for the Y axis.
C
      do 30 i = 1,10
         explicit_values(i) = 2.**real(i+3)
 30   continue
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy05.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy05',NhlFAppLayerClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./xy05f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy05Work',
     +        NhlFNcgmWorkstationLayerClass,0,rlist,ierr)
      else
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy05Work',NhlFXWorkstationLayerClass,
     +        0,rlist,ierr)
      endif
C
C Define the Data objects.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id for
C each object will then later be used in the xyCoordData resource of
C the plot.
C
      do 40 i=1,NCURVE
         call NhlFRLClear(rlist)
         call NhlFRLSetfloatarray(rlist,'caYArray',ydra(1,i),len(i),
     +        ierr)
         write(datastr,50)i
         call NhlFCreate(dataid(i),datastr,NhlFCoordArraysLayerClass,
     +        appid,rlist,ierr)
 40   continue
 50   format('xyData',i1)
C
C This array of Data objects is now the resource value for
C xyCoordData.  Tweak some more XYPlot resources in the resource file
C An XyDataSpec object gets created by XyPlot internally to deal
C with each DataItem that is in the xyCoordData resource.  So,
C you can set Data Specific resources using the name of each data
C item that you add.  See the resource file ("xy05.res").
C
      call NhlFRLClear(rlist)
      call NhlFRLSetfloatarray(rlist,'xyYIrregularPoints',
     +     explicit_values,10,ierr)
      call NhlFRLSetintegerarray(rlist,'xyCoordData',dataid,NCURVE,
     +     ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFXyPlotLayerClass,xworkid,
     +                rlist,ierr)
C
C Draw the plot (to its parent XWorkstation).
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying "xworkid" will also destroy "plotid".
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(xworkid,ierr)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

