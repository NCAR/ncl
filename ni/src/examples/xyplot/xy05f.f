C     
C      $Id: xy05f.f,v 1.5 1995-03-01 18:37:45 haley Exp $
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
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfcoordarrayslayerclass
      external nhlfxyplotlayerclass

      parameter(NPTS=500,NCURVE=4)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid
      integer dataid(ncurve)
      integer rlist, i, j, len(NCURVE)
      real explicit_values(10)
      character*10 datastr
      real ydra(NPTS,NCURVE), theta
      data len/500,200,400,300/
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
      call nhlfinitialize
      call nhlfrlcreate(rlist,'setrl')
C
C Create Application and XWorkstation objects.  The Application
C object name is used to determine the name of the resource file,
C which is "xy05.res" in this case.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'xy05',nhlfapplayerclass,0,rlist,ierr)

      call nhlfcreate(xworkid,'xy05Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the Data objects.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id for
C each object will then later be used in the xyCoordData resource of
C the plot.
C
      do 40 i=1,NCURVE
         call nhlfrlclear(rlist)
         call nhlfrlsetfloatarray(rlist,'caYArray',ydra(1,i),len(i),
     +        ierr)
         write(datastr,50)i
         call nhlfcreate(dataid(i),datastr,nhlfcoordarrayslayerclass,
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
      call nhlfrlclear(rlist)
      call nhlfrlsetfloatarray(rlist,'xyYIrregularPoints',
     +     explicit_values,10,ierr)
      call nhlfrlsetintegerarray(rlist,'xyCoordData',dataid,NCURVE,
     +     ierr)
      call nhlfcreate(plotid,'xyPlot',nhlfxyplotlayerclass,xworkid,
     +                rlist,ierr)
C
C Draw the plot (to its parent XWorkstation).
C
      call nhlfdraw(plotid,ierr)
      call nhlfframe(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying "xworkid" will also destroy "plotid".
C
      call nhlfrldestroy(rlist)
      call nhlfdestroy(xworkid,ierr)
      call nhlfdestroy(appid,ierr)
C
C Restores state.
C
      call nhlfclose
      stop
      end

