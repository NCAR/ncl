C     
C      $Id: xy05f.f,v 1.2 1995-02-16 14:53:35 haley Exp $
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
C                  The "CoordArrays" object is used to set up the data,
C                  and the "DataDep" object is used to describe
C                  attributes of the data being plotted, like the
C                  marker styles and sizes.
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfcoordarrayslayerclass
      external nhlfxydatadeplayerclass
      external nhlfxyplotlayerclass

      parameter(NPTS=500,NCURVE=4)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid
      integer dataid(ncurve),datadepid(ncurve)
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
      call nhlfcreate(appid,'xy05',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy05Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the Data objects.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id for
C each object will then later be used as a value for a DataDep
C resource, "dsDataItem".
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
C Create DataDep objects each as a child of the XWorkstation object.
C DataDep resources are used for defining things like the marker
C colors, styles, and sizes for each line.  The array of ids from
C these objects will become the value for the XyPlot resource
C "xyCurveData".
C
      do 60 i=1,NCURVE
         call nhlfrlclear(rlist)
         call nhlfrlsetinteger(rlist,'dsDataItem',dataid(i),ierr)
         write(datastr,70)i
         call nhlfcreate(datadepid(i),datastr,nhlfxydatadeplayerclass,
     +        xworkid,rlist,ierr)
 60   continue
 70   format('xyDataDep',i1)
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.  The resources that are being changed are done
C in the "xy05.res" file, and they will affect this XyPlot object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloatarray(rlist,'xyYIrregularPoints',
     +     explicit_values,10,ierr)
      call nhlfrlsetintegerarray(rlist,'xyCurveData',datadepid,NCURVE,
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

