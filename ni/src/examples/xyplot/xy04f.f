C     
C      $Id: xy04f.f,v 1.2 1995-02-16 14:53:31 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy04f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Thu Feb  9 10:00:38 MST 1995
C
C  Description:    This program shows one way on how to create an XyPlot
C                  object with multiple lines in the plot using
C                  the CoordArrays object.  Some of the XyPlot line
C                  resources are tweaked in the resource file to
C                  show how to change the appearance of these multiple
C                  lines.
C
C                  The "CoordArrays" object is used to set up the data,
C                  and the "DataDep" object is used to describe
C                  attributes of the data being plotted, like the
C                  line color and the dash patterns.
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfcoordarrayslayerclass
      external nhlfxydatadeplayerclass
      external nhlfxyplotlayerclass

      parameter(NPTS=500,NCURVE=4)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid,datadepid
      integer rlist, i, j, len(2)
      real ydra(NPTS,NCURVE), theta
      data len/NPTS,NCURVE/
C
C Set up arrays of labels, colors, and dash patterns for each curve.
C
      integer colors(NCURVE),xydash(NCURVE)
      data colors/50, 75, 100, 40/
      data xydash/0, 5, 10, 15/
      character*7 explabs(NCURVE)
      data explabs/'Curve 1','Curve 2','Curve 3', 'Curve 4'/
C
C Initialize data for the XyPlot object.
C
      do 20 j = 1,NCURVE
         do 10 i = 1,NPTS
            theta = PI100*real(i-1)
            ydra(i,j) = j*100.+.9*real(i-1)*sin(theta)
 10      continue
 20   continue
C
C Initialize the HLU library and set up resource template.
C
      call nhlfinitialize
      call nhlfrlcreate(rlist,'setrl')
C
C Create Application and XWorkstation objects.  The Application
C object name is used to determine the name of the resource file,
C which is "xy04.res" in this case.
C
      call nhlfcreate(appid,'xy04',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy04Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the "CoordArrays" object.  Since only the Y values are
C specified here, each Y value will be paired with its integer
C array index.  The id for this object will then later be used as
C the value for the Data Dep resource, "dsDataItem".
C
      call nhlfrlclear(rlist)
      call nhlfrlsetmdfloatarray(rlist,'caYArray',ydra,2,len,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Create a DataDep object as a child of the XWorkstation object.
C DataDep resources are used for defining things like the color, label,
C and dash pattern of each line.  The id from this object will become
C the value for the XyPlot resource "xyCurveData".
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'dsDataItem',dataid,ierr)
      call nhlfrlsetintegerarray(rlist,'xyColors',colors,NCURVE,ierr)
      call nhlfrlsetintegerarray(rlist,'xyDashPatterns',xydash,NCURVE,
     +                          ierr)
      call nhlfrlsetstringarray(rlist,'xyExplicitLabels',explabs,NCURVE,
     +                          ierr)
      call nhlfcreate(datadepid,'xyDataDep',nhlfxydatadeplayerclass,
     +     xworkid,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.  The resources that are being changed are done
C in the "xy04.res" file, and they will affect this XyPlot object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'xyCurveData',datadepid,ierr)
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
