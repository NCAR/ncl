C     
C      $Id: xy04f.f,v 1.5 1995-02-26 14:20:00 haley Exp $
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
C                  and the resource file is used to set up attributes
C                  of the data being plotted, like the line color and
C                  the dash patterns.
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfcoordarrayslayerclass
      external nhlfxyplotlayerclass

      parameter(NPTS=500,NCURVE=4)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid
      integer rlist, i, j, len(2)
      real ydra(NPTS,NCURVE), theta
      data len/NPTS,NCURVE/
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
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'xy04',nhlfapplayerclass,0,rlist,ierr)

      call nhlfcreate(xworkid,'xy04Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the data object.  The id for this object will later be used
C as the value for the XYPlot data resource, 'xyCoordData'.
C Since only the Y values are specified here, each Y value will be
C paired with its integer array index.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetmdfloatarray(rlist,'caYArray',ydra,2,len,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.  The resources that are being changed are done
C in the "xy04.res" file, and they will affect this XyPlot object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'xyCoordData',dataid,ierr)
      call nhlfcreate(plotid,'xyPlot',nhlfxyplotlayerclass,xworkid,
     +                rlist,ierr)
C
C Draw the plot (to its parent X Workstation)
C
      call nhlfdraw(plotid,ierr)
      call nhlfframe(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying 'appid' will destroy 'xworkid' which will also destroy
C plotid.
C
      call nhlfrldestroy(rlist)
      call nhlfdestroy(appid,ierr)
C
C Restores state.
C
      call nhlfclose
      stop
      end
