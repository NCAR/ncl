C
C      $Id: xy01f.f,v 1.5 1995-02-18 00:53:42 boote Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy01f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Wed Feb  8 11:44:39 MST 1995
C
C  Description:    This program shows how to create an XyPlot object
C                  with all the default resources being used, with the
C                  exception of the data resource.  There's no "default
C                  data", so we need to create some.  A resource file
C                  is included with this example, but only to show what
C                  all the XyPlot resources are and what their defaults
C                  are set to. The whole resource file is commented out.
C
C                  The "CoordArrays" object is used to set up the data.
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlfxyplotlayerclass
      external nhlfcoordarrayslayerclass

      parameter(NPTS=500)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid
      integer rlist, i
      real   ydra(NPTS), theta
C
C Initialize some data for the XY plot
C
      do 10 i = 1,NPTS
         theta = PI100*real(i-1)
         ydra(i) = 500.+.9*real(i-1)*sin(theta)
 10   continue
C
C Initialize the HLU library and set up resource template.
C
      call nhlfinitialize
      call nhlfrlcreate(rlist,'setrl')
C
C Create Application and XWorkstation objects.  The Application object
C name is used to determine the name of the resource file, which is
C "xy01.res" in this case.
C
      call nhlfcreate(appid,'xy01',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy01Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the data object.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id for
C this object will later be used as the value for the XYPlot data
C resource, 'xyCoordData'.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloatarray(rlist,'caYArray',ydra,NPTS,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'xyCoordData',dataid,ierr)
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

