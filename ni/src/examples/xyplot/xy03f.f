C     
C      $Id: xy03f.f,v 1.5 1995-02-18 00:53:46 boote Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy03f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Wed Feb  8 11:44:39 MST 1995
C
C  Description:    This program shows how to create an XyPlot object
C                  with some of the XyPlot line resources tweaked.  A
C                  resource file is used to changed the resources 
C                  except in those cases where a resource is an array
C                  and can only be changed programmatically.
C                  This program uses the same Y-axis dataset as the
C                  example "xy02", but this time values for the X
C                  axis are specified, changing the look of the plot.
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
      real xdra(NPTS), ydra(NPTS), theta
C
C Initialize some data for the XyPlot object.
C
      do 10 i = 1,NPTS
         theta = PI100*real(i-1)
         xdra(i) = 500.+.9*real(i-1)*cos(theta)
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
C "xy03.res" in this case.
C
      call nhlfcreate(appid,'xy03',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy03Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the data object.  The id for this object will later be used
C as the value for the XyPlot data resource, 'xyCoordData'.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloatarray(rlist,'caXArray',xdra,NPTS,ierr)
      call nhlfrlsetfloatarray(rlist,'caYArray',ydra,NPTS,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C Xworkstation object.  The resources that are being changed are done
C in the "xy03.res" file, and they will affect this XyPlot object.
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

