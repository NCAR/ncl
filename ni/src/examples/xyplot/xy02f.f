C
C      $Id: xy02f.f,v 1.2 1995-02-09 23:07:16 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy02f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Wed Feb  8 11:44:39 MST 1995
C
C  Description:    This program shows how to create an XY plot object
C                  with all the default resources being used, with the
C                  exception of the data resource.  There's no "default
C                  data", so we need to create some.  A resource file
C                  is used to change the resources except in those cases
C                  where a resource has to be changed programmatically,
C                  like array resources.
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
      integer rlist, i, j
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
C Create application and X workstation object.  The application name
C is used to determine the name of the resource file, which will be
C "xy02.res" in this case.
C
      call nhlfcreate(appid,'xy02',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy02Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the data object.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id
C for this object will later be used as the value for the XYPlot data
C resource, 'xyCurveData'.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloatarray(rlist,'caYArray',ydra,NPTS,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Create the Plot object which is created as a child of the X 
C workstation object.  The resources that are being changed are done 
C in the "xy02.res" file, and they affect this Plot object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'xyCurveData',dataid,ierr)
      call nhlfcreate(plotid,'xyPlot',nhlfxyplotlayerclass,xworkid,
     +                rlist,ierr)
C
C Draw the plot (to its parent X Workstation)
C
      call nhlfdraw(plotid,ierr)
      call nhlfframe(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying 'xworkid' will also destroy plotid.
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

