C     
C      $Id: xy04f.f,v 1.1 1995-02-09 23:07:22 haley Exp $
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
C  Description:    This program shows one way on how to create an XY
C                  plot object with multiple lines in the plot using
C                  the CoordArrays object.  Some of the XY line
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
C Initialize some data for the XY plot
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
C Create application and X workstation object.  The application name
C is used to determine the name of the resource file, which will be
C 'xy04.res' in this case.
C
      call nhlfcreate(appid,'xy04',nhlfapplayerclass,0,0,ierr)
      call nhlfcreate(xworkid,'xy04Work',nhlfxworkstationlayerclass,
     +                0,0,ierr)
C
C Define the data object.  The id for this object will later be used
C as the value for the XYPlot data resource, 'xyCurveData'.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetmdfloatarray(rlist,'caYArray',ydra,2,len,ierr)
      call nhlfcreate(dataid,'xyData',nhlfcoordarrayslayerclass,
     +                0,rlist,ierr)
C
C Define Data Dependent resources.  Here's where you specify the arrays
C for defining the color, label, and dash pattern of each line, 
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'dsDataItem',dataid,ierr)
      call nhlfrlsetintegerarray(rlist,'xyColors',colors,NCURVE,ierr)
      call nhlfrlsetintegerarray(rlist,'xyDashPatterns',xydash,NCURVE,
     +                          ierr)
      call nhlfrlsetstringarray(rlist,'xyExplicitLabels',explabs,NCURVE,
     +                          ierr)
      call nhlfcreate(datadepid,'XYDep',nhlfxydatadeplayerclass,0,rlist,
     +                ierr)
C
C This new Data Dependent object is now the resource value for
C xyCurveData.  Tweak some XYPlot resources as well (in the resource
C file).
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'xyCurveData',datadepid,ierr)
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

