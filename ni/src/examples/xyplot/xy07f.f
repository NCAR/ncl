C
C      $Id: xy07f.f,v 1.1 1995-04-24 21:26:29 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy07f.f
C
C  Author:     Mary Haley (converted from example "agex11")
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Apr 24 11:11:02 MST 1995
C
C  Description:   This example is similar to the ncargex Autograph
C                 example "agex11".  It shows how to draw a "scattergram".
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFCoordArraysClass
      external NhlFXyPlotClass

      parameter(NPTS=250)
C
C Create data arrays for XyPlot object.
C
      real xdra(NPTS), ydra(NPTS)

      integer appid,xworkid,plotid,dataid(2)
      integer list, i, j
      real x
      character*10 datastr
      integer NCGM
C
C Default is to an X workstation.
C
      NCGM=0
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy07.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy07',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGMWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy07f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy07Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else
C
C Create an XWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy07Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      endif
C
C Since we have two sets of points that we want to color differently,
C we need to create two Data obects here.
C
      do 20 j = 1,2
C
C Initialize data.
C
         do 10 i = 1, NPTS
            xdra(i)=.5+(2.*(fran()-.5))**5
            ydra(i)=.5+(2.*(fran()-.5))**5
 10     continue
C
C Define a data object.  Note that we are naming each object differently
C so we can distinguish them in the resource file.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetFloatArray(rlist,'caXArray',xdra,NPTS,ierr)
        call NhlFRLSetFloatArray(rlist,'caYArray',ydra,NPTS,ierr)
        write(datastr,30)j-1
        call NhlFCreate(dataid(j),datastr,NhlFcoordArraysClass,0,rlist,
     +                  ierr)
 20   continue
 30   format('xyData',i1)
C
C Create the XyPlot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetIntegerArray(rlist,'xyCoordData',dataid,2,ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFxyPlotClass,xworkid,rlist,
     +     ierr)
C
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying xworkwid will also destroy plotid.
C
      call NhlFDestroy(xworkid,ierr)
C
C Restores state.
C
      call NhlFDestroy(appid)
      call NhlFClose

      stop
      end
      
      function fran()
C
C Pseudo-random-number generator.
C
      double precision x
      save x
      data x / 2.718281828459045 /
      x=mod(9821.d0*x+.211327d0,1.d0)
      fran=real(x)
      return
      end
