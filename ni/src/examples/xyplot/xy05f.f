C
C      $Id: xy05f.f,v 1.8 1995-04-04 21:56:14 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                Copyright (C)  1995                                  C
C        University Corporation for Atmospheric Research              C
C                All Rights Reserved                                  C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy05f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Tue Apr  4 09:49:35 MDT 1995
C
C  Description:    This example shows one way on how to create an
C                  XyPlot object with multiple lines and multiple
C                  lengths  using multiple data objects.  Some of the
C                  XyPlot line resources are tweaked to show how to
C                  change the appearances of the lines.
C
C                  The "CoordArrays" object is used to set up the data.
C                  (The C version uses the "CoordArrTable" object which
C                  is not available in Fortran or NCL.)
C
C
      external NhlFAppLayerClass
      external NhlFXWorkstationLayerClass
      external NhlFNcgmWorkstationLayerClass
      external NhlFCoordArraysLayerClass
      external NhlFXyPlotLayerClass

      parameter(NPTS=100,NCURVE=10,PI=3.14159)
C
C Create data arrays for XyPlot object.
C
      real y(NPTS,NCURVE)
      integer length(NCURVE)

      integer appid,xworkid,plotid,dataid(NCURVE)
      integer rlist, i, j, num
      character*10 datastr
      integer NCGM
C
C Default is to an X workstation.
C
      NCGM=0
C
C Initialize the HLU library and set up resource template
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy05.res" in this
C case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy05',NhlFAppLayerClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy05f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy05Work',
     +        NhlFNcgmWorkstationLayerClass,0,rlist,ierr)
      else
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy05Work',NhlFXWorkstationLayerClass,
     +                0,rlist,ierr)
      endif
C
C Initialize data.
C
      do 20 j = 1,NCURVE
         length(j) = NPTS - (j-1)*10
         do 10 i = 1,length(j)
            y(i,j) = real(j)*sin((2.*(i-1)*PI)/real(length(j)-1))
 10      continue
 20   continue
C
C Create NCURVE CoordArray objects which define the data for the XyPlot
C object. The id array from this object will become the value for the
C XyPlot resource, "xyCoordData".
C
      do 30 i=1,NCURVE
         call NhlFRLClear(rlist)
         call NhlFRLSetFloatArray(rlist,'caYArray',y(1,i),
     +        length(i),ierr)
         write(datastr,40)i-1
         call NhlFCreate(dataid(i),datastr,NhlFCoordArraysLayerClass,
     +                0,rlist,ierr)
 30   continue
 40   format('xyData',i1)
C
C Create the XyPlot object and tweak some of the tickmark, title and
C view port resources (some in the "xy05.res" resource file).
C
      call NhlFRLClear(rlist)
      call NhlFRLSetIntegerArray(rlist,'xyCoordData',dataid,NCURVE,
     +     ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFXyPlotLayerClass,xworkid,
     +     rlist,ierr)
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
