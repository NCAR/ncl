C
C      $Id: xy01f.f,v 1.15 2010-03-15 02:06:27 haley Exp $
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
C                  using all the default resources except for the data
C                  resource; since there's no "default data", we need
C                  to create some.  A resource file is included with
C                  this example, but only to show what all the XyPlot
C                  resources are and what their defaults are set to.
C                  The whole resource file is commented out.
C                  The "CoordArrays" object is used to set up the data.
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass
C
C Define the number of points in the curve.
C
      parameter(NPTS=500)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid
      integer rlist, i
      real   ydra(NPTS), theta
      CHARACTER*7  wks_type

      wks_type = "x11"

C
C Initialize some data for the XY plot.
C
      do 10 i = 1,NPTS
         theta = PI100*real(i-1)
         ydra(i) = 500.+.9*real(i-1)*sin(theta)
 10   continue
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy01.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy01',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy01f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy01f.ps',ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy01f.pdf',ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a PS/PDF cairo workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy01f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a PNG image workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy01f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy01Work',
     +        NhlFcairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Define the data object.  Since only the Y values are specified here,
C each Y value will be paired with its integer array index.  The id for
C this object will later be used as the value for the XyPlot data
C resource, "xyCoordData".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra,NPTS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,
     +                0,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFXyPlotClass,xworkid,
     +                rlist,ierr)
C
C Draw the plot.
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
