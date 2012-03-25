C     
C      $Id: xy04f.f,v 1.18 2010-03-15 22:49:25 haley Exp $
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
C                  of the data being plotted, like the line color, the
C                  dash patterns, and line label colors.
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCoordArraysClass
      external NhlFXyPlotClass
C
C Define the number of curves and points in each curve.
C
      parameter(NPTS=500,NCURVE=4,NCOLORS=6)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid
      integer rlist, i, j, len(2)
      real ydra(NPTS,NCURVE), theta
C
C Modify the color map using named colors.  Color indices '1' and '2'
C are the background and foreground colors respectively.
C
      character*6 cmap(NCOLORS)
      data cmap/"black","white","red","green","blue","yellow"/

      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize data for the XyPlot object.
C
      do 20 j = 1,NCURVE
         do 10 i = 1,NPTS
            theta = PI100*real(i-1)
            ydra(i,j) = (j-1)*200.+(i-1)*.9*sin(theta)
 10      continue
 20   continue
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy04.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy04',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy04f.ncgm',ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +                NhlFCairoWindowWorkstationClass,
     +                0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy04f.ps',ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy04f.pdf',ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +        NhlFcairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy04f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetStringArray(rlist,'wkColorMap',cmap,NCOLORS,ierr)
         call NhlFCreate(xworkid,'xy04Work',
     +        NhlFcairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Define the data object.  The id for this object will later be used
C as the value for the XYPlot data resource, 'xyCoordData'.
C Since only the Y values are specified here, each Y value will be
C paired with its integer array index.
C
      len(1) = NPTS
      len(2) = NCURVE
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'caYArray',ydra,2,len,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,
     +                0,rlist,ierr)
C
C This new DataItem object is now the resource value for xyCoordData.
C Tweak some XyPlot resources as well (in the resource file).
C Also tweak some XyDataSpe resources in the resource file.
C When Data is added to an XyPlot object, an XyDataSpec object is
C created internally to the XyPlot.  It has the same name as the Data
C object that is added, and so you can set XyDataSpec (XyPlot Data
C Specific) resources for each piece of data added to the xyCoordData
C resource.
C
C Create the XyPlot object which is created as a child of the
C XWorkstation object.  The resources that are being changed are done
C in the "xy04.res" file.
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
