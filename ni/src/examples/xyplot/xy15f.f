C
C      $Id: xy15f.f,v 1.5 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy15f.f
C
C  Author:     David Brown (converted to Fortran by Mary Haley)
C           National Center for Atmospheric Research
C           PO 3000, Boulder, Colorado
C
C  Date:       Tue Oct 17 11:22:10 MDT 1995
C
C Description:  
C       This example illustrates the creation of a set of 4
C       of 'stacked' XyPlots. Each plot has the same X axis.
C       By making the top 3 plots into annotations of the 
C       bottom plot, all four plots can be manipulated as
C       a unit. To demonstrate this concept the second frame sets
C       the viewport of the base plot. Because all the annotations
C       have their "amResizeNotify" resource set to true (in the
C       resource file), all the annotation plots resize themselves
C       proportionally to the change in the size of the base plot.
C       Each plot draws a variation of sinusoidal curve.
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass
C
C Define the number of curves and points in each curve.
C
      parameter(NPTS=500,NCURVE=4,NCOLORS=6,PI100=0.031415926535898)

      real ydra1(NPTS),ydra2(NPTS),ydra3(NPTS),ydra4(NPTS)

      integer   appid,xworkid
      integer   dataid1,dataid2,dataid3,dataid4
      integer   xy1, xy2, xy3, xy4
      integer   am2, am3, am4
      integer   rlist, i
      real      theta
      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Initialize data for the XyPlot object.
C
      do 10 i = 1,NPTS
         theta = PI100*real(i-1)
         ydra1(i) = sin(theta)
         ydra2(i) = sin(theta * theta)
         ydra3(i) = sin(exp(theta))
         ydra4(i) = sin(3*sqrt(abs(theta)))
 10   continue
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy15.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy15',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy15f.ncgm',ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy15f.ps',ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy15f.pdf',ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy15f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy15f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(xworkid,'xy15Work',
     +        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Define 4 separate CoordArrays objects - one for each XYPlot. 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra1,NPTS,ierr)
      call NhlFCreate(dataid1,'xyData1',NhlFcoordArraysClass,appid,
     +     rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra2,NPTS,ierr)
      call NhlFCreate(dataid2,'xyData2',NhlFcoordArraysClass,appid,
     +     rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra3,NPTS,ierr)
      call NhlFCreate(dataid3,'xyData3',NhlFcoordArraysClass,appid,
     +     rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra4,NPTS,ierr)
      call NhlFCreate(dataid4,'xyData4',NhlFcoordArraysClass,appid,
     +     rlist,ierr)
C
C Create 4 XyPlot objects. 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid1,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.3,ierr)
      call NhlFCreate(xy1,'xy1',NhlFxyPlotClass,xworkid,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid2,ierr)
      call NhlFCreate(xy2,'xy2',NhlFxyPlotClass,xworkid,rlist,ierr)


      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid3,ierr)
      call NhlFCreate(xy3,'xy3',NhlFxyPlotClass,xworkid,rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid4,ierr)
      call NhlFCreate(xy4,'xy4',NhlFxyPlotClass,xworkid,rlist,ierr)

      call NhlFAddAnnotation(xy1,xy2,am2)
      call NhlFAddAnnotation(xy1,xy3,am3)
      call NhlFAddAnnotation(xy1,xy4,am4)
C
C Draw the plot.
C
      call NhlFDraw(xy1,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Now set the viewport of the base plot only redrawing the base plot
C causes all the other plots to be redrawn as well. Notice that they
C are all resized and repositioned to match the new size and position
C of the base plot. The whole assemblage functions as a single 
C composite plot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.65,ierr)
      call NhlFRLSetFloat(rlist,'vpXF',0.25,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',0.4,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',0.16,ierr)
      call NhlFSetValues(xy1,rlist,ierr)
    
      call NhlFDraw(xy1,ierr)
      call NhlFFrame(xworkid,ierr)
C
C Destroy the resource list.
C The workstation, all the plots, and the data objects are all
C descended from the App object, so it is only necessary to 
C destroy the App object in order to destroy all the objects created
C in this example.
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
