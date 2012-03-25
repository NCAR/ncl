C
C      $Id: xy08f.f,v 1.6 2010-03-15 22:49:25 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                Copyright (C)  1995                                  C
C        University Corporation for Atmospheric Research              C
C                All Rights Reserved                                  C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy08f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Tue Apr  4 09:49:35 MDT 1995
C
C  Description: This example is similar to the ncargex Autograph
C               example "agex13".  It shows how to use Irregular
C               points to change the transformation of your plot.
C
C               The "CoordArrays" object is used to set up the data.
C               (The C version uses the "CoordArrTable" object which
C               is not available in Fortran or NCL.)
C
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

      parameter(NPTS=61,NCURVE=3,NCOLORS=3)
C
C Create data arrays for XyPlot object.
C
      real xdra(NPTS,NCURVE), ydra(NPTS,NCURVE), xcoord(2*NPTS,NCURVE)
      integer len(NCURVE)
      data len/12,37,61/

      integer appid,xworkid,plotid,dataid(NCURVE)
      integer rlist, i, j
      character*10 datastr
C
C Create array for customizing the plot.
C
      real explicit_values(14)
C
C Set up data file name.
C
      character*8 filenm
      data filenm/'xy08.asc'/
C
C Modify the color map.  Color indices '1' and '2' are the background
C and foreground colors respectively.
C
      integer clen(2)
      data clen/3,NCOLORS/
      real cmap(3,NCOLORS)
      data cmap/1.00,1.00,1.00,
     +          0.00,0.00,0.00,
     +          0.00,0.00,1.00/

      CHARACTER*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"
C
C
C Fill the data arrays.
C
      do 10 i = 1,14
         explicit_values(i) = 2.**real(i-7)
 10   continue
      open(unit=10,file=filenm,status='old',form='formatted',err=104)
      do 30 i = 1,NCURVE
         read(10,1001)(xcoord(j,i),j=1,len(i)*2)
         do 20 j = 1,len(i)*2
            xcoord(j,i) = 2.**((xcoord(j,i)-15.)/2.5)
 20      continue
 30   continue
 1001 format(1X,24F5.1)
      do 50 i = 1,NCURVE
         do 40 j = 2,len(i)*2,2
            xdra(j/2,i) = xcoord(j-1,i)
            ydra(j/2,i) = xcoord(j,i)
 40      continue
 50   continue
C
C Initialize the HLU library and set up resource template
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy08.res" in this
C case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy08',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy08f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +                NhlFCairoWindowWorkstationClass,
     +                0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy08f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy08f.pdf',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy08f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +        NhlFcairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./xy08f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,clen,ierr)
         call NhlFCreate(xworkid,'xy08Work',
     +        NhlFcairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create NCURVE CoordArray objects which define the data for the XyPlot
C object. The id array from this object will become the value for the
C XyPlot resource, "xyCoordData".
C
      do 60 i=1,NCURVE
         call NhlFRLClear(rlist)
         call NhlFRLSetFloatArray(rlist,'caYArray',ydra(1,i),
     +        len(i),ierr)
         call NhlFRLSetFloatArray(rlist,'caXArray',xdra(1,i),
     +        len(i),ierr)
         write(datastr,70)i-1
         call NhlFCreate(dataid(i),datastr,NhlFCoordArraysClass,
     +                0,rlist,ierr)
 60   continue
 70   format('xyData',i1)
C
C Create the XyPlot object and customize tick marks.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'tmXBValues',explicit_values,14,
     +     ierr)
      call NhlFRLSetFloatArray(rlist,'tmYLValues',explicit_values,14,
     +     ierr)
      call NhlFRLSetFloatArray(rlist,'xyXIrregularPoints',
     +     explicit_values,14,ierr)
      call NhlFRLSetFloatArray(rlist,'xyYIrregularPoints',
     +     explicit_values,14,ierr)
      call NhlFRLSetIntegerArray(rlist,'xyCoordData',dataid,NCURVE,
     +     ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFXyPlotClass,xworkid,
     +     rlist,ierr)
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
 104  write (6,*) 'error in opening file: ',filenm
      end
