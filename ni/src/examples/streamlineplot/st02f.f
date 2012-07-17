C
C      $Id: st02f.f,v 1.6 2010-03-15 22:49:24 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1993                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       st02f.f
C
C  Author:     David Brown
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       Wed Apr  3 17:00:55 MST 1996
C
C   Description:   Given a simple mathematically generated data set,
C           demonstrates line-drawn streamline arrows and the use
C                  of some basic StreamlinePlot resources
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFVectorFieldClass
      external NhlFStreamlinePlotClass

      parameter(N=30,M=25)
      parameter(PI=3.14159)

      character*7  wks_type
      integer appid,wid,stid,vfid
      integer rlist,grlist
      integer len_dims(2)
      real U(M,N),V(M,N)
      real igrid, jgrid
      real stepsize,arrowlength,spacing
      integer i,j

C
C Define the workstation type
C
      wks_type = "x11"

C
C Generate vector data arrays
C
      igrid = 2.0 * PI / real(M)
      jgrid = 2.0 * PI / real(N)
      do 20 j = 1,N
         do 10 i=1,M
            U(i,j) = 10.0 * cos(jgrid * real(j-1))
            V(i,j) = 10.0 * cos(igrid * real(i-1))
 10      continue
 20   continue
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'st02',NhlFappClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./st02f.ncgm',ierr)
         call NhlFCreate(wid,'st02Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'st02Work',
     1        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./st02f.ps',ierr)
         call NhlFCreate(wid,'st02Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./st02f.pdf',ierr)
         call NhlFCreate(wid,'st02Work',NhlFPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./st02f',ierr)
         call NhlFCreate(wid,'st02Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./st02f',ierr)
         call NhlFCreate(wid,'st02Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create a VectorField data object using the data set defined above.
C By default the array bounds will define the data boundaries
C (zero-based, as in C language conventions)
C
      len_dims(1) = M
      len_dims(2) = N
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFVectorFieldClass,appid,
     1     rlist,ierr)
C
C Create a StreamlinePlot object, supplying the VectorField object as data
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Modifying StreamlinePlot resources',ierr)
      call NhlFRLSetInteger(rlist,'stVectorFieldData',vfid,ierr)
      call NhlFCreate(stid,'streamlineplot',NhlFStreamlinePlotClass,wid,
     1     rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Get the values of several resources that are set dynamically based
C on the assumed NDC size of a grid cell. Each of this will be separately
C modified in the course of this example to illustrate their effect.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'stStepSizeF',stepsize,ierr)
      call NhlFRLGetFloat(grlist,'stArrowLengthF',arrowlength,ierr)
      call NhlFRLGetFloat(grlist,'stMinLineSpacingF',spacing,ierr)
      call NhlFGetValues(stid,grlist,ierr)
C
C Increase the step size 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Larger Step Size',ierr)
      call NhlFRLSetFloat(rlist,'stStepSizeF',stepsize * 4.0,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Decrease the step size 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Smaller Step Size',
     1                     ierr)
      call NhlFRLSetFloat(rlist,'stStepSizeF',stepsize * 0.25,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Increase the minimum line spacing 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Larger Minimum Line Spacing',ierr)
      call NhlFRLSetFloat(rlist,'stStepSizeF',stepsize,ierr)
      call NhlFRLSetFloat(rlist,'stMinLineSpacingF', spacing * 4.0,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Decrease the minimum line spacing
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Smaller Minimum Line Spacing',ierr)
      call NhlFRLSetFloat(rlist,'stMinLineSpacingF',spacing * 0.25,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Increase the line starting grid stride 
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Larger Line Starting Grid Stride',ierr)
      call NhlFRLSetFloat(rlist,'stMinLineSpacingF',spacing,ierr)
      call NhlFRLSetInteger(rlist,'stLineStartStride',3,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Decrease the line starting grid stride
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Smaller Line Starting Grid Stride',ierr)
      call NhlFRLSetInteger(rlist,'stLineStartStride',1,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Increase the arrow size
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString','Larger Arrows',ierr)
      call NhlFRLSetInteger(rlist,'stLineStartStride',2,ierr)
      call NhlFRLSetFloat(rlist,'stArrowLengthF',arrowlength*2.0,ierr)
      call NhlFSetValues(stid,rlist,ierr)

      callNhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
