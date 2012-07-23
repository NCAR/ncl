C
C      $Id: st01f.f,v 1.5 2010-03-15 22:49:24 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       st01f.f
C
C  Author:     David Brown
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri May 10 10:52:47 MDT 1996
C
C  Description:  Basic StreamlinePlot example
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
      integer rlist
      integer len_dims(2)
      real U(M,N),V(M,N)
      real igrid, jgrid
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
C directory so the application looks for a resource file in the
C working directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'st01',NhlFappClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./st01f.ncgm',ierr)
         call NhlFCreate(wid,'st01Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'st01Work',
     1        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr) 
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./st01f.ps',ierr)
         call NhlFCreate(wid,'st01Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./st01f.pdf',ierr)
         call NhlFCreate(wid,'st01Work',NhlFPDFWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./st01f',ierr)
         call NhlFCreate(wid,'st01Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./st01f',ierr)
         call NhlFCreate(wid,'st01Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create a VectorField object; then use its id as the value of
C the 'stVectorFieldData' resource when creating the StreamlinePlot
C object.
C
      len_dims(1) = M
      len_dims(2) = N
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFVectorFieldClass,appid,
     1     rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Basic StreamlinePlot Example',ierr)
      call NhlFRLSetInteger(rlist,'stVectorFieldData',vfid,ierr)
      call NhlFCreate(stid,'streamline',NhlFStreamlinePlotClass,wid,
     1     rlist,ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
