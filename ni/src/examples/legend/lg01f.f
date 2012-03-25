CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lg01c.c
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates the Legend Object defaults.
C

      external NhlFAppClass
      external NhlFLegendClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr

      character*7  wks_type
C
C Default is to display output to an X workstation
C
      wks_type = "x11"
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'lg01',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lg01f.ncgm',ierr)
         call NhlFCreate(wid,'lg01Work',
     1        NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'lg01Work',
     +        NhlFCairoWindowWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./lg01f.ps',ierr)
         call NhlFCreate(wid,'lg01Work',
     1        NhlFPSWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./lg01f.pdf',
     1        ierr)
         call NhlFCreate(wid,'lg01Work',
     1        NhlFPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lg01f',
     1        ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lg01Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr) 
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkFileName','./lg01f',
     1        ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'lg01Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr) 
      endif
C     
C Specify the viewport extent of the object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',0.,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',1.,ierr)
      call NhlFCreate(pid,'Legend',NhlFLegendClass,wid,rlist,
     1      ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
