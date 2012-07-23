CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C) 1995                                    C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           ti02f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 mdt 1995
C
C      Description:    Demonstrates the title object
C                      defaults.
C
      external NhlFAppClass
      external NhlFTitleClass
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
C Default is to create an X workstation.
C
      wks_type = "x11"
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. In this example the resource file supplies
C the plot title only.
C
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'ti02',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./ti02f.ncgm',ierr)
         call NhlFCreate(wid,'ti02Work',NhlFNcgmWorkstationClass,0,
     $        rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'ti02Work',
     +        NhlFCairoWindowWorkstationClass,
     1        0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./ti02f.ps',ierr)
         call NhlFCreate(wid,'ti02Work',NhlFPSWorkstationClass,0,
     $        rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./ti02f.pdf',ierr)
         call NhlFCreate(wid,'ti02Work',NhlFPDFWorkstationClass,0,
     $        rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./ti02f',ierr)
         call NhlFCreate(wid,'ti02Work',
     $        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./ti02f',ierr)
         call NhlFCreate(wid,'ti02Work',
     $        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Specify the viewport extent of the object.
C
       call NhlFRLClear(rlist)
       call NhlFRLSetfloat(rlist,'vpXF',.2,ierr)
       call NhlFRLSetfloat(rlist,'vpYF',.8,ierr)
       call NhlFRLSetfloat(rlist,'vpWidthF',.6,ierr)
       call NhlFRLSetfloat(rlist,'vpHeightF',.6,ierr)

       call NhlFCreate(pid,'Titles',NhlFTitleClass,wid,rlist,ierr)

       call NhlFDraw(pid,ierr)
       call NhlFFrame(wid,ierr)
       call NhlFDestroy(pid,ierr)
       call NhlFDestroy(wid,ierr)
       call NhlFDestroy(appid,ierr)
       call NhlFClose

       stop
       end
