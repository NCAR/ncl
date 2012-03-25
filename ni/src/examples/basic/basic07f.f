CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           basic07f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri May 25 18:31:18 mdt 1995
C
C      Description:    Demonstrates creating 3 simultaneous workstations.
C                      The TextItem output states which type of workstation,
C                      out of NCGM, PDF, PostScript, and X11.
C
      external NhlFAppClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFTextItemClass

      integer appid, widx,widn,widp,widpdf, pidx,pidn,pidp,pidpdf
      integer srlist, ierr
      integer i
C------------------------------------------------------
C
C  Call a subroutine that generates an LLU
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. In this example the resource file supplies the
C plot title only.
C
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'basic07',NhlFAppClass,0,srlist,ierr)
C
C Create an NCGM workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkMetaName','basic07f.ncgm',ierr)
      call NhlFCreate(widn,'basic07ncgm',NhlFNcgmWorkstationClass,0,
     1        srlist,ierr)
C
C Create an older-style PostScript workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPSFileName','basic07f.ps',ierr)
      call NhlFRLSetString(srlist,'wkOrientation','portrait',ierr)
      call NhlFRLSetString(srlist,'wkPSFormat','ps',ierr)
      call NhlFCreate(widp,'basic07ps',NhlFPSWorkstationClass,0,
     1        srlist,ierr)
C
C Create an older-style PDF workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPDFFileName','basic07f.pdf',ierr)
      call NhlFRLSetString(srlist,'wkOrientation','portrait',ierr)
      call NhlFRLSetString(srlist,'wkPDFFormat','pdf',ierr)
      call NhlFCreate(widpdf,'basic07pdf',NhlFPDFWorkstationClass,0,
     1        srlist,ierr)
C
C Create an X Workstation.
C
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'wkPause','True',ierr)
      call NhlFCreate(widx,'basic07x11',
     +     NhlFCairoWindowWorkstationClass,
     $     0,srlist,ierr)
C
C Create three plots, one for each workstation type.
C
C  Use color index 2
      i = 2
      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'txBackgroundFillColor',
     $     i,ierr)
      call NhlFCreate(pidx,'TextItems',NhlFTextItemClass,
     $     widx,srlist,ierr)

      call NhlFCreate(pidn,'TextItems',NhlFTextItemClass,
     $     widn,srlist,ierr)

      call NhlFCreate(pidp,'TextItems',NhlFTextItemClass,
     $     widp,srlist,ierr)
      call NhlFCreate(pidpdf,'TextItems',NhlFTextItemClass,
     $     widpdf,srlist,ierr)

      call NhlFDraw(pidx,ierr)
      call NhlFDraw(pidn,ierr)
      call NhlFDraw(pidp,ierr)
      call NhlFDraw(pidpdf,ierr)
      call NhlFFrame(widx,ierr)
      call NhlFFrame(widp,ierr)
      call NhlFFrame(widpdf,ierr)
      call NhlFFrame(widn,ierr)
C
      call NhlFDestroy(pidx,ierr)
      call NhlFDestroy(pidn,ierr)
      call NhlFDestroy(pidp,ierr)
      call NhlFDestroy(pidpdf,ierr)
      call NhlFDestroy(widx,ierr)
      call NhlFDestroy(widp,ierr)
      call NhlFDestroy(widpdf,ierr)
      call NhlFDestroy(widn,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
C
      stop
      end

