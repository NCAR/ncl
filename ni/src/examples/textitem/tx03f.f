CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tx03f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 mdt 1995
C
C      Description:    Demonstrates the TextItem Object
C                      Writes "NCAR Graphics" in a series of
C                      different colors (using the default colormap.)
C
      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFTextItemClass

      integer appid, wid, pid
      integer srlist, grlist,ierr
      integer i
      character*7  wks_type
C
C Default is to create a metafile.
C
      wks_type = "x11"
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
      call NhlFCreate(appid,'tx03',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./tx03f.ncgm',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFNcgmWorkstationClass,0,
     1        srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X Workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'tx03Work',
     $        NhlFCairoWindowWorkstationClass,
     $        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./tx03f.ps',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFPSWorkstationClass,0,
     1        srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./tx03f.pdf',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFPDFWorkstationClass,0,
     1        srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(srlist,'wkFileName','./tx03f',ierr)
         call NhlFCreate(wid,'tx03Work',
     1        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(srlist,'wkFileName','./tx03f',ierr)
         call NhlFCreate(wid,'tx03Work',
     1        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C Get the number of colors in the default color table.
C
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkColorMapLen',num_colors,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C
C
C Create a TextItem and then draw multiple frames varying the fill
C color of the text bounding box to all entries of the default
C workstation color map.
C
      call NhlFRLClear(srlist)
      call NhlFCreate(pid,'TextItems',NhlFTextItemClass,
     $        wid,srlist,ierr)
      do 10 i=1,num_colors,10
         call NhlFRLClear(srlist)
         call NhlFRLSetinteger(srlist,'txBackgroundFillColor',
     $        i,ierr)
         call NhlFSetValues(pid,srlist,ierr)
         call NhlFDraw(pid,ierr)
         call NhlFFrame(wid,ierr)
 10   continue
C
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
C
      stop
      end
