CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                Copyright (C)  1995                                  C
C        University Corporation for Atmospheric Research              C
C                All Rights Reserved                                  C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tx07f.f
C
C      Author:         Fred Clare (converted to Fortran by David
C                      Younghans)
C                      National Center for Atmospheric Research
C                      PO 3000, Boulder, Colorado
C                      (translated to Fortran by David Younghans)
C
C      Date:           Tue Nov  27 16:00:58 MST 1995
C
C      Description:    Demonstrates TextItem text spacings and
C                      aspect ratios.
C
      program tx07f
      implicit none
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFTextItemClass

      integer   i,ierr,appid,wid,exid,labid,rlist
      real ypos,aspect
      real bkg_color(3),spacings(3)
      character*25  label
      character*7  wks_type

      data bkg_color/1.0,1.0,1.0/
      data spacings/0.0,1.5,0.6/
C
C Set the display.  Default is to display output to an X workstation.
C
      wks_type = "x11"
C
C  Initialize.
C 
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tx07',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create a meta file workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName',
     &        './tx07f.ncgm',ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',NhlFncgmWorkstationClass,
     &        0,rlist,ierr)

      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetInteger(rlist,'wkPause','True',ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',
     &        NhlFCairoWindowWorkstationClass,0, rlist,ierr)

      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName',
     &        './tx07f.ps',ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',NhlFpsWorkstationClass,
     &        0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then

C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName',
     &        './tx07f.pdf',ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',NhlFpdfWorkstationClass,
     &        0,rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then

C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName',
     &        './tx07f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',
     &        NhlFCairoPSpdfWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then

C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName',
     &        './tx07f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &        bkg_color,3,ierr)
         call NhlFCreate(wid,'tx07Work',
     &        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create two TextItem objects.
C
      call NhlFSetColor(wid,1,0.0, 0.0, 1.0,ierr)
      call NhlFSetColor(wid,2,0.4, 0.0, 0.4,ierr)
      call NhlFSetColor(wid,3,1.0, 0.0, 0.0,ierr)
      
      call NhlFCreate(exid,'Example String',
     &     NhlFtextItemClass,wid,0,ierr)
      call NhlFCreate(labid,'Label String',NhlFtextItemClass,wid,0,ierr)
C
C  Set up example string.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'txString', 'NCAR Graphics',ierr)
      call NhlFRLSetInteger(rlist,'txFontColor', 1,ierr)
      call NhlFRLSetInteger(rlist,'txFont', 25,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.5,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', .05,ierr)
      call NhlFSetValues(exid,rlist,ierr)
C
C  Set up label string.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'txFontColor', 2,ierr)
      call NhlFRLSetInteger(rlist,'txFont', 21,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.5,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', .04,ierr)
      call NhlFSetValues(labid,rlist,ierr)
C
C  Spacings
C
      do 100 i=0,2
         ypos = 0.83 - 0.17 * real(i)
         call NhlFRLClear(rlist)
        
         write (label,900) spacings(i+1)
         call NhlFRLSetString(rlist,'txString',label,ierr)
         call NhlFRLSetFloat(rlist,'txPosYF',ypos,ierr)
         call NhlFSetValues(labid,rlist,ierr)
         call NhlFDraw(labid,ierr)
  
         call NhlFRLClear(rlist)
         call NhlFRLSetFloat(rlist,'txConstantSpacingF',
     &        spacings(i+1),ierr)
         call NhlFRLSetFloat(rlist,'txPosYF',ypos - 0.07,ierr)
         call NhlFSetValues(exid,rlist,ierr)
         call NhlFDraw(exid,ierr)
 100  continue
 900  format ('txConstantSpacingF = ',f3.1)
C
C  Aspect ratios.
C
      do 200 i=0,1
         ypos = 0.32 - 0.17 * real(i)
         aspect = 1.3 * real(i) + 0.7
         call NhlFRLClear(rlist)

         write (label,910) aspect         
         call NhlFRLSetString(rlist,'txString',label,ierr)
         call NhlFRLSetFloat(rlist,'txPosYF',ypos,ierr)
         call NhlFSetValues(labid,rlist,ierr)
         call NhlFDraw(labid,ierr)
  
         call NhlFRLClear(rlist)
         call NhlFRLSetFloat(rlist,'txConstantSpacingF',0.0,ierr)
         call NhlFRLSetFloat(rlist,'txPosYF',ypos - 0.07,ierr)
         call NhlFRLSetFloat(rlist,'txFontAspectF',aspect,ierr)
         call NhlFSetValues(exid,rlist,ierr)
         call NhlFDraw(exid,ierr)
 200  continue
 910  format ('txFontAspectF = ',f3.1)
C
C  Plot title.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'txFontColor', 3,ierr)
      call NhlFRLSetInteger(rlist,'txFont', 25,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.93,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', .045,ierr)
      call NhlFRLSetString(rlist,'txString',
     &     'Text Spacings and Aspect Ratios',ierr)
      call NhlFSetValues(labid,rlist,ierr)
      call NhlFDraw(labid,ierr)

      call NhlFFrame(wid,ierr)
C
C  Close things down.
C
      call NhlFDestroy(exid,ierr)
      call NhlFDestroy(labid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFClose

      stop
      end
