CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tx06f.f
C
C      Author:         Fred Clare (converted to Fortran by 
C                      David Younghans)
C                      National Center for Atmospheric Research
C                      PO 3000, Boulder, Colorado
C                      (translated to Fortran by David Younghans)
C
C      Date:           Mon Nov  27 12:42:08 MST 1995
C
C      Description:    Demonstrates TextItem text justifications.
C
C

      program tx06f
      implicit none
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFXWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFTextItemClass

      integer   appid, wid, pid, rlist, ierr
      character*13 labels(9)
      real char_height, Xmarker_height, y_coord
      real bkg_color(3)
      integer just
      real bb_top,bb_bottom,bb_left,bb_right
      integer NCGM, X11, PS

      data labels/'Top Left','Center Left','Bottom Left',
     &     'Top Center','Center Center','Bottom Center',
     &     'Top Right','Center Right','Bottom Right'/
      char_height = 0.035
      Xmarker_height = 0.03
      bkg_color(1) = 1.0
      bkg_color(2) = 1.0
      bkg_color(3) = 1.0
C
C Set the display.  Default is to display output to an X workstation.
C
      NCGM=0
      X11=1
      PS=0
C
C  Initialize the high level utility library and create application.
C 
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tx06f',NhlFAppClass,0,rlist,ierr)

      if (NCGM .eq. 1) then
C
C Create a meta file workstation.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,'wkMetaName','./tx06f.ncgm',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'tx06Work',NhlFNcgmWorkstationClass,
     &       0,rlist,ierr)
    
      else if (X11 .eq. 1) then
C
C Create an XWorkstation object.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,'wkPause','True',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'tx06Work',
     &       NhlFxWorkstationClass,0,rlist,ierr)

      else if (PS .eq. 1) then
C
C Create a PS workstation.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,'wkPSFileName','./tx06c.ps',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'tx06Work',NhlFpsWorkstationClass,
     &       0,rlist,ierr)
      endif
C
C Create a TextItem object.
C
      call NhlFSetColor(wid,1,0.0, 0.0, 1.0,ierr)
      call NhlFSetColor(wid,2,1.0, 0.0, 0.0,ierr)
      call NhlFSetColor(wid,3,0.4, 0.0, 0.4,ierr)

      call NhlFCreate(pid,'TextItem',NhlFtextItemClass,wid,0,ierr)

C
C  Run through the text justifications.
C

      do 100 just=0,8
  
C
C  Set up and draw a text string.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'txString', labels(just+1),ierr)
      call NhlFRLSetInteger(rlist,'txFontColor', 1,ierr)
      call NhlFRLSetInteger(rlist,'txJust', just,ierr)
      call NhlFRLSetInteger(rlist,'txFont', 22,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', char_height,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF', 0.5,ierr)
      y_coord = 0.08*just+0.1
      if ( (mod(just,3)) .eq. 0) then
         y_coord = y_coord + 0.45*char_height
      
      else if ( (mod(just,3)) .eq. 2) then
         y_coord = y_coord - 0.45*char_height
      
      endif
      call NhlFRLSetFloat(rlist,'txPosYF', y_coord,ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

C
C  Mark the justification point.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'txString', 'X',ierr)
      call NhlFRLSetInteger(rlist,'txJust', 4,ierr)
      call NhlFRLSetInteger(rlist,'txFont', 22,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', Xmarker_height,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor', 2,ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

 100  end do
C
C  Label the plot.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'txFont' ,25,ierr)
      call NhlFRLSetInteger(rlist,'txJust' ,4,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor' ,3,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.045,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.5,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.92,ierr)
      call NhlFRLSetString(rlist,'txString', 'Text Justifications',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFRLSetFloat(rlist,'txPosXF', 0.5+0.5*char_height,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF', 0.84,ierr)
      call NhlFRLSetString(rlist,'txString', 
     &     ' - Marks the justification point',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFGetBB(pid,bb_top,bb_bottom,bb_left,bb_right,ierr)
      call NhlFRLSetInteger(rlist,'txFont' ,22,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor' ,2,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', Xmarker_height,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',bb_left,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.837,ierr)
      call NhlFRLSetInteger(rlist,'txJust' ,7,ierr)
      call NhlFRLSetString(rlist,'txString', 'X',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFFrame(wid,ierr)

      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFClose

      stop
      end

