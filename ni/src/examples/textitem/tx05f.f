CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tx05f.f
C
C      Author:         Fred Clare (converted to Fortran by David
C                      Younghans)
C                      National Center for Atmospheric Research
C                      PO 3000, Boulder, Colorado                      
C                      (translated to Fortran by David Younghans)  
C
C      Date:           Tue Nov 21 15:55 MST 1995
C
C      Description:    Demonstrates the TextItem object with text having
C                      various heights and at various angles.
C

      program tx05f
      implicit none

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlfTextItemclass

      integer   appid, wid, pid, rlist, ierr
      real height, angle, dtr
      real bkg_color(3)
      real x_coord, y_coord
      integer NCGM, X11, PS

      dtr=0.017453292519943
      bkg_color(1)=1.0
      bkg_color(2)=1.0
      bkg_color(3)=1.0
C
C Set the display.  Default is to dispaly output to an X workstation.
C
      NCGM=0
      X11=1
      PS=0
C
C Initialize the high level utility library and create application.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tx05f',NhlFAppClass,0,rlist,ierr)

      if (NCGM .eq. 1) then
C
C Create a meta file workstation.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,'wkMetaName','./tx05f.ncgm',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'tx05Work',NhlFNcgmWorkstationClass,
     &       0,rlist,ierr)

      else if (X11 .eq. 1) then
C
C Create an XWorkstation object.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetInteger(rlist,'wkPause','True',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'t x 0 5 W o r leftk',
     &       NhlFXWorkstationClass,0, rlist,ierr)

      else if (PS .eq. 1) then
C
C Create a PS workstation.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetString(rlist,'wkPSFileName',
     &       './tx05f.ps',ierr)
        call NhlFRLSetFloatArray(rlist,'wkBackgroundColor',
     &       bkg_color,3,ierr)
        call NhlFCreate(wid,'tx05Work',NhlFPsWorkstationClass,
     &       0,rlist,ierr)

      endif
C
C Create a TextItem object.
C
      call NhlFSetColor(wid,1, 0.0, 0.0, 1.0,ierr)
      call NhlFSetColor(wid,2, 0.4, 0.0, 0.4,ierr)

      call NhlFCreate(pid,'TextItems',NhlFTextItemClass,wid,0,ierr)

C
C  Set text font and string.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'txFont',22,ierr)
      call NhlFRLSetString(rlist,'txString', 'NCAR',ierr)
      call NhlFSetValues(pid,rlist,ierr)

C
C Draw string with various heights and at various angles.
C

      angle = 0.0
 100  if (angle .lt. 136.) then
         x_coord = 0.3 + 0.4*cos(dtr*angle)
         y_coord = 0.2 + 0.4*sin(dtr*angle)
         height  = 0.0005*(136.-angle)

         call NhlFRLClear(rlist)
         call NhlFRLSetFloat(rlist,'txAngleF', angle,ierr)
         call NhlFRLSetFloat(rlist,'txPosXF', x_coord,ierr)
         call NhlFRLSetFloat(rlist,'txPosYF', y_coord,ierr)
         call NhlFRLSetFloat(rlist,'txFontHeightF', height,ierr)
         call NhlFSetValues(pid,rlist,ierr)
         call NhlFDraw(pid,ierr)

         if (210.*height .gt. 1.) then
            angle = angle + 210.*height
         else
            angle = angle + 1.
         endif
         
         goto 100
      endif

C
C Text strings at specific angles.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txAngleF', 180.0,ierr)
      call NhlFRLSetInteger(rlist,'txFont' ,22,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor' ,1,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.04,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.25,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.34,ierr)
      call NhlFRLSetString(rlist,'txString', 'NCAR',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFRLSetInteger(rlist,'txFontColor' ,2,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.03,ierr)
      call NhlFRLSetFloat(rlist,'txAngleF', 0.0,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.4,ierr)
      call NhlFRLSetString(rlist,'txString', '180 degrees',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txAngleF', -45.0,ierr)
      call NhlFRLSetInteger(rlist,'txFont' ,22,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor' ,1,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.04,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.7,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.6,ierr)
      call NhlFRLSetString(rlist,'txString', 'NCAR',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFRLSetInteger(rlist,'txFontColor' ,2,ierr)
      call NhlFRLSetFloat(rlist,'txAngleF', 0.0,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.03,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.73,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.65,ierr)
      call NhlFRLSetInteger(rlist,'txJust' ,1,ierr)
      call NhlFRLSetString(rlist,'txString', '-45 degrees',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

C
C Label the plot.
C

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txAngleF', 0.0,ierr)
      call NhlFRLSetInteger(rlist,'txFont' ,25,ierr)
      call NhlFRLSetInteger(rlist,'txJust' ,1,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor' ,2,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF', 0.05,ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',  0.2,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',  0.84,ierr)
      call NhlFRLSetString(rlist,'txString', 'Text heights &',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)

      call NhlFRLSetFloat(rlist,'txPosYF', 0.76,ierr)
      call NhlFRLSetString(rlist,'txString', 'Text angles',ierr)
      call NhlFSetValues(pid,rlist,ierr)
      call NhlFDraw(pid,ierr)
    
      call NhlFFrame(wid,ierr)

      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFClose

      stop
      end
