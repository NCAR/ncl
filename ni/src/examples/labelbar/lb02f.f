CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lb02f.f
C
C      Author:         Bob Lackman
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 mdt 1995
C
C      Description:    Demonstrates the LabelBar Object
C                      Creates color bars with every other index of the
C                      32 different colors in the default colormap.
C
      external NhlFLabelBarClass
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr
      integer colors(16)
      character*15 line_labels(16)

      data colors / 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31/

      data line_labels / 'Color Index 1 ','Color Index 3 ',
     $     'Color Index 5  ','Color Index 7  ',
     $     'Color Index 9  ','Color Index 11 ',
     $     'Color Index 13 ','Color Index 15 ',
     $     'Color Index 17 ','Color Index 19 ',
     $     'Color Index 21 ','Color Index 23 ',
     $     'Color Index 25 ','Color Index 27 ',
     $     'Color Index 29 ','Color Index 31 '/

      integer NCGM, X11, PS
C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0
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
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'lb02',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./lb02f.ncgm',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFNcgmWorkstationClass,0,rlist,ierr) 
      else  if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'lb02Work',NhlFXWorkstationClass,
     $        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./lb02f.ps',ierr)
         call NhlFCreate(wid,'lb02Work',
     $        NhlFPSWorkstationClass,0,rlist,ierr) 
      endif
C
C Create a plot with 16 color indices (Every other one of the default
C workstation colormap.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetintegerarray(rlist,'lbFillColors',
     $     colors,16,ierr)
      call NhlFRLSetstringarray(rlist,'lbLabelStrings',
     $     line_labels,16,ierr)
      call NhlFRLSetfloat(rlist,'vpXF',0.,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',1.,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',1.,ierr)
      call NhlFCreate(pid,'LabelBar',NhlFLabelBarClass,
     $     wid,rlist,ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
