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
C                      Creates color bars with every 5th index of the
C                      114 different colors in the default colormap.
C
      external NhlFLabelBarClass
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
        
      integer appid, wid, pid
      integer rlist, ierr
      integer colors(22)
      character*15 line_labels(22)

      data colors / 1,6,11,16,21,26,31,36,41,46,51,56,
     $     61,66,71,76,81,86,91,96,101,106 /

      data line_labels / 'Color Index 1 ','Color Index 6 ',
     $     'Color Index 11','Color Index 16',
     $     'Color Index 21','Color Index 26',
     $     'Color Index 31','Color Index 36',
     $     'Color Index 41','Color Index 46',
     $     'Color Index 51','Color Index 56',
     $     'Color Index 61','Color Index 66',
     $     'Color Index 71','Color Index 76',
     $     'Color Index 81','Color Index 86',
     $     'Color Index 91','Color Index 96',
     $     'Color Index 101','Color Index 106'/

      integer NCGM
C
C Default is to display output to an X workstation
C
      NCGM=0
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
      else 
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'lb02Work',NhlFXWorkstationClass,
     $        0,rlist,ierr)
      endif
C
C Create a plot with 22 color indices (Every 5th one of the default
C workstation colormap.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetintegerarray(rlist,'lbFillColors',
     $     colors,22,ierr)
      call NhlFRLSetstringarray(rlist,'lbLabelStrings',
     $     line_labels,22,ierr)
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
