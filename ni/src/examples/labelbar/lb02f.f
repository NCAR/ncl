CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lb02c.c
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
      external nhlfhlulayerclass
      external nhlfreslistlayerclass
      external nhlflabelbarlayerclass
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass

        
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

C
C Initialize the high level utility library
C
      call nhlfinitialize

C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call nhlfrlcreate(rlist,'setrl')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfcreate(appid,'lb02',nhlfapplayerclass,0,rlist,ierr)

C
C Create an xworkstation object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'wkPause',true,ierr)
      call nhlfcreate(wid,'lb02Work',nhlfxworkstationlayerclass,
     $     0,rlist,ierr)

C
C Create a plot with 22 color indices (Every 5th one of the default
C workstation colormap.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetintegerarray(rlist,'lbFillColors',
     $     colors,22,ierr)
      call nhlfrlsetstringarray(rlist,'lbLabelStrings',
     $     line_labels,22,ierr)
      call nhlfrlsetfloat(rlist,'vpXF',0.,ierr)
      call nhlfrlsetfloat(rlist,'vpYF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpWidthF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',1.,ierr)
      call nhlfcreate(pid,'LabelBar',nhlflabelbarlayerclass,
     $     wid,rlist,ierr)

      call nhlfdraw(pid,ierr)
      call nhlfframe(wid,ierr)
      call nhlfdestroy(pid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose

      stop
      end
