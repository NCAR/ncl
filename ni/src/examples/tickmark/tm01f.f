CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tm01f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 MDT 1995
C
C      Description:    Demonstrates the TickMark Object
C                      defaults.
C
      external nhlfapplayerclass
      external nhlftickmarklayerclass
      external nhlfxworkstationlayerclass
      external nhlfncgmworkstationlayerclass
        
      integer appid, wid, pid
      integer rlist, ierr

      integer NCGM
C
C Default is to create an X workstation.
C
      NCGM=0
C
C Initialize the high level utility library
C
      call nhlfinitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. In this example the resource file supplies the
C plot title only.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfcreate(appid,'tm01',nhlfapplayerclass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation object.
C
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'wkMetaName','./tm01f.ncgm',ierr)
         call nhlfcreate(wid,'tm01Work',nhlfncgmworkstationlayerclass,0,
     $        rlist,ierr)
      else
C
C Create an XWorkstation object.
C
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'wkPause','True',ierr)
         call nhlfcreate(wid,'tm01Work',nhlfxworkstationlayerclass,0,
     1        rlist,ierr)
      endif
C
C Specify the viewport extent of the object.
C

      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'vpXF',.2,ierr)
      call nhlfrlsetfloat(rlist,'vpYF',.8,ierr)
      call nhlfrlsetfloat(rlist,'vpWidthF',.6,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',.6,ierr)

      call nhlfcreate(pid,'TickMarks',nhlftickmarklayerclass,wid,
     1      rlist,ierr)

      call nhlfdraw(pid,ierr)
      call nhlfframe(wid,ierr)
      call nhlfdestroy(pid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose

      stop
      end
