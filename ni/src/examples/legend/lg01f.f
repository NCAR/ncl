CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           lg01f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 13 18:31:18 MDT 1995
C
C      Description:    Demonstrates the Legend Object defaults.
C

      external nhlfapplayerclass
      external nhlflegendlayerclass
      external nhlfxworkstationlayerclass
        
      integer appid, wid, pid
      integer rlist, ierr

C
C Initialize the high level utility library
C
      call nhlfinitialize

C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory.
C In this example the resource file supplies the plot title only.
C
      call nhlfrlcreate(rlist,'SETRL')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid,'lg01',nhlfapplayerclass,0,rlist,ierr)

C
C Create an XWorkstation object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'wkPause',1,ierr)
      call nhlfcreate(wid,'lg01Work',nhlfxworkstationlayerclass,0,
     1    rlist,ierr)
C     
C Specify the viewport extent of the object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'vpXF',0.,ierr)
      call nhlfrlsetfloat(rlist,'vpYF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpWidthF',1.,ierr)
      call nhlfrlsetfloat(rlist,'vpHeightF',1.,ierr)
      call nhlfcreate(pid,'Legend',nhlflegendlayerclass,wid,rlist,
     1      ierr)

      call nhlfdraw(pid,ierr)
      call nhlfframe(wid,ierr)
      call nhlfdestroy(pid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose

      stop
      end
