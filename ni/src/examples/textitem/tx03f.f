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
C                      114 different colors. (The default colormap.)
C
      external nhlfapplayerclass
      external nhlfxworkstationlayerclass
      external nhlftextitemlayerclass

      integer appid, wid, pid
      integer rlist, ierr
      integer m,i

      parameter(m=114)
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
      call nhlfrlcreate(rlist,'setrl')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfrlsetstring(rlist,'appDefaultParent','True',ierr)
      call nhlfcreate(appid,'tx03',nhlfapplayerclass,0,rlist,ierr)
C
C Create an xworkstation object.
C
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'wkPause',true,ierr)
      call nhlfcreate(wid,'tx03Work',nhlfxworkstationlayerclass,
     $     0,rlist,ierr)
C
C Create 114 plots varying the fill color of the text bounding box
C to all entries of the default workstation color map.
C
      do 10, i=1,m
         call nhlfrlclear(rlist)
         call nhlfrlsetinteger(rlist,'txBackgroundFillColor',
     $        i,ierr)
         call nhlfcreate(pid,'TextItems',nhlftextitemlayerclass,
     $        wid,rlist,ierr)

         call nhlfdraw(pid,ierr)
         call nhlfframe(wid,ierr)
 10   continue

      call nhlfdestroy(pid,ierr)
      call nhlfdestroy(wid,ierr)
      call nhlfdestroy(appid,ierr)
      call nhlfclose

      stop
      end
