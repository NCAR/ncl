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
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFTextItemClass

      integer appid, wid, pid
      integer rlist, ierr
      integer m,i
      integer NCGM

      parameter(m=114)
C
C Default is to create a metafile.
C
      NCGM=1
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
      call NhlFCreate(appid,'tx03',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./tx03f.ncgm',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFXWorkstationClass,
     $        0,rlist,ierr)
      endif
C
C Create 114 plots varying the fill color of the text bounding box
C to all entries of the default workstation color map.
C
      do 10, i=1,m
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'txBackgroundFillColor',
     $        i,ierr)
         call NhlFCreate(pid,'TextItems',NhlFTextItemClass,
     $        wid,rlist,ierr)

         call NhlFDraw(pid,ierr)
         call NhlFFrame(wid,ierr)
 10   continue

      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
