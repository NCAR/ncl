CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tm02f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 MDT 1995
C
C      Description:     Demonstrates the TickMark Object with a
C                       number of resources set from the resource file
C
      external NhlFAppClass
      external NhlFTickMarkClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
C       
      integer appid, wid, pid
      integer rlist, ierr
      integer NCGM, X11, PS
C
C Default is to create an X workstation.
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
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tm02',NhlFAppClass,0,rlist,ierr)
C
      if (NCGM.eq.1) then
C
C Create an NCGM workstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./tm02f.ncgm',ierr)
         call NhlFCreate(wid,'tm02Work',NhlFNcgmWorkstationClass,0,
     $        rlist,ierr)
      else if (X11.eq.1) then
C
C Create an XWorkstation object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPause','True',ierr)
      call NhlFCreate(wid,'tm02Work',NhlFXWorkstationClass,0,
     1      rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./tm02f.ps',ierr)
         call NhlFCreate(wid,'tm02Work',NhlFPSWorkstationClass,0,
     $        rlist,ierr)
      endif
C
C Specify the viewport extent of the object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'vpXF',.2,ierr)
      call NhlFRLSetfloat(rlist,'vpYF',.8,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',.6,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',.6,ierr)
C
      call NhlFCreate(pid,'TickMarks',NhlFTickMarkClass,wid,
     1      rlist,ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
C
      stop
      end
