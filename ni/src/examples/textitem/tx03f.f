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
C                      different colors (using the default colormap.)
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFTextItemClass

      integer appid, wid, pid
      integer srlist, grlist,ierr
      integer i
      integer NCGM, X11, PS
C
C Default is to create a metafile.
C
      NCGM=1
      X11=0
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
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tx03',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./tx03f.ncgm',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFNcgmWorkstationClass,0,
     1        srlist,ierr)
      else if (X11.eq.1) then
C
C Create an X Workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFXWorkstationClass,
     $        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./tx03f.ps',ierr)
         call NhlFCreate(wid,'tx03Work',NhlFPSWorkstationClass,0,
     1        srlist,ierr)
      endif
C
C Get the number of colors in the default color table.
C
      call NhlFRLCreate(grlist,'getrl')
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkColorMapLen',num_colors,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C
C
C Create a TextItem and then draw multiple frames varying the fill
C color of the text bounding box to all entries of the default
C workstation color map.
C
      call NhlFRLClear(srlist)
      call NhlFCreate(pid,'TextItems',NhlFTextItemClass,
     $        wid,srlist,ierr)
      do 10, i=1,num_colors
         call NhlFRLClear(srlist)
         call NhlFRLSetinteger(srlist,'txBackgroundFillColor',
     $        i,ierr)
         call NhlFSetValues(pid,srlist,ierr)
         call NhlFDraw(pid,ierr)
         call NhlFFrame(wid,ierr)
 10   continue
C
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose
C
      stop
      end
