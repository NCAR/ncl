C
C      $Id: st01f.f,v 1.1 1996-05-10 17:05:11 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C  File:       st01f.f
C
C  Author:     David Brown
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Fri May 10 10:52:47 MDT 1996
C
C  Description:  Basic StreamlinePlot example
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFVectorFieldClass
      external NhlFStreamlinePlotClass

      parameter(N=30,M=25)
      parameter(PI=3.14159)

      integer NCGM, X11, PS
      integer appid,wid,stid,vfid
      integer rlist
      integer len_dims(2)
      real U(M,N),V(M,N)
      real igrid, jgrid
      integer i,j

      NCGM=0
      X11=1
      PS=0
C
C Generate vector data arrays
C
      igrid = 2.0 * PI / real(M)
      jgrid = 2.0 * PI / real(N)
      do 20 j = 1,N
         do 10 i=1,M
            U(i,j) = 10.0 * cos(jgrid * real(j-1))
            V(i,j) = 10.0 * cos(igrid * real(i-1))
 10      continue
 20   continue
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'st01',NhlFappClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./st01f.ncgm',ierr)
         call NhlFCreate(wid,'st01Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'st01Work',NhlFXWorkstationClass,
     1        0,rlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./st01f.ps',ierr)
         call NhlFCreate(wid,'st01Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      endif
C
C Create a VectorField object; then use its id as the value of
C the 'stVectorFieldData' resource when creating the StreamlinePlot
C object.
C
      len_dims(1) = M
      len_dims(2) = N
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFVectorFieldClass,appid,
     1     rlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tiMainString',
     1     'Basic StreamlinePlot Example',ierr)
      call NhlFRLSetInteger(rlist,'stVectorFieldData',vfid,ierr)
      call NhlFCreate(stid,'streamline',NhlFStreamlinePlotClass,wid,rlist,
     1     ierr)

      call NhlFDraw(stid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose
      stop
      end
