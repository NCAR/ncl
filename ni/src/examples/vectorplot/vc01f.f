C
C      $Id: vc01f.f,v 1.1 1996-05-10 15:22:16 haley Exp $
C
C***********************************************************************
C                                                                      *
C                Copyright (C)  1996                                   *
C        University Corporation for Atmospheric Research               *
C                All Rights Reserved                                   *
C                                                                      *
C***********************************************************************
C
C   File:       vc01f.f
C 
C   Author:     David Brown
C               National Center for Atmospheric Research
C               PO 3000, Boulder, Colorado
C 
C
C  Date:       Wed Apr  3 17:00:55 MST 1996
C
C  Description:  Basic VectorPlot example
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass
      external NhlFVectorFieldClass
      external NhlFVectorPlotClass

      parameter(M=30,N=25)
      parameter(PI=3.14159)

      integer NCGM, X11, PS
      integer appid,wid,vcid,vfid
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
C directory so the application looks for a resource file in the working
C directory. 
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'vc01',NhlFappClass,0,rlist,ierr)


      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./vc01f.ncgm',ierr)
         call NhlFCreate(wid,'vc01Work',NhlFNcgmWorkstationClass,
     1     0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'vc01Work',NhlFXWorkstationClass,
     1        0,rlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./vc01f.ps',ierr)
         call NhlFCreate(wid,'vc01Work',NhlFPSWorkstationClass,
     1        0,rlist,ierr)
      endif
C
C Create a VectorField object then use its id as the value of
C the 'vcVectorFieldData' resource when creating the VectorPlot object.
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
     1     'Basic VectorPlot Example',ierr)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfid,ierr)
      call NhlFCreate(vcid,'vector',NhlFVectorPlotClass,wid,rlist,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
