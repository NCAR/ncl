C
C      $Id: nm04f.f,v 1.4 1998-06-23 22:53:01 fred Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm04f.f
C
C  Author:     Mary Haley (taken from one of Fred Clare's examples)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Dec 22 13:53:37 MST 1997
C
C  Description: Simple 3D interpolation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass

      parameter(NUM=1000,NX=21,NY=21,NZ=21)

      real xi(NUM), yi(NUM), zi(NUM), u(NUM)

      real xo(NX), yo(NY), zo(NZ), output(NX,NY,NZ)
      real xmin,ymin,zmin,xmax,ymax,zmax
      data xmin,ymin,zmin,xmax,ymax,zmax / -2., -2., -2., 2., 2., 2./
      integer ier
      integer appid,wid,gkswid
      integer srlist, grlist
      integer i
      integer NCGM, X11, PS
C
C Default is to display output to an NCGM workstation.
C
      NCGM=1
      X11=1
      PS=0
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLCreate(grlist,'getrl')

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'nm04',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm04f.ncgm',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm04Work',NhlFXWorkstationClass,
     +        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm04f.ps',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      endif

      do 10 i=1,NUM
         xi(i) = xmin+(xmax-xmin)*drnm1()
         yi(i) = ymin+(ymax-ymin)*drnm1()
         zi(i) = zmin+(zmax-zmin)*drnm1()
         u(i) = xi(i)**2 + yi(i)**2 + zi(i)**2
 10   continue
C
C  Create the output grid.
C
      do 102 i=1,NX
         xo(i) = xmin+(real(i-1)/real(NX-1))*(xmax-xmin)
 102  continue

      do 103 j =1,NY
         yo(j)= ymin+(real(j-1)/real(NY-1))*(ymax-ymin)
 103  continue

      do 104 k=1,NZ
        zo(k) = zmin+(real(k-1)/real(NZ-1))*(zmax-zmin)
  104 continue
C
C  Interpolate.
C
      call dsgrid3s(num,xi,yi,zi,u,NX,NY,NZ,xo,yo,zo,output,ier)
C
C Get Workstation ID.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C
C There's no HLU object for surface plots yet, so we need to call the
C LLUs to get a surface plot.
C
      call gacwk (gkswid)
      call tdez3d(NX,NY,NZ,xo,yo,zo,output,3.0,2.,-35.,65.,6)
      call gdawk (gkswid)
      call NhlFFrame(wid,ierr)
C
C NhlDestroy destroys the given id and all of its children.
C
      call NhlFDestroy(wid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end
      REAL FUNCTION DRNM1()
C
      DATA ISEED/1/
      SAVE ISEED
C
      ISEED = ISEED*1103515245 + 12345
      IT = IAND(ISHIFT(ISEED,-16),32767)
C
      DRNM1 = REAL(IT)/32767.
C
      RETURN
      END
