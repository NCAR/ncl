C
C      $Id: nm06f.f,v 1.4 1998-06-23 22:53:13 fred Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm06f.f
C
C  Author:     Mary Haley (taken from one of Fred Clare's examples)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       Mon Dec 22 16:36:20 MST 1997
C
C  Description: Smoothing in a simple 2D interpolation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass

      parameter(NUM=171,NX=21,NY=21)

      real xi(NUM), yi(NUM), zi(NUM)
      real xo(NX), yo(NY), output(NX,NY),output2(NX,NY)
      data xminin,yminin,xmaxin,ymaxin/ -0.2, -0.2, 1.2, 1.2/
      data xminot,yminot,xmaxot,ymaxot/  0.0,  0.0, 1.0, 1.0/
      data rho, theta, phi/3., -54., 32./
      integer ierr
      integer appid,wid,gkswid
      integer srlist, grlist
      integer i
      integer NCGM, X11, PS
C
C Default is to display output to an NCGM workstation.
C
      NCGM=1
      X11=0
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
      call NhlFCreate(appid,'nm06',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm06f.ncgm',ierr)
         call NhlFCreate(wid,'nm06Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm06Work',NhlFXWorkstationClass,
     +        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm06f.ps',ierr)
         call NhlFCreate(wid,'nm06Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      endif
C
C  Create random data in two-space and define a function.
C  To get this to work on your system, you may have to insert
C  the correct random number generator for your compiler.
C
      do 10 i=1,num
         xi(i) = xminin+(xmaxin-xminin)*drnm6()
         yi(i) = yminin+(ymaxin-yminin)*drnm6()
         zi(i) = (xi(i)-0.25)**2 + (yi(i)-0.50)**2
 10   continue
C
C  Create the output grid.
C
      do 102 i=1,nx
        xo(i) = xminot+(real(i-1)/real(nx-1))*(xmaxot-xminot)
  102 continue
      do 103 j =1,ny
        yo(j)= yminot+(real(j-1)/real(ny-1))*(ymaxot-yminot)
  103 continue
C
C  Interpolate.
C
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ierr)
C
C Get Workstation ID.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkGksWorkId',gkswid,ierr)
      call NhlFGetValues(wid,grlist,ierr)
C
C  Interpolate using dspnt2s.
C
      do 115 i = 1,NX
         do 110 j = 1,NY
            call dspnt2s(NUM,xi,yi,zi,1,xo(i),yo(j),output2(i,j),ier)
 110     continue
 115  continue
C
C There's no HLU object for surface plots yet, so we need to call the
C LLUs to get a surface plot.
C
      call gacwk (gkswid)
      call tdez2d(nx, ny, xo, yo, output, rho, theta, phi, 6)
      call gdawk (gkswid)
      call NhlFFrame(wid,ier)

      call gacwk (gkswid)
      call tdez2d(nx, ny, xo, yo, output2, rho, theta, phi, 6)
      call gdawk (gkswid)
      call NhlFFrame(wid,ier)
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
      REAL FUNCTION DRNM6()
C
      DATA ISEED/1/
      SAVE ISEED
C
      ISEED = ISEED*1103515245 + 12345
      IT = IAND(ISHIFT(ISEED,-16),32767)
C
      DRNM6 = REAL(IT)/32767.
C
      RETURN
      END
