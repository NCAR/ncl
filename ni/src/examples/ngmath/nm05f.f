
C      $Id: nm05f.f,v 1.6 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:         nm05f.f
C
C  Author:       Mary Haley (taken from one of Fred Clare's examples)
C                National Center for Atmospheric Research
C                PO 3000, Boulder, Colorado
C
C  Date:         Mon Dec 22 13:53:37 MST 1997
C
C  Description: How to vary the exponent of the distances in a
C               2D interpolation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass

      parameter(NUM=6,NX=61,NY=61)

      real xi(NUM), yi(NUM), zi(NUM)
      real xo(NX), yo(NY), output(NX,NY)
      real xinc, yinc
C
C  Input data points and values.
C
      data xi/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/
      data yi/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/
      data zi/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/
      data rho, theta, phi/3., -45., 55./
      integer ier
      integer appid,wid,gkswid
      integer srlist, grlist
      integer i
      character*7  wks_type
C
C Default is to display output to an NCGM workstation.
C
      wks_type = "ncgm"
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
      call NhlFCreate(appid,'nm05',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm05f.ncgm',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm05f.ps',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./nm05f.pdf',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm05f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm05f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif

C
C  Specify the output grid.
C
      xinc = 1./real(nx-1)
      yinc = 1./real(ny-1)
      do 30 i=1,nx
         xo(i) = real((i-1)*xinc)
 30   continue
      do 40 j=1,ny
         yo(j) = real((j-1)*yinc)
 40   continue
C
C  Exponent equals 0.5
C
      call dssetr('EXP',0.5)
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ier)
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
      call tdez2d(NX,NY,xo,yo,output,rho,theta,phi,6)
      call gdawk (gkswid)
      call NhlFFrame(wid,ierr)
C
C  Exponent equals 1.0
C
      call dssetr('EXP',1.0)
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ier)
      call gacwk (gkswid)
      call tdez2d(NX,NY,xo,yo,output,rho,theta,phi,6)
      call gdawk (gkswid)
      call NhlFFrame(wid,ierr)
C
C  Exponent equals 5.0
C
      call dssetr('EXP',5.0)
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ier)
      call gacwk (gkswid)
      call tdez2d(NX,NY,xo,yo,output,rho,theta,phi,6)
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
