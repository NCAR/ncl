C
C      $Id: nm04f.f,v 1.8 2010-03-15 22:49:24 haley Exp $
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
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass

      parameter(NUM=1000,NX=21,NY=21,NZ=21)

      real xi(NUM), yi(NUM), zi(NUM), u(NUM)

      real xo(NX), yo(NY), zo(NZ), output(NX,NY,NZ)
      real xmin,ymin,zmin,xmax,ymax,zmax
      data xmin,ymin,zmin,xmax,ymax,zmax / -2., -2., -2., 2., 2., 2./
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
      call NhlFCreate(appid,'nm04',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm04f.ncgm',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm04f.ps',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./nm04f.pdf',ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm04f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm04f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm04Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
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
C  Portable random number generator.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      SAVE    NEXTN
      DATA JSEED,IFRST/123456789,0/
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      DRNM1 = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
