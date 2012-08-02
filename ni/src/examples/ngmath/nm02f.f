C
C      $Id: nm02f.f,v 1.6 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm02f.c
C
C  Author:     Mary Haley (taken from one of Fred Clare's examples)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Dec 22 11:46:43 MST 1997
C
C  Description: Simple example of natural neighbor linear interpolation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFscalarFieldClass
      external NhlFcontourPlotClass

      parameter(Npts=6,NumXOut=21,NumYOut=21)
      parameter(IDIM=2*NumXOut*NumYOut)


      real  xo(NumXOut), yo(NumYOut), zo(NumXOut,NumYOut)
      real x(Npts),y(Npts),z(Npts)
      data x/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/
      data y/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/
      data z/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/
      dimension iwork(IDIM)
      real xc, yc
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
      call NhlFCreate(appid,'nm02',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm02f.ncgm',ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm02f.ps',ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./nm02f.pdf',ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm02f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm02f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm01Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif

C
C  Define the output grid.
C
      xmin = 0.
      xmax = 1.
      xc = (xmax-xmin)/(NumXOut-1.) 
      do 20 i=1,NumXOut
        xo(i) = xmin+real(i-1) * xc
 20   continue

      ymin = 0.
      ymax = 1.
      yc = (ymax-ymin)/(NumYOut-1.) 
      do 30 i=1,NumYOut
        yo(i) = ymin+real(i-1) * yc
 30   continue

      call nnseti('IGR',1)
      call natgrids(Npts, x, y, z, NumXOut, NumYOut, xo, yo, zo, ier)
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
      call drwsrf (gkswid,NumXOut,NumYOut,xo,yo,zo,15.,-25.,90.,iwork)
      call gdawk (gkswid)
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

      subroutine drwsrf(wkid,nx,ny,x,y,z,s1,s2,s3,iwk)
C
C  Procedure DRWSRF uses the NCAR Graphics function SRFACE to
C  draw a surface plot of the data values in Z.
C 
C  The point of observation is calculated from the 3D coordinate
C  (S1, S2, S3); the point looked at is the center of the surface.
C 
C   NX     -  Dimension of the X-axis variable X.
C   NY     -  Dimension of the Y-axis variable Y.
C   X      -  An array of X-axis values.
C   Y      -  An array of Y-axis values.
C   Z      -  An array dimensioned for NX x NY containing data
C             values for each (X,Y) coordinate.
C   S1     -  X value for the eye position.
C   S2     -  Y value for the eye position.
C   S3     -  Z value for the eye position.
C   IWK    -  Work space dimensioned for at least 2*NX*NY.
C 
C  
      dimension x(nx),y(ny),z(nx,ny),iwk(*)
      integer wkid
      dimension S(6)
c
c  Find the extreme values.
c
      xmn =  x(1)
      xmx =  x(1)
      ymn =  y(1)
      ymx =  y(1)
      zmn =  z(1,1)
      zmx =  z(1,1)

      do 10 i=2,nx
        xmn = min(xmn,x(i))
        xmx = max(xmx,x(i))
   10 continue

      do 11 i=1,ny
        ymn = min(ymn,y(i))
        ymx = max(ymx,y(i))
   11 continue

      do 12 i=1,nx
        do 13 j=1,ny
          zmn = min(zmn,z(i,j))
          zmx = max(zmx,z(i,j))
   13   continue
   12 continue

      if (s1.eq.0. .and. s2.eq.0. .and. s3.eq.0.) then
        st1 = -3.
        st2 = -1.5
        st3 = 0.75
      else
        st1 = s1
        st2 = s2
        st3 = s3
      endif
      s(1) = 5.*st1*(xmx-xmn)
      s(2) = 5.*st2*(ymx-ymn)
      s(3) = 5.*st3*(zmx-zmn)
      s(4) = 0.5*(xmx-xmn)
      s(5) = 0.5*(ymx-ymn)
      s(6) = 0.5*(zmx-zmn)
C
C Set foreground/background colors
C
      call gscr(wkid, 0, 1.00, 1.00, 1.00)
      call gscr(wkid, 1, 0.00, 0.00, 0.00)

      call srface (x,y,z,iwk,nx,nx,ny,s,0.)

      return
      end
