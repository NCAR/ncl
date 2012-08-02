C
C      $Id: nm03f.f,v 1.7 2010-03-15 22:49:24 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm03f.c
C
C  Author:     Mary Haley (taken from one of Fred Clare's examples)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Dec 22 11:49:46 MST 1997
C
C  Description: How to compute aspects and slopes.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFScalarFieldClass
      external NhlFVectorFieldClass
      external NhlFContourPlotClass
      external NhlFVectorPlotClass

      parameter(NumIn=171,NumXOut=21,NumYOut=21)
      parameter(IDIM=2*NumXOut*NumYOut)
      parameter(RAD2DEG = 57.29578)


      real  xo(NumXOut), yo(NumYOut), zo(NumXOut,NumYOut)
      real rtmp(NumXOut,NumYOut),u(NumXOut,NumYOut),v(NumXOut,NumYOut)
      real x(NumIn),y(NumIn),z(NumIn)

      data x/
     +    1.16,  0.47,  0.29,  0.72,  0.52,  1.12,  0.33,  0.20,  0.30,
     +    0.78,  0.92,  0.52,  0.44,  0.22, -0.10,  0.11,  0.59,  1.13,
     +    0.68,  1.11,  0.93,  0.29,  0.74,  0.43,  0.87,  0.87, -0.10,
     +    0.26,  0.85,  0.00, -0.02,  1.01, -0.12,  0.65,  0.39,  0.96,
     +    0.39,  0.38,  0.94, -0.03, -0.17,  0.00,  0.03,  0.67, -0.06,
     +    0.82, -0.03,  1.08,  0.37,  1.02, -0.11, -0.13,  1.03,  0.61,
     +    0.26,  0.18,  0.62,  0.42,  1.03,  0.72,  0.97,  0.08,  1.18,
     +    0.00,  0.69,  0.10,  0.80,  0.06,  0.82,  0.20,  0.46,  0.37,
     +    1.16,  0.93,  1.09,  0.96,  1.00,  0.80,  0.01,  0.12,  1.01,
     +    0.48,  0.79,  0.04,  0.42,  0.48, -0.18,  1.16,  0.85,  0.97,
     +    0.14,  0.40,  0.78,  1.12,  1.19,  0.68,  0.65,  0.41,  0.90,
     +    0.84, -0.11, -0.01, -0.02, -0.10,  1.04,  0.58,  0.61,  0.12,
     +   -0.02, -0.03,  0.27,  1.17,  1.02,  0.16, -0.17,  1.03,  0.13,
     +    0.04, -0.03,  0.15,  0.00, -0.01,  0.91,  1.20,  0.54, -0.14,
     +    1.03,  0.93,  0.42,  0.36, -0.10,  0.57,  0.22,  0.74,  1.15,
     +    0.40,  0.82,  0.96,  1.09,  0.42,  1.13,  0.24,  0.51,  0.60,
     +    0.06,  0.38,  0.15,  0.59,  0.76,  1.16,  0.02,  0.86,  1.14,
     +    0.37,  0.38,  0.26,  0.26,  0.07,  0.87,  0.90,  0.83,  0.09,
     +    0.03,  0.56, -0.19,  0.51,  1.07, -0.13,  0.99,  0.84,  0.22/
      data y/
     +   -0.11,  1.07,  1.11, -0.17,  0.08,  0.09,  0.91,  0.17, -0.02,
     +    0.83,  1.08,  0.87,  0.46,  0.66,  0.50, -0.14,  0.78,  1.08,
     +    0.65,  0.00,  1.03,  0.06,  0.69, -0.16,  0.02,  0.59,  0.19,
     +    0.54,  0.68,  0.95,  0.30,  0.77,  0.94,  0.76,  0.56,  0.12,
     +    0.05, -0.07,  1.01,  0.61,  1.04, -0.07,  0.46,  1.07,  0.87,
     +    0.11,  0.63,  0.06,  0.53,  0.95,  0.78,  0.48,  0.45,  0.77,
     +    0.78,  0.29,  0.38,  0.85, -0.10,  1.17,  0.35,  1.14, -0.04,
     +    0.34, -0.18,  0.78,  0.17,  0.63,  0.88, -0.12,  0.58, -0.12,
     +    1.00,  0.99,  0.45,  0.86, -0.15,  0.97,  0.99,  0.90,  0.42,
     +    0.61,  0.74,  0.41,  0.44,  1.08,  1.06,  1.18,  0.89,  0.74,
     +    0.74, -0.06,  0.00,  0.99,  0.03,  1.00, -0.04,  0.24,  0.65,
     +    0.12,  0.13, -0.09, -0.05,  1.03,  1.07, -0.02,  1.18,  0.19,
     +    0.03, -0.03,  0.86,  1.12,  0.38,  0.72, -0.20, -0.08, -0.18,
     +    0.32,  0.13, -0.19,  0.93,  0.81,  0.31,  1.09, -0.03,  1.01,
     +   -0.17,  0.84, -0.11,  0.45,  0.18,  0.23,  0.81,  0.39,  1.09,
     +   -0.05,  0.58,  0.53,  0.96,  0.43,  0.48,  0.96, -0.03,  1.13,
     +    1.16,  0.16,  1.15,  0.57,  0.13,  0.71,  0.35,  1.04,  0.62,
     +    1.03,  0.98,  0.31,  0.70,  0.97,  0.87,  1.14,  0.08,  1.19,
     +    0.88,  1.00,  0.51,  0.03,  0.17,  1.01,  0.44,  0.17, -0.11/
C

      dimension iwork(IDIM)
      real xc, yc
      integer ierr
      integer appid,wid,gkswid
      integer srlist, grlist
      integer i,len_dims(2)
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
C
      call NhlFRLCreate(srlist,'setrl')
      call NhlFRLCreate(grlist,'getrl')

      call NhlFRLClear(srlist)
      call NhlFRLSetString(srlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'nm03',NhlFAppClass,0,srlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm03f.ncgm',ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X11 workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,srlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm03f.ps',ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPDFFileName','./nm03f.pdf',ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm03f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFCairoPSPDFWorkstationClass,0,srlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkFileName','./nm03f',ierr)
         call NhlFRLSetString(srlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'nm03Work',
     +        NhlFCairoImageWorkstationClass,0,srlist,ierr)
      endif
C
C  Define the function values.
C
      do 5 i=1,NumIn
        z(i) = (x(i)-0.25)**2 + (y(i)-0.50)**2
    5 continue
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

      call nnseti('SDI - compute slopes and aspects',1)
      call nnseti('IGR - use gradient estimates',1)
      call nnseti('RAD - return results in radians',1)
      call natgrids(NumIn, x, y, z, NumXOut, NumYOut, xo, yo, zo, ierr)
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
      call drwsrf(gkswid,NumXOut,NumYOut,xo,yo,zo,10.,-25.,50.,iwork)
      call gdawk (gkswid)
C
C  Get the aspects.
C
      do 60 j=1,NumYOut
        do 65 i=1,NumXOut
          call nngetaspects(i,j,rtmp(i,j),ierr)
   65   continue
   60 continue

      do 100 i=1,NumXOut
        do 101 j=1,NumYOut
          u(i,j) = sin(rtmp(i,j))
          v(i,j) = cos(rtmp(i,j))
  101   continue
  100 continue
C
C Create a VectorField object; then use its id as the value of
C the 'vcVectorFieldData' resource when creating the VectorPlot object.
C
      len_dims(1) = NumXOut
      len_dims(2) = NumYOut

      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'vfUDataArray',u,2,len_dims,
     +     ierr)
      call NhlFRLSetMDFloatArray(srlist,'vfVDataArray',v,2,len_dims,
     +     ierr)
      call NhlFCreate(vfid,'vectorfield',NhlFVectorFieldClass,appid,
     +     srlist,ierr)

      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'vcVectorFieldData',vfid,ierr)
      call NhlFCreate(vcid,'VectorPlot',NhlFVectorPlotClass,wid,srlist,
     +     ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)
C
C  Get the slopes; convert to degrees.
C
      do 70 i=1,NumXOut
        do 75 j=1,NumYOut
          call nngetslopes(i,j,rtmp(i,j),ierr)
          rtmp(i,j) = RAD2DEG*rtmp(i,j)
   75   continue
   70 continue
C
C Create a ScalarField data object using the data set defined above.
C
      len_dims(1) = NumXOut
      len_dims(2) = NumYOut
      call NhlFRLClear(srlist)
      call NhlFRLSetMDFloatArray(srlist,'sfDataArray',rtmp,2,len_dims,
     +     ierr)
      call NhlFCreate(dataid,'data',NhlFScalarFieldClass,appid,srlist,
     +     ierr)
      call NhlFRLClear(srlist)
      call NhlFRLSetInteger(srlist,'cnScalarFieldData',dataid,ierr)
      call NhlFCreate(cnid,'ContourPlot',NhlFContourPlotClass,wid,
     +     srlist,ierr)

      call NhlFDraw(cnid,ierr)
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
      call gscr(wkid, 2, 0.00, 1.00, 1.00)
      call gscr(wkid, 3, 0.00, 1.00, 0.00)
      call gscr(wkid, 4, 0.70, 1.00, 0.00)
      call gscr(wkid, 5, 1.00, 1.00, 0.00)
      call gscr(wkid, 6, 1.00, 0.75, 0.00)
      call gscr(wkid, 7, 1.00, 0.50, 0.50)
      call gscr(wkid, 8, 1.00, 0.00, 0.00)

      call srface (x,y,z,iwk,nx,nx,ny,s,0.)

      return
      end
