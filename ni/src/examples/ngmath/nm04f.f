C
C      $Id: nm04f.f,v 1.2 1997-12-23 17:25:45 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm04f.c
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
         xi(i) = xmin+(xmax-xmin)*dsrand()
         yi(i) = ymin+(ymax-ymin)*dsrand()
         zi(i) = zmin+(zmax-zmin)*dsrand()
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
      call drwtd3(gkswid,NX,NY,NZ,xo,yo,zo,output,3.0,0.,0.,0.,-6)
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

      subroutine drwtd3(wkid,nx,ny,nz,x,y,z,u,value,s1,s2,s3,ist)
C
C  Procedure DRWTD3 uses the NCAR Graphics functions in Tdpack
C  to draw an isosurface plot of the data values in U.
C
C  The point of observation is specified by (s1, s2, s3); the point
C  looked at is the center of the surface.  If s1 = s2 = s3 = 0.,
C  then the observation point is calculated automatically.
C
C   NX     -  Dimension of the X-axis variable X.
C   NY     -  Dimension of the Y-axis variable Y.
C   NZ     -  Dimension of the Z-axis variable Z.
C   X      -  An array of X-axis values.
C   Y      -  An array of Y-axis values.
C   Z      -  An array of Z-axis values.
C   U      -  An array dimensioned for NX x NY x NZ containing data
C             values for each (X,Y,Z) coordinate.
C   S1     -  X value for the eye position.
C   S2     -  Y value for the eye position.
C   S3     -  Z value for the eye position.
C   VALUE  -  The iso value.
C   IST    -  A style index defining the colors used to shade the
C             surface as per:
C
C                    1  -  wire frame
C                    2  -  gray shades underneath; gray shades on top.
C                    3  -  gray shades underneath; red shades on top.
C                    4  -  gray shades underneath; green shades on top.
C                    5  -  gray shades underneath; blue shades on top.
C                    6  -  gray shades underneath; cyan shades on top.
C                    7  -  gray shades underneath; magenta shades on top.
C
C             If IST is positive, then a white backgound is use;
C             if IST is the negative of any of the above values, then
C             a black background is used.
C            
      integer wkid
      dimension x(nx),y(ny),z(nz),u(nx,ny,nz)
      data ang1,ang2,rmul / -35.,25.,2.9 /
      parameter (mtri=40000)
      dimension rtri(10,mtri),rtwk(mtri,2),itwk(mtri)
      common /dstddt/ rtri,rtwk,itwk
C
C  Set the desired values of the shading parameters.  Values of SHDE
C  near 0 give brighter colors and values near 1 give pastel shades.
C  Values of SHDR near 0 give a narrow range of shades and values near
C  1 give a wide range of shades.
C
      data shde,shdr / .1 , 0.8 /
      data dtor / .017453292519943 /
      call gscr(wkid,0,0.,0.,0.)
      call gscr(wkid,1,1.,1.,1.)
C
C  Find mins and maxs.
C
      xmin = x(1)
      xmax = x(1)
      do 120 i=2,nx
        xmin = min(xmin,x(i))
        xmax = max(xmax,x(i))
  120 continue
      ymin = y(1)
      ymax = y(1)
      do 125 i=2,ny
        ymin = min(ymin,y(i))
        ymax = max(ymax,y(i))
  125 continue
      zmin = z(1)
      zmax = z(1)
      do 130 k=1,nz
        zmin = min(zmin,z(k))
        zmax = max(zmax,z(k))
  130 continue
      xrng = xmax-xmin
      yrng = ymax-ymin
      zrng = zmax-zmin
      xmid = 0.5*(xmin+xmax)
      ymid = 0.5*(ymin+ymax)
      zmid = 0.5*(zmin+zmax)

      call gscr (wkid,2,1.,0.,0.)
      call gscr (wkid,3,0.,1.,0.)
      call gscr (wkid,4,0.,0.,1.)
      call gscr (wkid,5,0.,1.,1.)
      call gscr (wkid,6,1.,0.,1.)
      call gscr (wkid,7,1.,1.,0.)
      call gscr (wkid,8,0.7,0.7,0.7)

      do 101 icol=11,42
        p=1.-     real(icol-11)/31.
        q=1.-shdr*real(icol-11)/31.
        call gscr (wkid,icol    ,     p,     p,     p) !  gray scale
        call gscr (wkid,icol+ 32,     q,     q,     q)  !  white
        call gscr (wkid,icol+ 64,     q,shde*q,shde*q)  !  red
        call gscr (wkid,icol+ 96,shde*q,     q,shde*q)  !  green
        call gscr (wkid,icol+128,shde*q,shde*q,     q)  !  blue
        call gscr (wkid,icol+160,shde*q,     q,     q)  !  cyan
        call gscr (wkid,icol+192,     q,shde*q,     q)  !  magenta
  101 continue
C
C Define tdpack rendering styles 0 through 7.  the indices 0-7 can 
C then be used as final arguments in calls to TDITRI, TDSTRI, and TDMTRI.
C
      xsl = 0.07*xrng
      ysl = 0.07*yrng
      zsl = 0.00*zrng
      call tdstrs (1,-1, 0, -1,  0, -1, 1, 0, xsl, ysl, zsl)
      call tdstrs (2,43,74, 43, 74, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (3,43,74, 75,106, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (4,43,74,107,138, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (5,43,74,139,170, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (6,43,74,171,202, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (7,43,74,203,234, 1, 1, 0, xsl, ysl, zsl)
C
C Create the triangle list representing an isosurface.
C
      ntri=0
      call tditri (x,nx,y,ny,z,nz,u,nx,ny,value,
     +             rtri,mtri,ntri,ist)
      if (ntri .eq. mtri) then
        print * , 'triangle list overflow in tditri'
        stop
      end if
C
C  Determine a default eye position if none is specified.
C
      if (s1.eq.0. .and. s2.eq.0. .and. s3.eq.0.) then
        r = rmul*sqrt(xrng*xrng + yrng*yrng + zrng*zrng)
        xeye = xmid+r*cos(dtor*ang1)*cos(dtor*ang2)
        yeye = ymid+r*sin(dtor*ang1)*cos(dtor*ang2)
        zeye = zmid+r*sin(dtor*ang2)
      else
        xeye = s1
        yeye = s1
        zeye = s1
      endif
C
C Initialize Tdpack.
C
      call tdinit (xeye, yeye, zeye, xmid, ymid, zmid,
     +                   xmid, ymid, zmid+0.1*zrng, 0)
C
C Order the triangles.
C
      call tdotri (rtri,mtri,ntri,rtwk,itwk,1)
      if (ntri .eq. mtri) then
        print * , 'triangle list overflow in tdotri'
        stop
      end if
C
C  Draw the triangles.
C
      call tddtri (rtri,mtri,ntri,itwk)
      return
      end

      real function dsrand()

      data iseed/1/
      save iseed

      iseed = iseed*1103515245 + 12345
      it = iand(ishift(iseed,-16),32767)

      dsrand = real(it)/32767.

      return
      end
