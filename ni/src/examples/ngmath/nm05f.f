
C      $Id: nm05f.f,v 1.1 1997-12-23 16:01:09 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1997                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       nm05f.c
C
C  Author:     Mary Haley (taken from one of Fred Clare's examples)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Dec 22 13:53:37 MST 1997
C
C  Description: How to vary the exponent of the distances in a
C               2D interpolation.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFXWorkstationClass

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
      data xeye, yeye, zeye/3.3, -3.3, 3.3/
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
      call NhlFCreate(appid,'nm05',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkMetaName','./nm05f.ncgm',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFNcgmWorkstationClass,0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'nm05Work',NhlFXWorkstationClass,
     +        0,srlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetString(srlist,'wkPSFileName','./nm05f.ps',ierr)
         call NhlFCreate(wid,'nm05Work',
     +        NhlFPSWorkstationClass,0,srlist,ierr)
      endif

C
C  Specify the output grid.
C
      xinc = 1./real(nx-1)
      yinc = 1./real(ny-1)
      do 30 i=1,nx
         xo(i) = real((i-1)*xinc)
         do 40 j=1,ny
            yo(j) = real((j-1)*yinc)
 40      continue
 30   continue
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
      call drwtd2(gkswid,NX,NY,xo,yo,output,xeye,yeye,zeye,-6)
C
C  Exponent equals 1.0
C
      call dssetr('EXP',1.0)
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ier)
      call drwtd2(gkswid,NX,NY,xo,yo,output,xeye,yeye,zeye,-6)
C
C  Exponent equals 5.0
C
      call dssetr('EXP',5.0)
      call dsgrid2s(num, xi, yi, zi, nx, ny, xo, yo, output, ier)
      call drwtd2(gkswid,NX,NY,xo,yo,output,xeye,yeye,zeye,-6)
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

      subroutine drwtd2(wkid,nx,ny,x,y,z,s1,s2,s3,ist)
C
C  Procedure DRWTD2 uses the NCAR Graphics functions in Tdpack
C  to draw a surface plot of the data values in Z.
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
C             If IST is positive, then a white backgound is used;
C             if IST is the negative of any of the above values, then
C             a black background is used.
C            
      parameter (ierrf=6, lunit=2, iwkid=1, iwtype=1)
      parameter (mtri=40000)
      integer wkid
      dimension x(nx),y(ny),z(nx,ny)
      dimension rtri(10,mtri),rtwk(mtri,2),itwk(mtri)
      data ang1,ang2,rmul / -35.,25.,2.9 /
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
        ymax = max(ymax,y(I))
  125 continue
      zmin = z(1,1)
      zmax = z(1,1)
      do 130 i=1,nx
        do 140 j=1,ny
          zmin = min(zmin,z(i,j))
          zmax = max(zmax,z(i,j))
  140   continue
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
        call gscr (wkid,icol    ,     p,     p,     p)  !  gray scale
        call gscr (wkid,icol+ 32,     q,     q,     q)  !  white
        call gscr (wkid,icol+ 64,     q,shde*q,shde*q)  !  red
        call gscr (wkid,icol+ 96,shde*q,     q,shde*q)  !  green
        call gscr (wkid,icol+128,shde*q,shde*q,     q)  !  blue
        call gscr (wkid,icol+160,shde*q,     q,     q)  !  cyan
        call gscr (wkid,icol+192,     q,shde*q,     q)  !  magenta
  101 continue
C
C Define tdpack rendering styles 0 through 7.  The indices 0-7 can 
C then be used as final arguments in calls to TDITRI, TDSTRI, and TDMTRI.
C
      xsl = 0.05*xrng
      ysl = 0.05*yrng
      zsl = 0.00*zrng
      call tdstrs (1,-1, 0, -1,  0, -1, 1, 0, xsl, ysl, zsl)
      call tdstrs (2,43,74, 43, 74, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (3,43,74, 75,106, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (4,43,74,107,138, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (5,43,74,139,170, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (6,43,74,171,202, 1, 1, 0, xsl, ysl, zsl)
      call tdstrs (7,43,74,203,234, 1, 1, 0, xsl, ysl, zsl)
C
C Create the triangle list representing a surface.
C
      ntri=0
      call tdstri (x,nx,y,ny,z,nx,rtri,mtri,ntri,ist)
      if (ntri .eq. mtri) then
        print * , 'Triangle list overflow in TDITRI'
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
        yeye = s2
        zeye = s3
      endif
C
C Initialize TDPACK.
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
      call frame
      return
      end
