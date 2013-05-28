C NCLFORTSTART
      subroutine spareapolyi(vlat,vlon,nv,rad,area)
      implicit none
c input
      integer nv
      double precision  vlat(nv), vlon(nv), rad
c output
      double precision  area
C NCLEND
c local 
      integer npts

c Are beginning and end pts the same?
c If so, pass one less value. spareapoly wants no duplicate pts.

      npts = nv
      if (vlat(1).eq.vlat(nv) .and. vlon(1).eq.vlon(nv)) then
          npts = nv-1
      end if
      call spareapoly(vlat,vlon,npts,rad,area)

      return
      end
      
c --------------
      subroutine spareapoly(vlat,vlon,nv,rad,area)
      implicit none
c input
      integer nv
      double precision  vlat(nv), vlon(nv), rad
c output
      double precision  area

C*************************************************************
C Computing the Area of a Spherical Polygon of Arbitrary Shape
C     Bevis and Cambareri (1987)
C     Mathematical Geology, vol.19, Issue 4, pp 335-346 
C*************************************************************
c Computes the area of a spherical polygon with nv vertices and sides.

c ARGUMENTS:
c vlat,vlon ...  vectors containing the latitude and longitude
c                of each vertex.  The ith.  vertex is located at 
c                [vlat(i),vlon(i)].
c nv        ...  the number of vertices and sides in the spherical
c                polygon
c rad       ...  the radius of the sphere

c area      ...  the area of the spherical polygon

c UNITS:
c   Latitudes and longitudes are specified in degrees.  The user selects
c   the units of length in which to specify the radius, and the polygon
c   area will be returned in the square of these units.
c SIGN CONVENTION:
c   Latitudes are positive to the north and negative to the south.
c   Longitudes are positive to the east and negative to the west.
c VERTEX ENUMERATION: 
c   The vertices are numbered sequentially around the border of the
c   spherical polygon.  Vertex 1 lies between vertex nv and vertex 2.
c   The user must follow the convention whereby in moving around the
c   polygon border in the direction of increasing vertex number clockwise
c   bends occur at salient vertices.  A vertex is salient if the interior
c   angle'is less than 180 degrees.  (In the case of a convex polygon
c   this convention implies that the vertices are numbered in clockwise
c   sequence).
c
c   Two adjacent vertices may never be exactly 180 degrees apart
c   because they could be connected by infinitely many different
c   great circle arcs, and thus the border of the spherical
c   polygon would not be uniquely defined.  


c local
      integer iv
      double precision  pi, twopi, suma
      double precision  flat, flon, blat, blon, fang, bang, fvb
      parameter (pi=3.141592654d0, twopi=2.0d0*pi)

      suma = 0.0d0
      do iv=1,nv
         if (iv.eq.1) then
             flat=vlat(2)
             flon=vlon(2)
             blat=vlat(nv)
             blon=vlon(nv)
         elseif (iv.lt.nv) then
             flat=vlat(iv+1)
             flon=vlon(iv+1)
             blat=vlat(iv-1)
             blon=vlon(iv-1)
         else
             flat=vlat(1)
             flon=vlon(1)
             blat=vlat(nv-1)
             blon=vlon(nv-1)
         end if 
      
         call trnsfrmlon (vlat(iv),vlon(iv),flat,flon,fang)
         call trnsfrmlon (vlat(iv),vlon(iv),blat,blon,bang) 

         fvb = bang-fang
         if (fvb.lt. 0.0d0) fvb = fvb+twopi
         suma = suma+fvb
      end do

      area = (suma-pi*(nv-2))*rad**2

      return
      end
c --- 
      subroutine trnsfrmlon(plat,plon,qlat,qlon,tranlon)
      implicit none
c Finds the "longitude" of point Q in a geographic coordinate system
c  for which point P acts as a "north pole'.
c input
c   plat,plon,qlat,qlon ....  degrees
c output
c   tranlon ... radians
c input
      double precision plat,plon,qlat,qlon
c output
      double precision tranlon
c local
      double precision pi, dtr, tt, bb
      parameter (pi=3.141592654d0,dtr=pi/180.0d0)

      tt = sin((qlon-plon)*dtr)*cos(qlat*dtr)

      bb = sin(qlat*dtr)*cos(plat*dtr)-cos(qlat*dtr)*sin(plat*dtr)   
     &    *cos((qlon-plon)*dtr)

      tranlon = atan2(tt,bb)

      return
      end 
