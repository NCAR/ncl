c***********************************************************************
c Determine the latitudes and weights used by the Lin-Rood Model
c based on an email from "Shian-Jiann Lin" lin@dao.gsfc.nasa.gov
c His code was "dumbed down" to f77 for the gnu77 compiler.
c***********************************************************************
C NCLFORTSTART
C NCL:  lw = linrood_latwt(nlat) 
      subroutine   linrood(lat,weight,nlat)
      implicit     none
c                             INPUT
      integer      nlat
c                             OUTPUT [return like NCL 'gaus']
      double precision weight(nlat), lat(nlat)
C NCLEND
      integer          nl
      double precision dlat
c                             Calculate latitudes
      dlat   = 180.d0/(nlat-1.d0)
      lat(1) = -90.d0
      do nl=2,nlat-1
         lat(nl) = lat(nl-1) + dlat
      end do
      lat(nlat)  = 90.d0
c                             Calculate weights 
      call linroodwt (weight,nlat)

      return
      end
c ------------------------------------------------------
C NCLFORTSTART
C NCL:  lw = linroodwt(nlat) 
      subroutine linroodwt (weight,nlat)
      implicit none
      integer nlat
      double precision weight(nlat)  
C NCLEND
c                                LOCAL [AUTOMATIC ARRAYS]
      real             dp, dl
      double precision sine(nlat),cosp(nlat),sinp(nlat),cose(nlat)
      integer          nl

      call setrig (nlat, dp, dl, cosp, cose, sinp, sine)
c                             Compute gw to be used in physpkg
      do nl=2,nlat-1
         weight(nl) = sine(nl+1) - sine(nl)
      end do

      weight(   1) =  1. + sine(2)
      weight(nlat) =  1. - sine(nlat)

      return
      end
c -----------------------------------------
c c c subroutine setrig(im, jm, dp, dl, cosp, cose, sinp, sine)
      subroutine setrig(jm, dp, dl, cosp, cose, sinp, sine)
      implicit none
c                               INPUT
      integer jm
c                               OUTPUT
      double precision sine(jm),cosp(jm),sinp(jm),cose(jm)
c                               LOCAL
      real    dl, dp
      integer j, jm1
      double precision pi, ph5   

      jm1 = jm - 1
      pi  = 4.0d0 * atan(1.d0)
c c c dl  = (pi+pi)/dble(im)    ! not used: drop "im"
      dp  = pi/dble(jm1)

      do j=2,jm
         ph5     = -0.5d0*pi + (dble(j-1)-0.5d0)*(pi/dble(jm1))
         sine(j) = sin(ph5)
      enddo

      cosp( 1) =  0.d0
      cosp(jm) =  0.d0

      do j=2,jm1
         cosp(j) = (sine(j+1)-sine(j)) / dp
      enddo

c Define cosine at edges..

      do j=2,jm
         cose(j) = 0.5d0 * (cosp(j-1) + cosp(j))
      enddo

      cose(1)  = cose(2)

      sinp( 1) = -1.d0
      sinp(jm) =  1.d0

      do j=2,jm1
         sinp(j) = 0.5d0 * (sine(j) + sine(j+1))
      enddo

      return
      end
