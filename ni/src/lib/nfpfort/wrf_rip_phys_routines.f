c======================================================================
c
c !IROUTINE: WETBULBCALC -- Calculate wet bulb temperature (C)
c
c !DESCRIPTION:
c
c   Calculates wet bulb temperature in C, given pressure in 
c      temperature in K and mixing ratio in kg/kg.
c
c !INPUT:
c    nx     - index for x dimension
c    ny     - index for y dimension
c    nz     - index for z dimension
c    prs    - pressure (mb)
c    tmk    - temperature (K)
c    qvp    - water vapor mixing ratio (kg/kg)
c
c !OUTPUT:
c    twb    - Wet bulb temperature (C)
c
c !ASSUMPTIONS:
c
c !REVISION HISTORY:
c     2009-March  - Mark T. Stoelinga - from RIP4.5
c     2010-August - J. Schramm - modified to run with NCL and ARW wrf output
c
c !INTERFACE:
c ------------------------------------------------------------------
C NCLFORTSTART
      subroutine wetbulbcalc(prs,tmk,qvp,twb,nx,ny,nz)
      implicit none
      integer nx, ny, nz
      double precision prs(nx,ny,nz)
      double precision tmk(nx,ny,nz)
      double precision qvp(nx,ny,nz)
      double precision twb(nx,ny,nz)
C NCLEND
      integer i,j,k
      double precision q, t, p, e, tlcl, eth
      double precision PSADITHTE(150),PSADIPRS(150),PSADITMK(150,150)
      double precision tonpsadiabat
      include 'constants.h'
c
c  Before looping, set lookup table for getting temperature on
c  a pseudoadiabat.
c
      CALL DLOOKUP_TABLE(PSADITHTE,PSADIPRS,PSADITMK,
     &                   "./psadilookup.dat")

      DO k=1,nz
      DO j=1,ny
      DO i=1,nx
            q=dmax1(qvp(i,j,k),1.d-15)
            t=tmk(i,j,k)
            p=prs(i,j,k)
            e=q*p/(eps+q)
            tlcl=tlclc1/(dlog(t**tlclc2/e)-tlclc3)+tlclc4
            eth=t*(1000./p)**(gamma*(1.+gammamd*q))*
     &         exp((thtecon1/tlcl-thtecon2)*q*(1.+thtecon3*q))
            twb(i,j,k)=tonpsadiabat(eth,p,psadithte,psadiprs,
     &                              psaditmk,gamma)-celkel
      ENDDO
      ENDDO
      ENDDO
c
      return
      end
c======================================================================
c
c !IROUTINE: omgcalc -- Calculate omega (dp/dt)
c
c !DESCRIPTION:
c
c   Calculate approximate omega, based on vertical velocity w (dz/dt).
c   It is approximate because it cannot take into account the vertical
c   motion of pressure surfaces.
c
c !INPUT:
c    mx - index for x dimension
c    my - index for y dimension
c    mx -  index for vertical dimension
c    qvp - water vapor mixing ratio (kg/kg)
c    tmk - temperature (K)
c    www - vertical velocity (m/s)
c    prs -  pressure (Pa)
c
c !OUTPUT:
c    omg - omega (Pa/sec)
c
c !ASSUMPTIONS:
c
c !REVISION HISTORY:
c     2009-March  - Mark T. Stoelinga - from RIP4.5
c     2010-August - J. Schramm - modified to run with NCL and ARW wrf output
c
c ------------------------------------------------------------------
c NCLFORTSTART
      subroutine omgcalc(qvp,tmk,www,prs,omg,mx,my,mz)
      implicit none
      integer mx, my, mz
      double precision qvp(mx,my,mz)
      double precision tmk(mx,my,mz)
      double precision www(mx,my,mz)
      double precision prs(mx,my,mz) 
      double precision omg(mx,my,mz)
c NCLEND
c Local variables
      integer i, j, k
      double precision VIRTUAL
c
c Constants
      include 'constants.h'

      do k=1,mz
      do j=1,my
      do i=1,mx
         omg(i,j,k)=-grav*prs(i,j,k)/
     &      (rgas*virtual(tmk(i,j,k),qvp(i,j,k)))*www(i,j,k)
      enddo
      enddo
      enddo
c
      return
      end
c======================================================================
c
c !IROUTINE: VIRTUAL_TEMP -- Calculate virtual temperature (K)
c
c !DESCRIPTION:
c
c   Calculates virtual temperature in K, given temperature
c      in K and mixing ratio in kg/kg.
c
c !INPUT:
c    NX     - index for x dimension
c    NY     - index for y dimension
c    NZ     - index for z dimension
c    RATMIX - water vapor mixing ratio (kg/kg)
c    TEMP   - temperature (K)
c
c !OUTPUT:
c    TV     - Virtual temperature (K)
c
c !ASSUMPTIONS:
c
c !REVISION HISTORY:
c     2009-March  - Mark T. Stoelinga - from RIP4.5
c     2010-August - J. Schramm - modified to run with NCL and ARW wrf output
c
c ------------------------------------------------------------------
C NCLFORTSTART
      SUBROUTINE VIRTUAL_TEMP(TEMP,RATMIX,TV,NX,NY,NZ)
      IMPLICIT NONE
      INTEGER NX,NY,NZ
      DOUBLE PRECISION TEMP(NX,NY,NZ)
      DOUBLE PRECISION RATMIX(NX,NY,NZ)
      DOUBLE PRECISION TV(NX,NY,NZ)
C NCLEND
      INTEGER I,J,K
      DOUBLE PRECISION EPS
      EPS = 0.622D0
      DO K=1,NZ
      DO J=1,NY
      DO I=1,NX
         TV(I,J,K) = TEMP(I,J,K)* (EPS+RATMIX(I,J,K))/ 
     &                     (EPS* (1.D0+RATMIX(I,J,K)))
      ENDDO
      ENDDO
      ENDDO
      RETURN
      END
