C NCLFORTSTART
      SUBROUTINE DPOTMP(PRESS,TEMP,S,RP,POTEMP)
      IMPLICIT NONE
C
C     TITLE:
C     *****
C
C       POTMP  -- CALCULATE POTENTIAL TEMPERATURE FOR AN ARBITRARY
C                 REFERENCE PRESSURE
C
C     PURPOSE:
C     *******
C
C       TO CALCULATE POTENTIAL TEMPERATURE
C
C       REF: N.P. FOFONOFF
C            DEEP SEA RESEARCH
C            IN PRESS NOV 1976
C
C     PARAMETERS:
C     **********
C
C       PRESS  -> PRESSURE IN DECIBARS
C       TEMP   -> TEMPERATURE IN CELSIUS DEGREES
C       S      -> SALINITY PSS 78
C       RP     -> REFERENCE PRESSURE IN DECIBARS
C                 (0.0 FOR THE QUANTITY THETA)
C       POTEMP <- POTENTIAL TEMPERATURE (DEG C)
C
      DOUBLE PRECISION PRESS,TEMP,S,RP,POTEMP
C NCLEND
C
C     VARIABLES:
C     *********
C
         INTEGER I,J,N
         DOUBLE PRECISION DP,P,Q,R1,R2,R3,R4,R5,S1,T,X
C
C     CODE:
C     ****
C
      S1 = S-35.0D0
      P  = PRESS
      T  = TEMP
C
      DP = RP - P
C C C N  = IFIX(ABS(DP)/1000.0D0) + 1     ! original; IFIX does not allow dble
      N  = IDINT (ABS(DP)/1000.0D0) + 1
      DP = DP/N
C
      DO 10 I=1,N
         DO 20 J=1,4
C
            R1 = ((-2.1687D-16*T+1.8676D-14)*T-4.6206D-13)*P
            R2 = (2.7759D-12*T-1.1351D-10)*S1
            R3 = ((-5.4481D-14*T+8.733D-12)*T-6.7795D-10)*T
            R4 = (R1+(R2+R3+1.8741D-8))*P+(-4.2393D-8*T+1.8932D-6)*S1
            R5 = R4+((6.6228D-10*T-6.836D-8)*T+8.5258D-6)*T+3.5803D-5
C
            X  = DP*R5
C
            GO TO (100,200,300,400),J
C
  100       CONTINUE
            T = T+0.5D0*X
            Q = X
            P = P + 0.5D0*DP
            GO TO 20
C
  200       CONTINUE
            T = T + .29298322D0*(X-Q)
            Q = 0.58578644D0*X + 0.121320344D0*Q
            GO TO 20
C
  300       CONTINUE
            T = T + 1.707106781D0*(X-Q)
            Q = 3.414213562D0*X - 4.121320344D0*Q
            P = P + 0.5D0*DP
            GO TO 20
C
  400       CONTINUE
            T = T + (X-2.0D0*Q)/6.0D0
  20      CONTINUE
  10    CONTINUE
C
        POTEMP = T
        RETURN
C
C       END POTMP
C
        END
c ------------
C NCLFORTSTART
        subroutine dpth2pres(nd, depth, has_msg_depth, depth_msg, 
     +             pressure)
        implicit none
c  DESCRIPTION:
c  This function computes pressure in bars from depth in meters
c  using a mean density derived from depth-dependent global 
c  average temperatures and salinities from Levitus 1994, and 
c  integrating using hydrostatic balance.
c 
c  References:
c 
c  Levitus, S., R. Burgett, and T.P. Boyer, World Ocean Atlas 
c  Volume 3: Salinity, NOAA Atlas NESDIS 3, US Dept. of ! Commerce, 1994.
c 
c  Levitus, S. and T.P. Boyer, World Ocean Atlas 1994, ! Volume 4:
c  Temperature, NOAA Atlas NESDIS 4, US Dept. of ! Commerce, 1994.
c 
c  Dukowicz, J. K., 2000: Reduction of Pressure and Pressure
c  Gradient Errors in Ocean Simulations, J. Phys. Oceanogr.,
c  submitted.

c  INPUT PARAMETERS:
c  nd     - size of 
c  depth  - depth in meters. No units check is made

      integer  nd, has_msg_depth
      double precision depth(nd), depth_msg

c  OUTPUT PARAMETERS:
c  pressure - pressure in bars 

      double precision pressure(nd)
C NCLEND

c  LOCAL PARAMETERS:
      integer n
      double precision c1
      data c1 /1.0d0/
c -----------------------------------------------------------------------
c  convert depth in meters to pressure in bars
c -----------------------------------------------------------------------

      if (has_msg_depth.eq.1) then
         do n=1,nd
            if(depth(n).eq.depth_msg) then
               pressure(n) = depth_msg
            else
               pressure(n) = 0.059808d0*(exp(-0.025d0*depth(n)) - c1) +
     &                       0.100766d0*depth(n)+2.28405d-7*depth(n)**2
            end if
         end do
      else
         do n=1,nd
            pressure(n) = 0.059808d0*(exp(-0.025d0*depth(n)) - c1) +
     &                    0.100766d0*depth(n)+2.28405d-7*depth(n)**2
         end do
      end if

      return
      end
