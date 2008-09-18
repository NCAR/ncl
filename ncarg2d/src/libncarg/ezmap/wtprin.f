C
C $Id: wtprin.f,v 1.4 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WTPRIN (UVAL,VVAL,DLAT,DLON,CSLT)
C
        DOUBLE PRECISION UVAL,VVAL,DLAT,DLON,CSLT
C
C The subroutine WTPRIN implements the inverse of the Winkel tripel
C projection, using an iterative algorithm due to Cengizhan Ipbuker,
C of the Istanbul Technical University, in Istanbul, Turkey, and Oztug
C Bildirici, of Selcuk University, in Konya, Turkey.  Their paper is
C called "A General Algorithm for the Inverse Transformation of Map
C Projections Using Jacobian Matrices", and it was published in the
C proceedings of the Third International Symposium on Mathematical and
C Computational Applications, September 4-6, 2002 (pp. 175-182).
C
C UVAL and VVAL are input values of U and V, in the projection plane.
C DLAT and DLON are output values of latitude and longitude, in radians.
C CSLT is the cosine of the standard parallel of the Winkel tripel, for
C which Winkel used ACOS(2/PI); in EZMAP, though, it is allowed to vary.
C
C The common block MAPCM0 contains mathematical constants.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
C Local variables.
C
        DOUBLE PRECISION CVAL,DLAP,DLOP,DNOM,DVAL,PULA,PULO,PVLA, PVLO,
     +                   TVAL,UDIF,VDIF
C
C First, we must check the point (UVAL,VVAL) to see if it lies in the
C region occupied by the projection of the globe.  IOOR will be set to
C 0 if so, and to 1 otherwise.  ("OOR" stands for "Out Of Range".)
C
        IOOR=0
C
C The first condition in the following block-if expresses the condition
C that the point (UVAL,VVAL) is outside the bounding rectangle of the
C area occupied by the projection of the globe.  If not, and if it is
C not in the central rectangle, we do a more complicated test to see if
C it is inside or outside one of the curved end pieces of the area.
C
        IF (ABS(UVAL)-PIOT*(CSLT+1.D0).GT.1.D-9.OR.
     +      ABS(VVAL)-PIOT.GT.1.D-9) THEN
          IOOR=1
        ELSE IF (ABS(UVAL).GT.PIOT*CSLT) THEN
          DLAT=ACOS(ABS(UVAL)/PIOT-CSLT)
          IF (ABS(VVAL)-(.5D0*DLAT+PIOF*SIN(DLAT)).GT.1.D-9) IOOR=1
        END IF
C
C If the input point is out of range, return the value "1.D12" to
C signal the fact.
C
        IF (IOOR.NE.0) THEN
          DLAT=1.D12
          DLON=1.D12
          RETURN
        END IF
C
C Ipbuker and Bildirici's algorithm makes use of partial derivatives
C of the forward projection functions, which are usually expressed
C using the SINC function, which is carefully defined so as not to
C cause problems when its argument is zero.  To facilitate the taking
C of partial derivatives, the SINC function was replaced by a more
C conventional expression, and it may be that which causes problems
C for the algorithm along the equator.  In any case, the following
C code takes care of the problem:
C
        IF (ABS(VVAL).LT.1.D-12) THEN
          DLAT=VVAL
          DLON=2.D0*UVAL/(CSLT+1.D0)
          RETURN
        END IF
C
C Set up the iteration loop.  DLAP and DLOP hold values of latitude and
C longitude from the previous iteration; they are given initial values
C that represent a guess at the proper values.
C
        DLAP=.999D0*VVAL
        DLOP=DLAP*(CSLT+COS(DLAP))
C
C NITR keeps track of the number of iterations performed, which is
C limited to 20.  (Actually, my tests indicate that a maximum of 7
C iterations suffice to give full double-precision accuracy.)
C
        NITR=1
C
C Iteration loop.  First, compute terms that occur frequently.
C
  101   TVAL=COS(DLAP)*COS(.5D0*DLOP)
C
        CVAL=1.D0-TVAL**2
        DVAL=ACOS(TVAL)
C
C To follow this code, refer to Ipbuker and Bildirici's paper.  UDIF
C and VDIF are functions of latitude and longitude expressing the
C difference between the projected values at a lat/lon position and
C the desired values.  Each of PULA, PULO, PVLA, and PVLO is a partial
C derivative of either U or V with respect to either latitude or
C longitude.
C
        TVAL=DVAL/SQRT(CVAL)
C
        UDIF=.5D0*(2.D0*TVAL*COS(DLAP)*SIN(.5D0*DLOP)+DLOP*CSLT)-UVAL
        VDIF=.5D0*(TVAL*SIN(DLAP)+DLAP)-VVAL
C
        TVAL=TVAL/CVAL
C
        PULA=SIN(DLOP)*SIN(2.D0*DLAP)/(4.D0*CVAL)-
     +       TVAL*SIN(DLAP)*SIN(.5D0*DLOP)
        PULO=.5D0*(COS(DLAP)**2*SIN(.5D0*DLOP)**2/CVAL+
     +           TVAL*COS(DLAP)*COS(.5D0*DLOP)*SIN(DLAP)**2+CSLT)
        PVLA=.5D0*(SIN(DLAP)**2*COS(.5D0*DLOP)/CVAL+
     +           TVAL*SIN(.5D0*DLOP)**2*COS(DLAP)+1.D0)
        PVLO=.125D0*(SIN(2.D0*DLAP)*SIN(.5D0*DLOP)/CVAL-
     +             TVAL*SIN(DLAP)*COS(DLAP)**2*SIN(DLOP))
C
        DNOM=PULA*PVLO-PVLA*PULO
C
C DLAT and DLON are improved values of latitude and longitude.
C
        DLAT=DLAP-(UDIF*PVLO-VDIF*PULO)/DNOM
        DLON=DLOP-(VDIF*PULA-UDIF*PVLA)/DNOM
C
C If the improved values are not sufficiently different from the
C previous values and fewer than 20 iterations have been performed,
C go back for another one.  Otherwise, just return DLAT and DLON.
C
        IF (ABS(DLAT-DLAP).GT.1.D-12.OR.ABS(DLON-DLOP).GT.1.D-12) THEN
          IF (NITR.LT.20) THEN
            DLAP=DLAT
            DLOP=DLON
            NITR=NITR+1
            GO TO 101
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
