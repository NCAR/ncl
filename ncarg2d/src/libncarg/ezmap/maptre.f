C
C	$Id: maptre.f,v 1.1.1.1 1992-04-17 22:32:06 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRE (UINS,VINS,UOUT,VOUT,UINT,VINT)
C
C This routine finds the point of intersection (UINT,VINT) of the line
C from (UINS,VINS) to (UOUT,VOUT) with the edge of an elliptical frame.
C The first point is inside the frame and the second outside the frame.
C
C Because MAPTRE can be called with the same actual arguments for UINT
C and VINT as for UOUT and VOUT, respectively, UINT and VINT must not
C be reset until all use of UOUT and VOUT is complete.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
C
C What's involved is just a lot of algebra.
C
      IF (ABS(UOUT-UINS).GT.ABS(VOUT-VINS)) THEN
        P=(VOUT-VINS)/(UOUT-UINS)
        Q=(UOUT*VINS-UINS*VOUT)/(UOUT-UINS)
        A=VRNG*VRNG+P*P*URNG*URNG
        B=2.*(P*Q*URNG*URNG-UCEN*VRNG*VRNG-P*URNG*URNG*VCEN)
        C=UCEN*UCEN*VRNG*VRNG+Q*Q*URNG*URNG-2.*Q*URNG*URNG*VCEN+
     +                           URNG*URNG*VCEN*VCEN-URNG*URNG*VRNG*VRNG
        UTM1=SQRT(MAX(B*B-4.*A*C,0.))
        UTM2=.5*(-B-UTM1)/A
        UTM3=.5*(-B+UTM1)/A
        IF (ABS(UTM2-(UINS+UOUT)/2.).LT.ABS(UTM3-(UINS+UOUT)/2.)) THEN
          UINT=UTM2
        ELSE
          UINT=UTM3
        END IF
        VINT=P*UINT+Q
      ELSE
        P=(UOUT-UINS)/(VOUT-VINS)
        Q=(UINS*VOUT-UOUT*VINS)/(VOUT-VINS)
        A=URNG*URNG+P*P*VRNG*VRNG
        B=2.*(P*Q*VRNG*VRNG-URNG*URNG*VCEN-P*UCEN*VRNG*VRNG)
        C=URNG*URNG*VCEN*VCEN+Q*Q*VRNG*VRNG-2.*Q*UCEN*VRNG*VRNG+
     +                           UCEN*UCEN*VRNG*VRNG-URNG*URNG*VRNG*VRNG
        VTM1=SQRT(MAX(B*B-4.*A*C,0.))
        VTM2=.5*(-B-VTM1)/A
        VTM3=.5*(-B+VTM1)/A
        IF (ABS(VTM2-(VINS+VOUT)/2.).LT.ABS(VTM3-(VINS+VOUT)/2.)) THEN
          VINT=VTM2
        ELSE
          VINT=VTM3
        END IF
        UINT=P*VINT+Q
      END IF
C
C Done.
C
      RETURN
C
      END
