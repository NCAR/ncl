C
C	$Id: maptra.f,v 1.1.1.1 1992-04-17 22:32:06 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPTRA (RLAT,RLON,UVAL,VVAL)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE /MAPCM6/
C
C The call to MAPTRA is simply passed on to MAPTRN, but the values
C returned are checked to see if the point lies outside the perimeter;
C if so, the value 1.E12 is substituted for UVAL.
C
      CALL MAPTRN (RLAT,RLON,UVAL,VVAL)
C
      IF (ELPM) THEN
        IF (((UVAL-UCNM)/URNM)**2+
     +      ((VVAL-VCNM)/VRNM)**2.GT.1.000002) THEN
          UVAL=1.E12
        END IF
      ELSE
        IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +      VVAL.LT.VMNM.OR.VVAL.GT.VMXM) THEN
          UVAL=1.E12
        END IF
      END IF
C
      RETURN
C
      END
