C
C $Id: maptra.f,v 1.7 1999-04-02 22:59:39 kennison Exp $
C
      SUBROUTINE MAPTRA (RLAT,RLON,UVAL,VVAL)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE   /MAPCM6/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPTRA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPTRA',2).NE.0) RETURN
      END IF
C
C The call to MAPTRA is simply passed on to MAPTRN, but the values
C returned are checked to see if the point lies outside the perimeter;
C if so, the value 1.E12 is substituted for UVAL.
C
      CALL MAPTRN (RLAT,RLON,UVAL,VVAL)
      IF (ICFELL('MAPTRA',3).NE.0) RETURN
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
