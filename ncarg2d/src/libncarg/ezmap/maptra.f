C
C $Id: maptra.f,v 1.8 2000-07-12 16:23:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
