C
C $Id: mapstl.f,v 1.11 2000-07-12 16:23:20 haley Exp $
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
      SUBROUTINE MAPSTL (WHCH,LVAL)
C
      CHARACTER*(*) WHCH
      LOGICAL LVAL
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
      IF (ICFELL('MAPSTL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
      IF      (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
        IDTL=0
        IF (LVAL) IDTL=1
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        IDOT=0
        IF (LVAL) IDOT=1
      ELSE IF (WHCH(1:2).EQ.'EL'.OR.WHCH(1:2).EQ.'el') THEN
        ELPF=LVAL
      ELSE IF (WHCH(1:2).EQ.'LA'.OR.WHCH(1:2).EQ.'la') THEN
        LBLF=LVAL
      ELSE IF (WHCH(1:2).EQ.'PE'.OR.WHCH(1:2).EQ.'pe') THEN
        PRMF=LVAL
      ELSE
        GO TO 901
      END IF
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL MAPCEM ('MAPSTL - UNKNOWN PARAMETER NAME ',WHCH,2,1)
      RETURN
C
      END
