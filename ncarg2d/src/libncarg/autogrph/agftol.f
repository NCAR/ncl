C
C $Id: agftol.f,v 1.3 2000-07-12 16:21:57 haley Exp $
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
      SUBROUTINE AGFTOL (IAXS,IDMA,VINP,VOTP,VLCS,LLUA,UBEG,UDIF,FUNS,
     +                                                        NBTP,SBSE)
C
C The routine AGFTOL is used by AGAXIS to map a fractional distance
C along the axis to a value in the label coordinate system or vice-
C versa.  Its arguments are as follows:
C
C -- IAXS specifies which axis is being drawn.  It is passed to the
C    routine AGUTOL.  See AGAXIS for a complete description of IAXS.
C
C -- IDMA specifies the direction of the mapping - from the fractional
C    system to the label system if IDMA .GT. 0 or from the label system
C    to the fractional system if IDMA .LT. 0.  IDMA also specifies how
C    the label-system value is given to or returned by AGFTOL.
C
C     -- If ABSV(IDMA) .EQ. 1, an actual value in the label coordinate
C        system (VLCS) is given to or returned by AGFTOL.
C
C     -- If ABSV(IDMA) .NE. 1, a value of the exponent/multiplier EXMU
C        corresponding to VLCS is given to or returned by AGFTOL.
C
C -- VINP is an input value in one coordinate system.
C
C -- VOTP is an output value in the other coordinate system.
C
C -- VLCS is an output value in the label coordinate system, returned
C    no matter what the value of IDMA.
C
C -- LLUA, UBGA, and UDFA specify the mapping from the user coordinate
C    system to the fractional system and vice-versa.  See the routine
C    AGAXIS for a complete description of these parameters.
C
C -- FUNS is a function-selector, to be used in calls to AGUTOL.  It
C    selects the mapping from the user coordinate system to the label
C    coordinate system and vice-versa.  See the routine AGAXIS for a
C    complete description of this parameter.
C
C -- NBTP and SBSE specify the mapping of label-coordinate-system values
C    to exponent/multiplier values and vice-versa.  See the routine
C    AGNUMB for a complete dexcription of these parameters.
C
C Determine desired direction of mapping.
C
      IF (IDMA.GT.0) THEN
C
C Map axis fraction VINP to a label-coordinate-system value VLCS.
C
        VUCS=UBEG+VINP*UDIF
        IF (LLUA.NE.0) VUCS=10.**VUCS
        CALL AGUTOL (IAXS,FUNS,1,VUCS,VLCS)
C
C If IDMA .EQ. 1, caller wants VLCS - otherwise, map VLCS to the
C appropriate exponent/multiplier value EXMU - return value in VOTP.
C
        IF (IDMA.EQ.1) THEN
          VOTP=VLCS
          RETURN
        END IF
C
        GO TO (101,102,103) , NBTP
C
  101   VOTP=VLCS/SBSE
        RETURN
C
  102   VOTP=ALOG10(VLCS/SBSE)
        RETURN
C
  103   VOTP=ALOG10(ABS(VLCS))/ALOG10(ABS(SBSE))
        RETURN
C
      ELSE
C
C If IDMA .EQ. -1, caller has provided VINP .EQ. VLCS, a value in the
C label coordinate system - otherwise, VINP .EQ. EXMU, the exponent/
C multiplier needed to generate VLCS.
C
        IF (IDMA.EQ.(-1)) THEN
          VLCS=VINP
          GO TO 107
        END IF
C
        GO TO (104,105,106) , NBTP
C
  104   VLCS=SBSE*VINP
        GO TO 107
C
  105   VLCS=SBSE*10.**VINP
        GO TO 107
C
  106   VLCS=SIGN(1.,SBSE)*ABS(SBSE)**VINP
C
C Map label-system value VLCS to a user-system value VUCS.
C
  107   CALL AGUTOL (IAXS,FUNS,-1,VLCS,VUCS)
C
C Map user-system value VUCS to an axis fraction VOTP and return.
C
        IF (LLUA.NE.0) VUCS=ALOG10(VUCS)
        VOTP=(VUCS-UBEG)/UDIF
        RETURN
C
      END IF
C
      END
