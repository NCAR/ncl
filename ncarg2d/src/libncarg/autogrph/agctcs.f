C
C $Id: agctcs.f,v 1.3 2000-07-12 16:21:56 haley Exp $
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
      SUBROUTINE AGCTCS (TPID,ITCS)
C
      CHARACTER*(*) TPID
C
C The routine AGCTCS is called by the routines AGGETC and AGSETC to
C check what type of character-string parameter is implied by the
C parameter identifier TPID and return an appropriate value of ITCS, as
C follows:
C
C -- ITCS = 0 implies that the parameter is not intrinsically of type
C    character and that AGGETC/AGSETC should not have been called in
C    the way that it was.
C
C -- ITCS = 1 implies a dash-pattern parameter.
C
C -- ITCS = 2 implies a label name.
C
C -- ITCS = 3 implies the line-end character.
C
C -- ITCS = 4 implies the text of some line of some label.
C
C Find out where in the parameter list the requested parameter lies.
C
      CALL AGSCAN (TPID,LOPA,NIPA,IIPA)
C
C See if it's a dash pattern.
C
      CALL AGSCAN ('DASH/PATT.',LODP,NIDP,IIDP)
      IF (LOPA.GE.LODP.AND.LOPA.LE.LODP+NIDP-1) THEN
        ITCS=1
        RETURN
      END IF
C
C See if it's a label name.
C
      CALL AGSCAN ('LABE/NAME.',LOLN,NILN,IILN)
      IF (LOPA.EQ.LOLN) THEN
        ITCS=2
        RETURN
      END IF
C
C See if it's the line-end character.
C
      CALL AGSCAN ('LINE/END .',LOLE,NILE,IILE)
      IF (LOPA.EQ.LOLE) THEN
        ITCS=3
        RETURN
      END IF
C
C See if it's the text of some label line.
C
      CALL AGSCAN ('LINE/BUFF/CONT.',LOLB,NILB,IILB)
      IF (LOPA.GE.LOLB.AND.LOPA.LE.LOLB+NILB-1.AND.
     +                                       MOD(LOPA-LOLB,6).EQ.3) THEN
        ITCS=4
        RETURN
      END IF
C
C Error - type not recognizable.
C
      ITCS=0
      RETURN
C
      END
