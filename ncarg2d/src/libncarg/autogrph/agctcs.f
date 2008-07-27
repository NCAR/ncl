C
C $Id: agctcs.f,v 1.6 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
