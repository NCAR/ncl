C
C $Id: agdlch.f,v 1.6 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGDLCH (IDCS)
C
C This routine deletes character strings previously stored by the
C routine AGSTCH (which see).  It has the following argument:
C
C -- IDCS is the identifying integer returned by AGSTCH when the string
C    was stored.
C
C The following common blocks contain variables which are required for
C the character-storage-and-retrieval scheme of AUTOGRAPH.
C
      COMMON /AGCHR1/ LNIC,INCH(2,50),LNCA,INCA
      SAVE /AGCHR1/
C
      COMMON /AGCHR2/ CHRA(2000)
      CHARACTER*1 CHRA
      SAVE /AGCHR2/
C
C Only if the identifier is between -LNIC and -1, inclusive, was the
C string ever stored, so that it needs to be deleted.  If the string is
C the last one in CHRA, we can just set INCA to point to the position
C preceding it; otherwise, we zero out the string but don't bother to
C collapse CHRA, which will happen in AGSTCH when the space is needed
C again.  In either case, the index entry in INCH is zeroed.
C
      IF (IDCS.GE.(-LNIC).AND.IDCS.LE.(-1)) THEN
        I=-IDCS
        J=INCH(1,I)
        IF (J.GT.0) THEN
          K=J+INCH(2,I)-1
          IF (K.EQ.INCA) THEN
            INCA=J-1
          ELSE
            DO 101 L=J,K
              CHRA(L)=CHAR(0)
  101       CONTINUE
          END IF
          INCH(1,I)=0
        END IF
      END IF
C
C Done.
C
      RETURN
C
      END
