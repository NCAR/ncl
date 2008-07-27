C
C $Id: agrpch.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGRPCH (CHST,LNCS,IDCS)
C
      CHARACTER*(*) CHST
C
C This routine is used to replace a character string previously stored
C by the routine AGSTCH (which see).  This could be done by an AGDLCH
C followed by an AGSTCH, and, in fact, under certain conditions, does
C exactly that.  Only when it is easy to do so does AGRPCH operate more
C efficiently.  Nevertheless, a user who (for example) repeatedly and
C perhaps redundantly defines x-axis labels of the same length may
C greatly benefit thereby; repeated deletes and stores would lead to
C frequent garbage collection by AGSTCH.
C
C AGRPCH has the following arguments:
C
C -- CHST is the new character string, to replace what was originally
C    stored.
C
C -- LNCS is the length of the character string in CHST.
C
C -- IDCS is the identifier returned by AGSTCH when the original string
C    was stored.  The value of IDCS may be changed by the call.
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
C If the identifier is positive or is negative but less than -LNIC, the
C original string was never stored in CHRA; just treat the replacement
C as a store and return a new value of IDCS.
C
      IF (IDCS.GT.(-1).OR.IDCS.LT.(-LNIC)) THEN
        CALL AGSTCH (CHST,LNCS,IDCS)
C
      ELSE
C
C The absolute value of the identifier is the index, in INCH, of the
C descriptor of the character string stored in CHRA.  If the new string
C is shorter than the old one, store it and zero remaining character
C positions.  Otherwise, treat the replacement as a delete followed by
C a store.
C
        I=-IDCS
        IF (LNCS.LE.INCH(2,I)) THEN
          J=INCH(1,I)-1
          DO 101 K=1,LNCS
            J=J+1
            CHRA(J)=CHST(K:K)
  101     CONTINUE
          DO 102 K=LNCS+1,INCH(2,I)
            J=J+1
            CHRA(J)=CHAR(0)
  102     CONTINUE
          INCH(2,I)=LNCS
        ELSE
          CALL AGDLCH (IDCS)
          CALL AGSTCH (CHST,LNCS,IDCS)
        END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
