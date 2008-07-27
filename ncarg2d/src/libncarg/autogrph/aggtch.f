C
C $Id: aggtch.f,v 1.6 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGGTCH (IDCS,CHST,LNCS)
C
      CHARACTER*(*) CHST
C
C This routine gets character strings previously stored by the routine
C AGSTCH (which see).  It has the following arguments:
C
C -- IDCS is the identifying integer returned by AGSTCH when the string
C    was stored.
C
C -- CHST is the character string returned.
C
C -- LNCS is the length of the character string returned in CHST.
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
C First, blank-fill the character variable to be returned.
C
      CHST=' '
C
C If the identifier is less than -LNIC, the (one-character) string is
C retrieved from it.
C
      IF (IDCS.LT.(-LNIC)) THEN
        CHST=CHAR(-IDCS-LNIC-1)
        LNCS=1
C
C If the identifier is between -LNIC and -1, its absolute value is the
C index, in INCH, of the descriptor of the character string stored in
C CHRA.
C
      ELSE IF (IDCS.LE.(-1)) THEN
        I=-IDCS
        J=INCH(1,I)-1
        IF (J.GE.0) THEN
          LNCS=MIN(LEN(CHST),INCH(2,I))
          DO 101 K=1,LNCS
            J=J+1
            CHST(K:K)=CHRA(J)
  101     CONTINUE
        ELSE
          LNCS=0
        END IF
C
C In all other cases, return a single blank.
C
      ELSE
        LNCS=1
C
      END IF
C
C Done.
C
      RETURN
C
      END
