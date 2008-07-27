C
C $Id: agstch.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGSTCH (CHST,LNCS,IDCS)
C
      CHARACTER*(*) CHST
C
C This routine stores strings of characters for later retrieval and/or
C modification by the routines AGGTCH, AGRPCH, and AGDLCH.  It has the
C following arguments:
C
C -- CHST is the character string to be stored.
C
C -- LNCS is the length of the character string in CHST.  LNCS must be
C    less than or equal to the value of the FORTRAN function LEN(CHST).
C
C -- IDCS is an identifying integer, returned to the caller by AGSTCH
C    for later use in calls to AGGTCH, AGRPCH, and AGDLCH.  If CHST is
C    more than one character long, it is stashed in the array CHRA, and
C    the value returned in IDCS is a negative number between -LNIC and
C    -1, inclusive, the absolute value of which is the index of an entry
C    in the array INCH describing where in the array CHRA the string was
C    stored.  If CHST is only one character long, IDCS is returned as
C    the value of the FORTRAN expression -(LNIC+1+ICHAR(CHST(1:1))).
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
C If the string is short enough, just embed it in a negative integer
C and return that value to the caller as the identifier of the string.
C
      IF (LNCS.LE.1) THEN
        IDCS=-(LNIC+1+ICHAR(CHST(1:1)))
        RETURN
      END IF
C
C Otherwise, the string must be stashed in CHRA and the negative of the
C index, in INCH, of its descriptor returned to the caller.  Loop, on I,
C through the index of character strings.
C
      DO 104 I=1,LNIC
C
C If the next entry in the index is zeroed, use it for the new string.
C
        IF (INCH(1,I).EQ.0) THEN
C
C Zeroed entry found.  Return the negative of its index to the user.
C
          IDCS=-I
C
C If there isn't enough room for the character string at the end of the
C character-storage array, do some garbage-collecting, eliminating all
C strings of all-zero characters.
C
          IF (LNCS.GT.LNCA-INCA) THEN
            J=0
            K=0
            DO 102 L=1,INCA
              IF (CHRA(L).EQ.CHAR(0)) THEN
                IF (J.EQ.0) J=L
              ELSE
                IF (J.NE.0) THEN
                  DO 101 M=1,LNIC
                    IF (INCH(1,M).GT.K) INCH(1,M)=INCH(1,M)+J-L
  101             CONTINUE
                  J=0
                END IF
                K=K+1
                CHRA(K)=CHRA(L)
              END IF
  102       CONTINUE
            INCA=K
          END IF
C
C If there still isn't enough room for the character string at the end
C of the character-storage array, take an error exit.  Otherwise, stash
C it and return.  All-zero characters are changed to blanks.
C
          IF (LNCS.GT.LNCA-INCA) GO TO 901
          INCH(1,I)=INCA+1
          INCH(2,I)=LNCS
          DO 103 J=1,LNCS
            INCA=INCA+1
            CHRA(INCA)=CHST(J:J)
            IF (ICHAR(CHRA(INCA)).EQ.0) CHRA(INCA)=' '
  103     CONTINUE
          RETURN
C
        END IF
C
  104 CONTINUE
C
C If no zeroed entry was found in the index of character strings, jump
C to log an error and quit.
C
      GO TO 902
C
C Error exits.
C
  901 CALL SETER ('AGSTCH - CHARACTER-STRING BUFFER OVERFLOW - SEE CONSU
     +LTANT',18,2)
C
  902 CALL SETER ('AGSTCH - CHARACTER-STRING INDEX OVERFLOW - SEE CONSUL
     +TANT',19,2)
C
      END
