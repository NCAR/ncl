C
C $Id: pcgtdi.f,v 1.11 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCGTDI (CHRS,NCHR,ICHR,NUMV)
C
C The subroutine PCGTDI gets an optionally signed decimal integer from
C the character string CHRS.  Its arguments are as follows:
C
C   CHRS is an input variable.  It contains a character string.
C
C   NCHR is an input variable specifying the number of characters in
C   CHRS.
C
C   ICHR is both an input variable and an output variable.  On entry,
C   it contains the index of the character in CHRS immediately after
C   which a decimal integer may occur.  On exit, it contains the index
C   of the last character of the integer, if one was found; otherwise,
C   it is unchanged.
C
C   NUMV is an output variable containing the value of the decimal
C   integer found.  The value zero indicates that no such integer was
C   found.
C
C COMMON block declarations.
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(7000),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      CHARACTER*(*) CHRS
C
C Initialize the value of the sign multiplier and the number.
C
      ISGN=1
      NUMV=0
C
C Increment the character index to the possible beginning of the number.
C
      ICHR=ICHR+1
C
C If there are at least two characters past this point in the string,
C look for a sign.
C
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR:ICHR).EQ.'-') THEN
          ISGN=-1
          ICHR=ICHR+1
        ELSE IF (CHRS(ICHR:ICHR).EQ.'+') THEN
          ISGN=1
          ICHR=ICHR+1
        ELSE IF (IDPC(ICHAR(CHRS(ICHR:ICHR))).LT.27.OR.
     +           IDPC(ICHAR(CHRS(ICHR:ICHR))).GT.36) THEN
          ISGN=0
        END IF
      END IF
C
C If we haven't found any character implying that no decimal integer
C starts at character index ICHR, find the numeric value.
C
      IF (ISGN.NE.0) THEN
        CALL PCGTPI (CHRS,NCHR,ICHR,10,NUMV)
      END IF
C
C Tack on the proper sign.
C
      NUMV=ISGN*NUMV
C
C Decrement the character index.
C
      ICHR=ICHR-1
C
C Done.
C
      RETURN
C
      END
