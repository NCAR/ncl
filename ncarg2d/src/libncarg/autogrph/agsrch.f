C
C $Id: agsrch.f,v 1.5 2000-07-12 16:22:02 haley Exp $
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
      SUBROUTINE AGSRCH (TPID,IPID,IKWL,UKWL,LKWL)
C
      CHARACTER*(*) TPID,UKWL,LKWL
C
C The routine AGSRCH is used by AGSCAN to search a parameter identifier
C for the next keyword and return the index of that keyword in a list of
C keywords.  It has the following arguments.
C
C -- TPID is the parameter identifier, a character string.
C
C -- IPID is the index of the last character examined in TPID.  It is
C    updated by AGSRCH to point to the first slash or period following
C    the next keyword.
C
C -- IKWL is returned containing the index (in the keyword list) of the
C    next keyword in the parameter identifier (list length, plus one,
C    if the keyword is not found in the list).
C
C -- UKWL is the keyword list - 4*NOKW characters in all.
C
C -- LKWL is identical to UKWL, but in lower case.
C
C ICHR is used to hold up to four characters of a keyword.
C
      CHARACTER*4 ICHR
C
C LPID is the assumed maximum length of a parameter identifier.
C
      DATA LPID / 100 /
C
C Compute the number of 4-character keywords in the keyword list.
C
      NOKW=LEN(UKWL)/4
C
C Find the next non-blank in the parameter identifier.
C
  101 IPID=IPID+1
      IF (IPID.GT.LPID) GO TO 107
      IF (TPID(IPID:IPID).EQ.' ') GO TO 101
C
C Pick up at most four characters of the keyword, stopping on the first
C blank, slash, or period encountered.
C
      NCHR=0
C
  102 IF (TPID(IPID:IPID).EQ.' '.OR.
     +    TPID(IPID:IPID).EQ.'/'.OR.
     +    TPID(IPID:IPID).EQ.'.') GO TO 103
C
      NCHR=NCHR+1
      ICHR(NCHR:NCHR)=TPID(IPID:IPID)
C
      IPID=IPID+1
C
      IF (NCHR.LT.4) GO TO 102
C
C If the keyword found has zero length, error.
C
  103 IF (NCHR.EQ.0) GO TO 107
C
C Scan ahead for the next slash or period.
C
  104 IF (TPID(IPID:IPID).EQ.'/'.OR.TPID(IPID:IPID).EQ.'.') GO TO 105
C
      IPID=IPID+1
      IF (IPID.GT.LPID) GO TO 107
      GO TO 104
C
C Search the keyword list for the keyword found.
C
  105 DO 106 I=1,NOKW
        IKWL=I
        ISTR=(I-1)*4+1
        IEND=(I-1)*4+NCHR
        IF (ICHR(1:NCHR).EQ.UKWL(ISTR:IEND).OR.
     +      ICHR(1:NCHR).EQ.LKWL(ISTR:IEND)) RETURN
  106 CONTINUE
C
C Keyword not found - set IKWL to impossible value and return.
C
  107 IKWL=NOKW+1
      RETURN
C
      END
