C
C $Id: pcpnwi.f,v 1.4 1995-05-01 22:21:16 kennison Exp $
C
      CHARACTER*16 FUNCTION PCPNWI (WHCH,IPAI)
C
      CHARACTER*(*) WHCH
      CHARACTER*16   CTMP
C
C Given the name (WHCH) of an internal parameter that is really an array
C and the integer value (IPAI) of the desired parameter array index, the
C value of PCPNWI (WHCH,IPAI) is the internal parameter name including
C the index, in the form expected by the routines PCGETR and PCSETR (in
C parentheses following the two-character internal parameter name).
C
      IF (LEN(WHCH).LT.2) THEN
        CALL SETER ('PCPNWI - PARAMETER NAME TOO SHORT',1,1)
        RETURN
      ELSE
        WRITE (CTMP,'(I16)') IPAI
        IBEG=0
        DO 101 I=1,16
          IF (CTMP(I:I).NE.' ') THEN
            IF (IBEG.EQ.0) IBEG=I
            IF (IBEG.NE.0) IEND=I
          END IF
  101   CONTINUE
        IF (IBEG.EQ.0) THEN
          CALL SETER ('PCPNWI - INTERNAL ERROR - SEE CONSULTANT',2,1)
          RETURN
        ELSE
          PCPNWI=WHCH(1:2)//'('//CTMP(IBEG:IEND)//')'//'            '
        END IF
      END IF
C
C Done.
C
      RETURN
C
      END
