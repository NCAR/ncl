C
C $Id: mprdnm.f,v 1.1 1995-07-01 00:04:55 kennison Exp $
C
      SUBROUTINE MPRDNM (IFDE,CHRS,LCHR,MCHR,NCHR,INUM)
C
      CHARACTER*1 CHRS(LCHR)
C
C Given the file descriptor of an open file in IFDE and a character
C buffer CHRS, of length LCHR, having in it MCHR characters read from
C the file, of which NCHR have previously been processed, this routine
C finds the next integer in the file and returns that as the value of
C INUM.
C
C Skip blanks.
C
  101 NCHR=NCHR+1
C
      IF (.NOT.(NCHR.GT.MCHR)) GO TO 10000
      CALL NGRDCH (IFDE,CHRS,LCHR,ISTA)
      IF (.NOT.(ISTA.LE.0)) GO TO 10001
      MCHR=0
      RETURN
10001 CONTINUE
      MCHR=ISTA
      NCHR=1
10000 CONTINUE
C
      IF (CHRS(NCHR).EQ.' ') GO TO 101
C
C Translate the number, stopping on a blank.
C
      INUM=ICHAR(CHRS(NCHR))-ICHAR('0')
C
  102 NCHR=NCHR+1
C
      IF (.NOT.(NCHR.GT.MCHR)) GO TO 10002
      CALL NGRDCH (IFDE,CHRS,LCHR,ISTA)
      IF (.NOT.(ISTA.LE.0)) GO TO 10003
      MCHR=0
      RETURN
10003 CONTINUE
      MCHR=ISTA
      NCHR=1
10002 CONTINUE
C
      IF (CHRS(NCHR).EQ.' ') RETURN
C
      INUM=10*INUM+ICHAR(CHRS(NCHR))-ICHAR('0')
C
      GO TO 102
C
      END
