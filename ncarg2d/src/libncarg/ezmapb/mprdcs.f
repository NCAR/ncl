C
C $Id: mprdcs.f,v 1.3 1998-05-24 00:40:57 kennison Exp $
C
      SUBROUTINE MPRDCS (IFDE,CHRS,LCHR,MCHR,NCHR,CVAR)
C
        CHARACTER*1 CHRS(LCHR)
C
        CHARACTER*(*) CVAR
C
C Given the file descriptor of an open file in IFDE and a character
C buffer CHRS, of length LCHR, having in it MCHR characters read from
C the file, of which NCHR have previously been processed, this routine
C returns the next LEN(CVAR) characters in the character variable CVAR.
C
C Find the length of the character variable.
C
        LOCV=LEN(CVAR)
C
C Define an index into the character variable.
C
        IICV=0
C
C Transfer LOCV characters to the character variable.
C
  101   NCHR=NCHR+1
C
        IF (NCHR.GT.MCHR) THEN
          CALL NGRDCH (IFDE,CHRS,LCHR,ISTA)
          IF (ISTA.LE.0) THEN
            MCHR=0
            RETURN
          ELSE
            MCHR=ISTA
            NCHR=1
          END IF
        END IF
C
        IICV=IICV+1
        CVAR(IICV:IICV)=CHRS(NCHR)
        IF (IICV.EQ.LOCV) RETURN
C
        GO TO 101
C
      END
