C
C $Id: displa.f,v 1.2 1996-04-18 17:46:22 kennison Exp $
C
      SUBROUTINE DISPLA (LFRA,LROW,LTYP)
C
C The subroutine DISPLA resets the parameters IFRA, IROW, and/or LLUX
C and LLUY.
C
      IF (LFRA.NE.0) CALL AGSETI ('FRAM.', MAX0(1,MIN0(3,LFRA)))
C
      IF (LROW.NE.0) CALL AGSETI ('ROW .',LROW)
C
      IF (LTYP.EQ.0) RETURN
C
      ITYP=MAX0(1,MIN0(4,LTYP))
      CALL AGSETI ('X/LOGA.',   (1-ITYP)/2)
      CALL AGSETI ('Y/LOGA.',MOD(1-ITYP,2))
C
      RETURN
C
      END
