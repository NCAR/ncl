C
C $Id: displa.f,v 1.6 2008-07-27 00:14:36 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DISPLA (LFRA,LROW,LTYP)
C
C The subroutine DISPLA resets the parameters IFRA, IROW, and/or LLUX
C and LLUY.
C
      IF (LFRA.NE.0) CALL AGSETI ('FRAM.', MAX(1,MIN(3,LFRA)))
C
      IF (LROW.NE.0) CALL AGSETI ('ROW .',LROW)
C
      IF (LTYP.EQ.0) RETURN
C
      ITYP=MAX(1,MIN(4,LTYP))
      CALL AGSETI ('X/LOGA.',   (1-ITYP)/2)
      CALL AGSETI ('Y/LOGA.',MOD(1-ITYP,2))
C
      RETURN
C
      END
