C
C $Id: tdlblp.f,v 1.2 2008-07-27 00:17:32 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDLBLP (AXIS,VLBL,PLBL)
C
        CHARACTER*1 AXIS
C
C AXIS is a single alphabetic character copied from the beginning of a
C string containing numeric labels to be used on a particular axis; it
C specifies how the label value passed in VLBL is to be used to compute
C a label position to be returned in PLBL.  The default version just
C puts the labels where their numeric values indicate they should go,
C but a user version may be supplied to modify the positions of numeric
C labels along any of the axes more or less at will.
C
        IF      (AXIS.EQ.'U') THEN
          PLBL=VLBL
        ELSE IF (AXIS.EQ.'V') THEN
          PLBL=VLBL
        ELSE IF (AXIS.EQ.'W') THEN
          PLBL=VLBL
        ELSE
          PLBL=VLBL
        END IF
C
        RETURN
C
      END
