C
C	$Id: gzckst.f,v 1.4 2000-08-22 15:08:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE GZCKST(NUM,ENAM,IER)
C
C  This subroutine provides the checking for error numbers
C  1 through 8.
C
C  INPUT:
C    NUM   -- Error number to check for
C    ENAM  -- Index of name of calling program (this is non-zero
C             only for non-inquiry functions, in which case GERHND
C             is called).
C
C  OUTPUT:
C    IER   -- 0 if no error, otherwise the error number.
C
      include 'gkscom.h'
C
      INTEGER ENAM
      RERR = 0
      IER = 0
      GO TO (10,20,30,40,50,60,70,80) NUM
C
   10 CONTINUE
      IF (OPS .NE. GGKCL) THEN
        IER = 1
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(1,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   20 CONTINUE
      IF (OPS .NE. GGKOP) THEN
        IER = 2
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(2,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   30 CONTINUE
      IF (OPS .NE. GWSAC) THEN
        IER = 3
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(3,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   40 CONTINUE
      IF (OPS .NE. GSGOP) THEN
        IER = 4
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(4,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   50 CONTINUE
      IF (OPS.NE.GWSAC .AND. OPS.NE.GSGOP) THEN
        IER = 5
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(5,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   60 CONTINUE
      IF (OPS.NE.GWSAC .AND. OPS.NE.GWSOP) THEN
        IER = 6
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(6,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   70 CONTINUE
      IF (OPS.NE.GWSOP .AND. OPS.NE.GWSAC .AND. OPS.NE.GSGOP) THEN
        IER = 7
        IF (ENAM .GE. 0) THEN
          ERS = 1
          CALL GERHND(7,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      GO TO 100
C
   80 CONTINUE
      IF (OPS.NE.GGKOP  .AND.  OPS.NE.GWSOP  .AND.
     +    OPS.NE.GWSAC  .AND.  OPS.NE.GSGOP) THEN
        IER = 8
        IF (ENAM.GE.0) THEN
          ERS = 1
          CALL GERHND(8,ENAM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
C
  100 CONTINUE
      RETURN
      END
