C
C	$Id: giitm.f,v 1.3 2000-07-12 16:39:42 haley Exp $
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
      SUBROUTINE GIITM (TYPE,LDR,DATREC)
C
C  INTERPRET ITEM
C
      INTEGER EIITM
      PARAMETER (EIITM=104)
C
      include 'gkscom.h'
C
      INTEGER TYPE,LDR
      CHARACTER*80 DATREC(LDR)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EIITM,IER)
      IF (IER .NE. 0) RETURN
C
C  Set function code and put out WKID.
C
      FCODE = 104
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = TYPE
      IF (LDR .GE. 1) THEN
        IF (LDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = DATREC(1)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EIITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ELSE
C
C  Send over the data record 80 characters at a time.
C
          CONT = 1
          STRL1 = 80*LDR
          STRL2 = 80
          LDRM1 = LDR-1
          DO 200 I=1,LDRM1
            IF (I .GT. 1) IL2 = 0
            STR(1:80) = DATREC(I)
            CALL GZTOWK
            IF (RERR.NE.0) THEN
              ERS = 1
              CALL GERHND(RERR,EIITM,ERF)
              ERS = 0
              RETURN
            ENDIF
  200     CONTINUE
          CONT = 0
          STR(1:80) = DATREC(LDR)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EIITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ELSE
        CONT = 0
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EIITM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      RETURN
      END
