C
C $Id: pcfopn.f.sed,v 1.3 2000-08-22 15:05:23 haley Exp $
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
      SUBROUTINE PCFOPN (IBNU,NFNT)
        CHARACTER*128 FLNM
        CHARACTER*131 CTMP
        IF (NFNT.EQ.0) THEN
          FLNM=' '
          CALL GNGPAT (FLNM,'SED_DBDIR',ISTA)
          IF (ISTA .NE. -1) THEN
            DO 101 I=1,115
              IF (FLNM(I:I).EQ.CHAR(0)) THEN
                FLNM(I:I+13)='/PlotcharData'//CHAR(0)
                GO TO 104
              END IF
  101       CONTINUE
            GO TO 105
          ELSE
            DO 102 I=2,128
              LENEM=I
              IF (FLNM(I:I).EQ.CHAR(0)) GO TO 103
  102       CONTINUE
  103       CTMP='PCFOPN - '//FLNM(1:LENEM-1)
            CALL SETER (CTMP(1:MIN(LENEM+8,131)),1,1)
            RETURN
          END IF
  104     CALL NGOFRO (FLNM,IBNU,ISTA)
          IF (ISTA.NE.0) GO TO 105
        ELSE
          CALL BOFRED (IBNU,NFNT,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFOPN - ERROR OPENING FONT'',I5)') NFNT
            CALL SETER (CTMP(1:32),2,1)
            RETURN
          END IF
        END IF
        RETURN
  105   CTMP='PCFOPN - ERROR OPENING PWRITX DATA FILE '//FLNM
        DO 106 I=131,1,-1
          IF (CTMP(I:I).NE.' ') THEN
            IEND=I
            GO TO 107
          END IF
  106   CONTINUE
  107   CALL SETER (CTMP(1:IEND),3,1)
        RETURN
      END
