C
C $Id: pcfred.f,v 1.5 2000-07-12 16:24:57 haley Exp $
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
      SUBROUTINE PCFRED (IBNU,NFNT,IBUF,LBUF)
        CHARACTER*32 CTMP
        DIMENSION IBUF(LBUF)
        IF (NFNT.EQ.0) THEN
          CALL NGRDIN (IBNU,IBUF,LBUF,ISTA)
          IF (ISTA.NE.LBUF) THEN
            CALL SETER ('PCFRED - ERROR READING PWRITX DATABASE',2,1)
          END IF
        ELSE
          CALL PCBNRD (IBNU,LBUF,IBUF,IOST,ISTA)
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFRED - ERROR READING FONT'',I5)') NFNT
            CALL SETER (CTMP,1,1)
            RETURN
          END IF
        END IF
        RETURN
      END
