C
C	$Id: gwitx.f,v 1.4 2000-07-12 16:54:43 haley Exp $
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
      SUBROUTINE GWITX
C
C  Process TEXT instruction.
C
C ************************************************************************
C **                                                                    **
C **     NOTE:  Continuation across character partitions is not         **
C **            implemented.  This limits character strings to          **
C **            lengths less than 32768.                                **
C **                                                                    **
C ************************************************************************
C
      include 'gksin.h'
      include 'gwiio.h'
      include 'gwiins.h'
      include 'gwiwsl.h'
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadc.h'
      include 'gwiopc.h'
      include 'gwienu.h'
C
      INTEGER  NUMO(2), KALL, IPRIM, NBYTES
      INTEGER  GFINAL, G01PBL
      SAVE
C
      DATA KALL/0/,  IPRIM/3/,  GFINAL/1/
C
      KALL = KALL+1
      IF  (KALL.EQ.1) THEN
C
C  First call of text element.
C
C  If the picture is empty, send the clip indicator and rectangle;
C  set the WSL entry "DISPLAY SURFACE EMPTY" to "NOT EMPTY".
C
        IF (MDEMPT .EQ. GEMPTY) CALL GWICLP(1)
        MDEMPT = GNEMPT
C
C  Process pending attributes.
C
        IF (AGPEND(IPRIM)) THEN
C
C  Some changes are pending.
C
           IF (VALCHG(IVTXIX))  THEN
C
C  TEXT INDEX
C
              NBYTES = 1+(MIXFW-1)/8
              CALL GWPTNI (CLTBIX, IDTBIX, NBYTES, RERR)
              CALL GWPTPR (MRTXIX, MIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXIX         = MRTXIX
              VALCHG(IVTXIX) = .FALSE.
           END IF
           IF (VALCHG(IVTXFO))  THEN
C
C  FONT component of text font and precision.
C
              NBYTES = 1+(MEFW-1)/8
              CALL GWPTNI (CLTFON, IDTFON, NBYTES, RERR)
              CALL GWPTPR (MRTXFO, MEFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXFO         = MRTXFO
              VALCHG(IVTXFO) = .FALSE.
           END IF
           IF (VALCHG(IVTXPR)) THEN
C
C  Precision component of text font and precision.
C
              NBYTES         = 1+(MIXFW-1)/8
              CALL GWPTNI (CLTPRE, IDTPRE, NBYTES, RERR)
              CALL GWPTPR (MRTXPR, MIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXPR         = MRTXPR
              VALCHG(IVTXPR) = .FALSE.
           END IF
           IF (VALCHG(IVCHXP)) THEN
C
C  Character expansion factor.
C
              NBYTES = 1+(2*MCFPP-1)/8
              CALL GWPTNI (CLCHEX, IDCHEX, NBYTES, RERR)
              CALL GFLCNV (ARCHXP, NUMO)
              CALL GWPTPR (NUMO, MCFPP, 2, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              ASCHXP         = ARCHXP
              VALCHG(IVCHXP) = .FALSE.
           END IF
           IF (VALCHG(IVCHSP)) THEN
C
C  Character spacing.
C
              NBYTES = 1+(2*MCFPP-1)/8
              CALL GWPTNI (CLCHSP, IDCHSP, NBYTES, RERR)
              CALL GFLCNV (ARCHSP, NUMO)
              CALL GWPTPR (NUMO, MCFPP, 2, RERR)
C
C  Set sent value to requested, clear change flag.
C
              ASCHSP         = ARCHSP
              VALCHG(IVCHSP) = .FALSE.
           END IF
           IF (VALCHG(IVTXCI)) THEN
C
C  Text color index.
C
              NBYTES = 1+(MCIXFW-1)/8
              CALL GWPTNI (CLTCLR, IDTCLR, NBYTES, RERR)
              CALL GWPTPR (MRTXCI, MCIXFW, 1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXCI         = MRTXCI
              VALCHG(IVTXCI) = .FALSE.
           END IF
           IF (VALCHG(IVTXP)) THEN
C
C  Text path.
C
              NBYTES = 1+(MEFW-1)/8
              CALL GWPTNI (CLTXPA, IDTXPA, NBYTES, RERR)
              CALL GWPTPR (MRTXP , MEFW,  1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXP         = MRTXP
              VALCHG(IVTXP) = .FALSE.
           END IF
           IF (VALCHG(IVTXAL)) THEN
C
C  Text alignment.
C
              NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
              CALL GWPTNI (CLTXAL, IDTXAL, NBYTES, RERR)
C
C  Put in CGM discrete alignment values.
C
              CALL GWPTPR (MRTXAL, MEFW, 2, RERR)
C
C  Add (unused) CGM continuous parameters.
C
              NUMO(1) = 0
              NUMO(2) = 0
              CALL GWPTPR (NUMO, MCFPP, 2, RERR)
              CALL GWPTPR (NUMO, MCFPP, 2, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSTXAL(1)      = MRTXAL(1)
              MSTXAL(2)      = MRTXAL(2)
              VALCHG(IVTXAL) = .FALSE.
           END IF
           IF (VALCHG(IVCHH)) THEN
C
C  Character height.
C
              NBYTES = 1+(MVDCFW-1)/8
              CALL GWPTNI (CLCHHT, IDCHHT, NBYTES, RERR)
              CALL GWPTPR (MRCHH, MVDCFW,  1, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSCHH         = MRCHH
              VALCHG(IVCHH) = .FALSE.
           END IF
           IF (VALCHG(IVCHOV)) THEN
C
C  Character up vector.
C
              NBYTES = 1+(4*MVDCFW-1)/8
              CALL GWPTNI (CLCHOR, IDCHOR, NBYTES, RERR)
              CALL GWPTPR (MRCHOV(1) , MVDCFW,  4, RERR)
              IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
              MSCHOV(1)      = MRCHOV(1)
              MSCHOV(2)      = MRCHOV(2)
              MSCHOV(3)      = MRCHOV(3)
              MSCHOV(4)      = MRCHOV(4)
              VALCHG(IVCHOV) = .FALSE.
           END IF
           IF (ANYASF)  THEN
C
C  Some ASF has changed.
C
              CALL GWISAS (IPRIM, RERR)
              IF (RERR.NE.0)  RETURN
           END IF
C
C  Clear aggregate change variable.
C
           AGPEND(IPRIM) = .FALSE.
        END IF
C
C  Treat first call, put out opcode, and points.
C
C
C  Put out opcode (class and id) and total length.
C
        NBYTES = G01PBL (STRL1, 1+(MEFW+2*MVDCFW-1)/8 )
        CALL GWPTNI (CLTEXT, IDTEXT, NBYTES, RERR)
C
C  Put out starting point.
C
C  Truncate point to limits of NDC unit square, convert to VDC,
C  and store in WPXPY.
C
        WPXPY(1) = MXOFF + IFIX(FLOAT(MXSCAL)*
     -                 (AMAX1(0.,AMIN1(1.0,RX(1)))))
        WPXPY(2) = MYOFF + IFIX(FLOAT(MYSCAL)*
     -                 (AMAX1(0.,AMIN1(1.0,RY(1)))))
        CALL GWPTPR (WPXPY, MVDCFW, 2, RERR)
C
C  Put out final flag.
C
        CALL GWPTPR (GFINAL, MEFW, 1, RERR)
C
C  Put out first (and perhaps only) part of character string.
C
        CALL GWPTPS (STR, STRL1, STRL2, 0, RERR)
        IF (RERR .NE. 0) RETURN
C
C  End of processing for first call of element.
C
      ELSE IF (KALL .GT. 1) THEN
C
C  This is a continuation call for the element.
C
        CALL GWPTPS (STR, STRL1, STRL2, 1, RERR)
        IF (RERR .NE. 0) RETURN
      END IF
C
C  If there is to be no continuation, reset the parameter "KALL".
C
      IF (CONT .EQ. 0) THEN
         KALL = 0
      END IF
C
      RETURN
C
      END
