C
C	$Id: drawpv.f,v 1.4 2006-03-16 17:32:33 kennison Exp $
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
      SUBROUTINE DRAWPV (IX,IY,IND)
C
C DRAWPV INTERCEPTS THE CALL TO PLOTIT TO CHECK IF THE PEN HAS TO BE
C MOVED OR IF IT IS ALREADY CLOSE ENOUGH TO THE WANTED POSITION.
C IF IND=2 NEVER MOVE PEN, JUST UPDATE VARIABLES IXSTOR AND IYSTOR.
C
C IN IXSTOR AND IYSTOR THE CURRENT POSITION OF THE PEN IS SAVED.
C
      COMMON /DSHDC/ IXSTOR,IYSTOR
C
      COMMON/INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
     1    ICLOSE
      SAVE
      IIND = IND + 1
      GOTO (100,90,105), IIND
C
   90 CONTINUE
C
C DRAW LINE AND SAVE POSITION OF PEN.
C
      IXSTOR = IX
      IYSTOR = IY
      CALL PLOTIT (IXSTOR,IYSTOR,1)
      GOTO 110
C
  100 CONTINUE
C
C CHECK IF PEN IS ALREADY CLOSE ENOUGH TO THE WANTED POSITION.
C
      IF ((ABS(IXSTOR-IX)+ABS(IYSTOR-IY)) .LE. ICLOSE) GO TO 110
C
      IXSTOR = IX
      IYSTOR = IY
      CALL PLOTIT (IXSTOR,IYSTOR,0)
      GOTO 110
C
  105 CONTINUE
C
C DO NOT MOVE PEN. JUST UPDATE VARIABLES IXSTOR AND IYSTOR.
C
      IXSTOR = IX
      IYSTOR = IY
C
  110 CONTINUE
C
      RETURN
      END
