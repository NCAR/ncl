C
C	$Id: pcffgp.f,v 1.3 2000-07-12 16:24:56 haley Exp $
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
      SUBROUTINE PCFFGP(NUM,IOFF,PFLG,X,Y)
C
C  Get NUM packets from the fontcap starting at bit offset IOFF.
C  Store the packet(s) in the arrays PFLG, X, and Y.
C
      include 'pcffme.h'
      include 'pcffdx.h'
C
      INTEGER PFLG(*),X(*),Y(*)
C
      CALL GBYTES(IBFC,PFLG,IOFF,PKFLWD,XBITWD+YBITWD,NUM)
      CALL GBYTES(IBFC,X,IOFF+PKFLWD,XBITWD,YBITWD+PKFLWD,NUM)
      CALL GBYTES(IBFC,Y,IOFF+PKFLWD+XBITWD,YBITWD,PKFLWD+XBITWD,NUM)
C
      RETURN
      END
