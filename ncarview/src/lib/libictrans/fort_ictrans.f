C
C	$Id: fort_ictrans.f,v 1.5 2000-08-22 15:11:58 haley Exp $
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
C
C	subroutine GOPNIC
C
C	Flag GKS I/O module (wks.c) to direct output to a memory file and to 
C	call ictrans when workstation is closed
C 
C on entry
C	STATUS		: 1 => GKS output sent to ictrans, 
C			  0 => normal operation
      SUBROUTINE GOPNIC(STATUS)
        INTEGER STATUS

        CALL INITIC

        STATUS = 1

        RETURN
      END

C
C	subroutine	GIARGS
C
C	Provide an argument list to be passed to ictrans when invoked by
C	'close workstation' (GCLWK)
C
C on entry
C	UNIT		: workstation connection identifier
C	ARGS		: ictrans argument string
C on exit
C	STATUS		: == -1 => failure, else ok
C

      SUBROUTINE GIARGS(UNIT, ARGS, STATUS)
        INTEGER UNIT, STATUS
        CHARACTER*(*) ARGS


        CHARACTER*200 NARGS

        I = LEN(ARGS) + 1

        IF (I .GT. 200) THEN
          STATUS = -1
          RETURN
        END IF

        NARGS = ARGS
        NARGS(I:I) = CHAR(0)

        CALL ICTARG(UNIT, NARGS, STATUS)

        RETURN
      END
