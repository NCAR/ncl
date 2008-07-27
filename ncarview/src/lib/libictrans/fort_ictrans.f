C
C	$Id: fort_ictrans.f,v 1.6 2008-07-27 03:18:45 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
