C
C $Id: tseter.f,v 1.1 1993-09-24 19:51:51 kennison Exp $
C
      PROGRAM TSETER
C
C This test program demonstrates minimal functioning of the error-
C handling package used by NCAR Graphics.  It first produces a single
C frame showing what output print lines to expect and then steps
C through a simple set of tests that should produce those print lines.
C
C Make required character-variable declarations.  ERMSG receives the
C error message returned by the character function SEMESS.
C
	CHARACTER*113 ERMSG,SEMESS
C
C The contents of the array LINE defines the lines of print that this
C program should produce.
C
	CHARACTER*60 LINE(38)
C
	DATA (LINE(I),I=1,10) /
     +  ' ' ,
     +  'PROGRAM TSETER EXECUTING' ,
     +  ' ' ,
     +  'TSETER - CALL ENTSR TO ENTER RECOVERY MODE' ,
     +  ' ' ,
     +  'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 1' ,
     +  ' ' ,
     +  'TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG' ,
     +  ' ' ,
     +  'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 2' /
C
	DATA (LINE(I),I=11,20) /
     +  ' ' ,
     +  'TSETER - EXECUTE STATEMENT ''IERRO=NERRO(JERRO)''' ,
     +  'TSETER - RESULTING IERRO:             2' ,
     +  'TSETER - RESULTING JERRO:             2' ,
     +  ' ' ,
     +  'TSETER - EXECUTE STATEMENT ''ERMSG=SEMESS()''' ,
     +  'TSETER - RESULTING ERMSG:  ROUTINE_NAME_2 - ERROR_MESSAGE_2' ,
     +  'TSETER - (PRINTING ABOVE LINE ALSO TESTED ICLOEM)' ,
     +  ' ' ,
     +  'TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE' /
C
	DATA (LINE(I),I=21,30) /
     +  'ERROR    2 IN ROUTINE_NAME_2 - ERROR_MESSAGE_2' ,
     +  'TSETER - (AN ERROR MESSAGE SHOULD HAVE BEEN PRINTED)' ,
     +  ' ' ,
     +  'TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG' ,
     +  ' ' ,
     +  'TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE' ,
     +  'TSETER - (NOTHING SHOULD HAVE BEEN PRINTED)' ,
     +  ' ' ,
     +  'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 3' ,
     +  ' ' /
C
	DATA (LINE(I),I=31,38) /
     +  'TSETER - CALL RETSR TO LEAVE RECOVERY MODE - BECAUSE' ,
     +  'TSETER - THE LAST RECOVERABLE ERROR WAS NOT CLEARED,' ,
     +  'TSETER - THIS WILL CAUSE A FATAL-ERROR CALL TO SETER' ,
     +  'ERROR    3 IN SETER - AN UNCLEARED PRIOR ERROR EXISTS' ,
     +  '... MESSAGE FOR UNCLEARED PRIOR ERROR IS AS FOLLOWS:' ,
     +  '... ERROR    5 IN ROUTINE_NAME_3 - ERROR_MESSAGE_3' ,
     +  '... MESSAGE FOR CURRENT CALL TO SETER IS AS FOLLOWS:' ,
     +  '... ERROR    2 IN RETSR - PRIOR ERROR IS NOW UNRECOVERABLE' /
C
C Open GKS.
C
	CALL OPNGKS
C
C Produce a single frame of output, detailing what the program ought to
C print.
C
	CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
	CALL PCSETC ('FC - FUNCTION CODE SIGNAL',CHAR(0))
C
	CALL PCSETI ('FN - FONT NUMBER',26)
C
	CALL PLCHHQ (.5,.975,'SETER TEST "tseter"',.025,0.,0.)
C
	CALL PCSETI ('FN - FONT NUMBER',1)
C
	CALL PLCHHQ (.5,.925,'See the print output; it should consist of
     + the following lines:',.011,0.,0.)
C
	DO 101 I=1,38
	  CALL PLCHHQ (.15,.9-REAL(I-1)*.022,LINE(I),.011,0.,-1.)
  101   CONTINUE
C
C Advance the frame.
C
	CALL FRAME
C
C Close GKS.
C
	CALL CLSGKS
C
C Enter recovery mode.
C
	PRINT * , ' '
	PRINT * , 'PROGRAM TSETER EXECUTING'
	PRINT * , ' '
	PRINT * , 'TSETER - CALL ENTSR TO ENTER RECOVERY MODE'
C
        CALL ENTSR (IROLD,1)
C
C Log a recoverable error.  Nothing should be printed, but the internal
C error flag should be set and the message should be remembered.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 1'
C
        CALL SETER ('ROUTINE_NAME_1 - ERROR_MESSAGE_1',1,1)
C
C Clear the internal error flag.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG'
C
	CALL ERROF
C
C Log another recoverable error.  Again, nothing should be printed, but
C the internal error flag should be set and the message should be
C remembered.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 2'
C
	CALL SETER ('ROUTINE_NAME_2 - ERROR_MESSAGE_2',2,1)
C
C Pick up and print the error flag, as returned in each of two
C ways by the function NERRO.
C
	PRINT * , ' '
	PRINT * , 'TSETER - EXECUTE STATEMENT ''IERRO=NERRO(JERRO)'''
C
	IERRO=NERRO(JERRO)
C
	PRINT * , 'TSETER - RESULTING IERRO:  ',IERRO
	PRINT * , 'TSETER - RESULTING JERRO:  ',JERRO
C
C Pick up and print the error message, as returned by the function
C SEMESS.  This also tests proper functioning of the function ICLOEM.
C
	PRINT * , ' '
	PRINT * , 'TSETER - EXECUTE STATEMENT ''ERMSG=SEMESS()'''
C
	ERMSG=SEMESS()
C
	PRINT * , 'TSETER - RESULTING ERMSG:  ',ERMSG(1:ICLOEM(ERMSG))
	PRINT * , 'TSETER - (PRINTING ABOVE LINE ALSO TESTED ICLOEM)'
C
C Print the current error message.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE'
C
	CALL EPRIN
C
	PRINT * , 'TSETER - (AN ERROR MESSAGE SHOULD HAVE BEEN PRINTED)'
C
C Clear the internal error flag again.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG'
C
	CALL ERROF
C
C Try to print the error message again.  Nothing should be printed.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE'
C
	CALL EPRIN
C
	PRINT * , 'TSETER - (NOTHING SHOULD HAVE BEEN PRINTED)'
C
C Log another recoverable error.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 3'
C
	CALL SETER ('ROUTINE_NAME_3 - ERROR_MESSAGE_3',5,1)
C
C Turn recovery mode off without clearing the internal error flag,
C which should be treated as a fatal error.
C
	PRINT * , ' '
	PRINT * , 'TSETER - CALL RETSR TO LEAVE RECOVERY MODE - BECAUSE'
	PRINT * , 'TSETER - THE LAST RECOVERABLE ERROR WAS NOT CLEARED,'
	PRINT * , 'TSETER - THIS WILL CAUSE A FATAL-ERROR CALL TO SETER'
C
	CALL RETSR (IROLD)
C
C Control should never get to the next statement, but just in case ...
C
	PRINT * , ' '
	PRINT * , 'TSETER - GOT CONTROL BACK - SOMETHING''S WRONG'
C
        STOP
C
      END
