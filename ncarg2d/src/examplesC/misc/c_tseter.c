/*
 *	$Id: c_tseter.c,v 1.1 1994-05-13 14:18:20 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>


/*
 * The contents of the array LINE defines the lines of print that this
 * program should produce.
 */
char *line[40] = {
 " " ,
 "PROGRAM TSETER EXECUTING" ,
 " " ,
 "TSETER - CALL ENTSR TO ENTER RECOVERY MODE" ,
 " " ,
 "TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 1" ,
 " " ,
 "TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG" ,
 " " ,
 "TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 2",
 " " ,
 "TSETER - EXECUTE STATEMENT IERRO=NERRO(JERRO)" ,
 "TSETER - RESULTING IERRO:             2" ,
 "TSETER - RESULTING JERRO:             2" ,
 " " ,
 "TSETER - EXECUTE STATEMENT ERMSG=SEMESS(0)" ,
 "TSETER - RESULTING ERMSG:  ROUTINE_NAME_2 - ERROR_MESSAGE_2" ,
 "TSETER - (PRINTING ABOVE LINE ALSO TESTED ICLOEM)" ,
 " " ,
 "TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE",
 "ERROR    2 IN ROUTINE_NAME_2 - ERROR_MESSAGE_2" ,
 "TSETER - (AN ERROR MESSAGE SHOULD HAVE BEEN PRINTED)" ,
 " " ,
 "TSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG" ,
 " " ,
 "TSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE" ,
 "TSETER - (NOTHING SHOULD HAVE BEEN PRINTED)" ,
 " " ,
 "TSETER - CALL SETER TO REPORT RECOVERABLE ERROR 3" ,
 " ",
 "TSETER - TEST THE USE OF ICFELL" ,
 " " ,
 "TSETER - CALL RETSR TO LEAVE RECOVERY MODE - BECAUSE" ,
 "TSETER - THE LAST RECOVERABLE ERROR WAS NOT CLEARED," ,
 "TSETER - THIS WILL CAUSE A FATAL-ERROR CALL TO SETER" ,
 "ERROR    3 IN SETER - AN UNCLEARED PRIOR ERROR EXISTS" ,
 "... MESSAGE FOR UNCLEARED PRIOR ERROR IS AS FOLLOWS:" ,
 "... ERROR    6 IN SETER/ROUTINE_NAME_3 - ERROR_MESSAGE_3" ,
 "... MESSAGE FOR CURRENT CALL TO SETER IS AS FOLLOWS:" ,
 "... ERROR    2 IN RETSR - PRIOR ERROR IS NOW UNRECOVERABLE"
};

/*
 * This test program demonstrates minimal functioning of the error-
 * handling package used by NCAR Graphics.  It first produces a single
 * frame showing what output print lines to expect and then steps
 * through a simple set of tests that should produce those print lines.
 *
 * Make required character-variable declarations.  ERMSG receives the
 * error message returned by the character function SEMESS.
 */

main()
{
	char ermsg[114],semess[114];
	int irold, i, ierro, jerro;
/*
 * Open GKS.
 */
	c_opngks();
/*
 * Produce a single frame of output, detailing what the program ought to
 * print.
 */
	c_set    (0.,1.,0.,1.,0.,1.,0.,1.,1);

	c_pcsetc ("FC - FUNCTION CODE SIGNAL","\0");

	c_pcseti ("FN - FONT NUMBER",26);

	c_plchhq (.5,.975,"SETER TEST tseter",.025,0.,0.);

	c_pcseti ("FN - FONT NUMBER",1);

	c_plchhq (.5,.925,"See the print output; it should consist of the following lines:",.011,0.,0.);

	for( i = 0; i < 40; i++ ) { 
		c_plchhq (.15,.9-(float)i*.022,line[i],.011,0.,-1.);
	}
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	c_clsgks();
/*
 * Enter recovery mode.
 */
	printf( "\nPROGRAM TSETER EXECUTING\n" );
	printf ( "\nTSETER - CALL ENTSR TO ENTER RECOVERY MODE\n" );

	c_entsr (&irold,1);
/*
 * Log a recoverable error.  Nothing should be printed, but the internal
 * error flag should be set and the message should be remembered.
 */
	printf( "\nTSETER - CALL SETER TO REPORT RECOVERABLE ERROR 1\n" );

	c_seter ("ROUTINE_NAME_1 - ERROR_MESSAGE_1",1,1);
/*
 * Clear the internal error flag.
 */
	printf( "\nTSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG\n");

	c_errof();
/*
 * Log another recoverable error.  Again, nothing should be printed, but
 * the internal error flag should be set and the message should be
 * remembered.
 */
	printf( "\nTSETER - CALL SETER TO REPORT RECOVERABLE ERROR 2\n" );

	c_seter ("ROUTINE_NAME_2 - ERROR_MESSAGE_2",2,1);
/*
 * Pick up and print the error flag, as returned in each of two
 * ways by the function NERRO.
 */
	printf ("\nTSETER - EXECUTE STATEMENT IERRO=NERRO(JERRO)\n" );

	ierro = c_nerro(&jerro);

	printf( "\nTSETER - RESULTING IERRO: %d",ierro );
	printf( "\nTSETER - RESULTING JERRO: %d\n",jerro );
/*
 * Pick up and print the error message, as returned by the function
 * SEMESS.  This also tests proper functioning of the function ICLOEM.
 */
	printf ("\nTSETER - EXECUTE STATEMENT ERMSG=SEMESS(0)\n" );

	strcpy(ermsg,c_semess(0) );

	printf ("TSETER - RESULTING ERMSG:  %s\n",ermsg);
	printf( "TSETER - (PRINTING ABOVE LINE ALSO TESTED ICLOEM)\n" );
/*
 * Print the current error message.
 */
	printf( "\nTSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE\n" );

	c_eprin();

	printf( "TSETER - (AN ERROR MESSAGE SHOULD HAVE BEEN PRINTED)\n" );
/*
 * Clear the internal error flag again.
 */
	printf( "\nTSETER - CALL ERROF TO TURN OFF INTERNAL ERROR FLAG\n" );

	c_errof();
/*
 * Try to print the error message again.  Nothing should be printed. );
 */
	printf( "\nTSETER - CALL EPRIN TO PRINT CURRENT ERROR MESSAGE\n" );

	c_eprin();

	printf( "TSETER - (NOTHING SHOULD HAVE BEEN PRINTED)\n" );
/*
 * Log another recoverable error.
 */
	printf( "\nTSETER - CALL SETER TO REPORT RECOVERABLE ERROR 3\n" );

	c_seter ("ROUTINE_NAME_3 - ERROR_MESSAGE_3",5,1);
/*
 * Test the use of ICFELL.
 */
	printf( "\nTSETER - TEST THE USE OF ICFELL\n" );

	if (c_icfell("TSETER",6) != 5) {
		printf( " " );
		printf( "TSETER - ICFELL MALFUNCTIONED - SOMETHING'S WRONG\n" );
		exit(1);
	}
/*
 * Turn recovery mode off without clearing the internal error flag,
 * which should be treated as a fatal error.
 */
	printf( "\nTSETER - CALL RETSR TO LEAVE RECOVERY MODE - BECAUSE\n" );
	printf( "TSETER - THE LAST RECOVERABLE ERROR WAS NOT CLEARED,\n" );
	printf( "TSETER - THIS WILL CAUSE A FATAL-ERROR CALL TO SETER\n" );

	c_retsr (irold);
/*
 * Control should never get to the next statement, but just in case ...
 */
	printf( " " );
	printf( "TSETER - GOT CONTROL BACK - SOMETHING'S WRONG\n" );
}

