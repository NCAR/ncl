/*
 *	$Id: commands.c,v 1.5 1992-04-03 23:20:39 clyne Exp $
 */
/*
 *	commands.c
 *
 *	Author		John Clyne
 *
 *	Date		Thu Sep 13 12:48:22 MDT 1990
 *
 *	This module is resonpsible for providing generating a syntatically
 *	correct ictrans command and sending it to the appropriate translator.
 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>

#ifdef	SYSV
#include <string.h>
#else
#include <strings.h>
#endif

#include "display.h"
#include "commands.h"
#include "talkto.h"

extern	char	*TalkTo();
extern	void	SetValues();

/*
 *	Command1
 *	[exported]
 *	
 *	A type 1 command has no data.
 *
 * on entry
 *	command		: name of the idt command to map into an ictrans 
 *			  command and the id of the translator to receive the
 *			  command.
 */
void	Command1(command)
	caddr_t	command;
{
	Command_Id	*comm_id = (Command_Id *) command;

	int	id = comm_id->id;/* id of the command recipient		*/

	char	buf[80];

	/*
	 * Build the ictrans command.
 	 */
	switch	((int) comm_id->command) {

	case LOOP: 
		(void) strcpy(buf, LOOP_STRING);
		(void) strcat(buf, "\n");
		break;

	case PLAYBACK: 
		(void) strcpy(buf, PLAYBACK_STRING);
		(void) strcat(buf, "\n");
		break;

	case JOGBACK: 
		(void) strcpy(buf, JOGBACK_STRING);
		(void) strcat(buf, "\n");
		break;

	case REDRAW: 
		(void) strcpy(buf, REDRAW_STRING);
		(void) strcat(buf, "\n");
		break;

	case STOP: 
		SignalTo(id, STOP_SIGNAL);
		return;
		break;

	case JOG: 
		(void) strcpy(buf, JOG_STRING);
		(void) strcat(buf, "\n");
		break;

	case PLAY: 
		(void) strcpy(buf, PLAY_STRING);
		(void) strcat(buf, "\n");
		break;

	case DONE: 
		(void) strcpy(buf, DONE_STRING);
		(void) strcat(buf, "\n");
		break;

	}

	/*
	 * send the command to the appopriate recipient
	 */
	(void) TalkTo(id, buf, ASYNC);
}
	
/*
 *	Command2
 *	[exported]
 *
 *	Generate a ictrans command with data based on the input to 
 *	this routine. The data is converted to type int. Dispose that 
 *	command to idt's communication module. The command is guaranteed 
 *	to be syntactically correct but not not necessarily valid in context.
 *
 * on entry
 *	command		: name of the idt command to map into an ictrans 
 *			  command and the id of the translator to receive the
 *			  command.
 *	*command_data	: data to send with the command
 */
void	Command2(command, command_data)
	caddr_t	command;
	char	*command_data;
{
	Command_Id	*comm_id = (Command_Id *) command;

	int	id = comm_id->id;/* id of the command recipient		*/
	char	*value;		/* command_data represented as an int	*/
	char	buf[80];	/* the ictrans complete command string	*/

	value = command_data;

	/*
	 * Build the ictrans command. Some commands are of the form
	 * "command data" and some are of the form "data command"
 	 */
	switch	((int) comm_id->command) {

	case DUP: 
		(void) strcpy(buf, DUP_STRING);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, "\n");

		break;

	case GOTO: 
		(void) strcpy(buf, command_data);
		(void) strcat(buf, " ");
		(void) strcat(buf, GOTO_STRING);
		(void) strcat(buf, "\n");

		break;

	case SKIP: 
		(void) strcpy(buf, SKIP_STRING);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, "\n");

		break;

	case START_SEGMENT: 
		(void) strcpy(buf, command_data);
		(void) strcat(buf, " ");
		(void) strcat(buf, START_SEGMENT_STRING);
		(void) strcat(buf, "\n");

		break;

	case STOP_SEGMENT: 
		(void) strcpy(buf, command_data);
		(void) strcat(buf, " ");
		(void) strcat(buf, STOP_SEGMENT_STRING);
		(void) strcat(buf, "\n");

		break;

	case SET_WINDOW: 
		(void) strcpy(buf, SET_WINDOW_STRING);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, "\n");

		break;

	case SAVE: 
		(void) strcpy(buf, SAVE_STRING);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, "\n");

		break;

	case ZOOM: 
		(void) strcpy(buf, ZOOM_STRING1);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, ZOOM_STRING2);
		(void) strcat(buf, "\n");

		/*
		 * this is a hack to register the coords selected with
		 * the zoom command with the set_window command.
		 */
		comm_id->command = SET_WINDOW;

		break;

	}

	/*
	 * store the value of the data for later reference
	 */
	SetValues(id, comm_id->command, value); 

	/*
	 * send the command to the appopriate recipient
	 */
	(void) TalkTo(id, buf, ASYNC);
}
	
/*
 *	Command3
 *	[exported]
 *	
 *	A type 3 command has data of type char *. The data is sent to the
 *	translator as is.
 *
 *
 * on entry
 *	command		: name of the idt command to map into an ictrans 
 *			  command and the id of the translator to receive the
 *			  command.
 *	*command_data	: data to send with the command
 */
void	Command3(command, command_data)
	caddr_t	command;
	char	*command_data;
{
	Command_Id	*comm_id = (Command_Id *) command;

	int	id = comm_id->id;/* id of the command recipient		*/
	char	buf[80];	/* the ictrans complete command string	*/


	/*
	 * Build the ictrans command. Some commands are of the form
	 * "command data" and some are of the form "data command"
 	 */
	switch	((int) comm_id->command) {


	case LIST: 	/* not used	*/
		(void) strcpy(buf, command_data);
		(void) strcat(buf, " ");
		(void) strcat(buf, LIST_STRING);
		(void) strcat(buf, "\n");

		break;

	case PRINT: 
		(void) strcpy(buf, PRINT_STRING1);
		(void) strcat(buf, " ");
		(void) strcat(buf, command_data);
		(void) strcat(buf, PRINT_STRING2);
		(void) strcat(buf, "\n");

		break;
	}

	/*
	 * send the command to the appopriate recipient
	 */
	(void) TalkTo(id, buf, ASYNC);
}
