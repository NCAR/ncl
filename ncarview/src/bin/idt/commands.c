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
#include "display.h"
#include "commands.h"
#include "talkto.h"

extern	void	TalkTo();
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
		strcpy(buf, LOOP_STRING);
		strcat(buf, "\n");
		break;

	case PLAYBACK: 
		strcpy(buf, PLAYBACK_STRING);
		strcat(buf, "\n");
		break;

	case JOGBACK: 
		strcpy(buf, JOGBACK_STRING);
		strcat(buf, "\n");
		break;

	case REDRAW: 
		strcpy(buf, REDRAW_STRING);
		strcat(buf, "\n");
		break;

	case STOP: 
		SignalTo(id, STOP_SIGNAL);
		return;
		break;

	case JOG: 
		strcpy(buf, JOG_STRING);
		strcat(buf, "\n");
		break;

	case PLAY: 
		strcpy(buf, PLAY_STRING);
		strcat(buf, "\n");
		break;

	case DONE: 
		strcpy(buf, DONE_STRING);
		strcat(buf, "\n");
		break;

	}

	/*
	 * send the command to the appopriate recipient
	 */
	TalkTo(id, buf, ASYNC);
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
	int	value;		/* command_data represented as an int	*/
	char	buf[80];	/* the ictrans complete command string	*/

	value = atoi (command_data);

	/*
	 * Build the ictrans command. Some commands are of the form
	 * "command data" and some are of the form "data command"
 	 */
	switch	((int) comm_id->command) {

	case DUP: 
		strcpy(buf, DUP_STRING);
		strcat(buf, " ");
		strcat(buf, command_data);
		strcat(buf, "\n");

		break;

	case GOTO: 
		strcpy(buf, command_data);
		strcat(buf, " ");
		strcat(buf, GOTO_STRING);
		strcat(buf, "\n");

		break;

	case SKIP: 
		strcpy(buf, SKIP_STRING);
		strcat(buf, " ");
		strcat(buf, command_data);
		strcat(buf, "\n");

		break;

	case START_SEGMENT: 
		strcpy(buf, command_data);
		strcat(buf, " ");
		strcat(buf, START_SEGMENT_STRING);
		strcat(buf, "\n");

		break;

	case STOP_SEGMENT: 
		strcpy(buf, command_data);
		strcat(buf, " ");
		strcat(buf, STOP_SEGMENT_STRING);
		strcat(buf, "\n");

		break;

	}

	/*
	 * store the value of the data for later reference
	 */
	SetValues(id, comm_id->command, value); 

	/*
	 * send the command to the appopriate recipient
	 */
	TalkTo(id, buf, ASYNC);
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

	case ZOOM: 
		strcpy(buf, ZOOM_STRING1);
		strcat(buf, " ");
		strcat(buf, command_data);
		strcat(buf, ZOOM_STRING2);
		strcat(buf, "\n");

		break;

	case SAVE: 
		strcpy(buf, SAVE_STRING);
		strcat(buf, " ");
		strcat(buf, command_data);
		strcat(buf, "\n");

		break;
	case LIST: 	/* not used	*/
		strcpy(buf, command_data);
		strcat(buf, " ");
		strcat(buf, LIST_STRING);
		strcat(buf, "\n");

		break;

	case PRINT: 
		strcpy(buf, PRINT_STRING1);
		strcat(buf, " ");
		strcat(buf, command_data);
		strcat(buf, PRINT_STRING2);
		strcat(buf, "\n");

		break;
	}

	/*
	 * send the command to the appopriate recipient
	 */
	TalkTo(id, buf, ASYNC);
}
