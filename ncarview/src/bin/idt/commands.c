/*
 *	$Id: commands.c,v 1.7 1992-08-12 21:41:42 clyne Exp $
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
#include <string.h>
#include "display.h"
#include "commands.h"
#include "talkto.h"

extern	char	*TalkTo();
extern	void	SetValues();

/*
 *	Command
 *	[exported]
 *
 *	Format a command string and set it to the translator. The $format
 *	arg contains one or more newline-separated translator commands and
 *	possibly a single %s format specifier. 
 *	
 *
 *
 * on entry
 *	command		: name of the idt command to map into an ictrans 
 *			  command and the id of the translator to receive the
 *			  command.
 *	*format		: a printf-style formating string containing the
 *			  actual command string to be sent to the translator.
 *			  If $format contains a '%s' the string referenced by
 *			  $command_data will be substitued for '%s'.
 *
 *		
 *	*command_data	: data to send with the command
 */
void	Command(command, format, command_data)
	caddr_t	command;
	char	*format;
	char	*command_data;	/* possibly NULL	*/
{
	Command_Id	*comm_id = (Command_Id *) command;

	int	id = comm_id->id;	/* id of the command recipient	*/
	char	buf[1024];		/* formatted command string(s)	*/
	char	cmd_buf[1024];	/* a single translator command string	*/
	char	*s;

	sprintf(buf, format, command_data);

	/*
	 * break the string of possibly multiple translator commands into
	 * multiple strings of single commands
	 */
	s = strtok(buf, "\n");
	if (s) {
		sprintf(cmd_buf, "%s\n", s);
		(void) TalkTo(id, cmd_buf, ASYNC);
	}

	for (s=strtok(NULL, "\n"); s; s=strtok(NULL, "\n")) {
		sprintf(cmd_buf, "%s\n", s);
		(void) TalkTo(id, cmd_buf, ASYNC);
	}
	/*
	 * store the value of the data for later reference
	 */
	SetValues(id, comm_id->command, command_data); 
}
