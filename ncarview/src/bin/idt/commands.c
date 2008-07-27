/*
 *	$Id: commands.c,v 1.12 2008-07-27 03:18:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

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
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include "display.h"
#include "commands.h"
#include "talkto.h"


/*
 *	Command
 *	[exported]
 *
 *	Format a command string and send it to the translator. The $format
 *	arg contains one or more newline-separated translator commands and
 *	possibly a single %s format specifier. 
 *	
 *
 *
 * on entry
 *	id		: communication channel
 *	*format		: a printf-style formating string containing the
 *			  actual command string to be sent to the translator.
 *			  If $format contains a '%s' the string referenced by
 *			  $command_data will be substitued for '%s'.
 *
 *		
 *	*command_data	: data to send with the command
 */
void	Command(id, format, command_data)
	int	id;
	char	*format;
	char	*command_data;	/* possibly NULL	*/
{

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
}
