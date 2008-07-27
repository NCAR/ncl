/*
 *	$Id: get_cmd.c,v 1.13 2008-07-27 03:18:45 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

/*
 *	get_cmd.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Apr 13 13:34:33 MDT 1990
 *
 *	Fetch the next command to process.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ncarg/c.h>
#include "lex.h"
#include "ictrans.h"
#include "get_cmd.h"

extern	int	my_yylex();
extern	char	yytext[];

/*
 *	sym_to_literal
 *	[internal]
 *
 *	This funtion does the parsing for conv_frame and conv_frame_list
 */
static	int sym_to_literal(s, current_frame, last_frame)
	char	*s;
	int	current_frame,
		last_frame;
{

	int	literal;

	/*
	 * look for {[ D | . | $ ]( + | - ) D} | D | . | $ 
	 * where D is an unsigned int
	 */
	literal = current_frame;	/* the default	*/

	if (isdigit(*s)) {
		literal = atoi(s);
		while(isdigit(*s))
			s++;
	}
	else if (*s == '.') {
		literal = current_frame;
		s++;
	}
	else if (*s == '$') {
		literal = last_frame;
		s++;
	}

	switch ((int) *s) {	/* relative address	*/	
	
	case	'+':
		s++;
		literal += atoi(s);
		break;
	case	'-':
		s++;
		literal -= atoi(s);
		break;
	default:	/* no relative address	*/
		break;
	}

	return(literal);
}

/*
 *	conv_frame
 *	[internal]
 *
 *	convert a string containing a single frame address specification 
 *	into its integer value. The string contains now spaces and is
 *	of the form: 
 *
 *		[ D|.|$ ][ (+|-)D ] 	
 *		D ::= (0-9)+
 * on entry
 *	*s		: contains the address string
 *	last_frame	: is the last valid frame number
 *	current_frame	: is the current frame number
 * on exit
 *	*frames		: contains the converted frame
 *	return		: 0 => success, else failure
 */
static	int	conv_frame(s, frames, last_frame, current_frame)
	char	*s;
	FrameList	*frames;
	int	last_frame,
		current_frame;
{

	int	start_frame;
	
	/*
	 * make sure have enough memory
	 */
	if (frames->num == frames->num_alloced) {
		frames->fc = (FrameCount *) realloc (
			(char *) frames->fc, 
			(unsigned) ((frames->num_alloced + SMALL_MALLOC_BLOCK) 
			* sizeof (FrameCount))
		);
		if (! frames->fc) {
			return(-1);
		}

		frames->num_alloced += SMALL_MALLOC_BLOCK;
	}

	/* 
	 *	convert symbolic frame address string to an integer
	 */
	start_frame = sym_to_literal(s, current_frame, last_frame);

	if (start_frame < 1 || start_frame > last_frame)
		return(-1);

	frames->fc[frames->num].start_frame =  start_frame;
	frames->fc[frames->num].num_frames = 1;
	frames->num++;

	return(0);
}


/*
 *	conv_frame_list
 *	[internal]
 *
 *	convert a string containing a pair of comma seperated  frame address 
 *	specifications into a pair of integer values. The string contains no 
 *	spaces and is *	of the form: 
 *
 *		[ D|.|$ ][ (+|-)D ] , [ D|.|$ ][ (+|-)D ] 
 *		D ::= (0-9)+
 * on entry
 *	*s		: contains the address string
 *	last_frame	: is the last valid frame number
 *	current_frame	: is the current frame number
 * on exit
 *	*frames		: contains the converted frame
 *	return		: 0 => success, else failure
 */
static	int	conv_frame_list(s, frames, last_frame, current_frame)
	char	*s;
	FrameList	*frames;
	int	last_frame,
		current_frame;
{


	int	start_frame,
		stop_frame;
	/*
	 * make sure have enough memory
	 */
	if (frames->num == frames->num_alloced) {
		frames->fc = (FrameCount *) realloc (
			(Voidptr) frames->fc, 
			(unsigned) ((frames->num_alloced + SMALL_MALLOC_BLOCK) 
			* sizeof (FrameCount))
		);
		if (! frames->fc) {
			return(-1);
		}

		frames->num_alloced += SMALL_MALLOC_BLOCK;
	}

	start_frame = sym_to_literal(s, current_frame, last_frame);

	s = strchr(s, ',');	/* skip past the ','	*/
	s++;

	stop_frame = sym_to_literal(s, current_frame, last_frame);

	if (start_frame < 1 || stop_frame > last_frame)
		return(-1);

	frames->fc[frames->num].start_frame = start_frame;
	frames->fc[frames->num].num_frames = stop_frame - start_frame;

	if (frames->fc[frames->num].num_frames < 0)
		frames->fc[frames->num].num_frames -= 1;
	else
		frames->fc[frames->num].num_frames += 1;

	frames->num++;

	return(0);
	
}






static	void	conv_name(s, name)
	char	*s;
	char	**name;
{
	static	char buf[MAX_LINE_LEN];

	if (strlen(s) >= MAX_LINE_LEN) {
		(void) fprintf(stderr, "Command name too long: %s\n", s);
		(void) strcpy(buf, "noop");
	}
	else 
		(void) strcpy(buf, s);

	*name = buf;	
}
static	void	conv_data(s, data)
	char	*s;
	char	**data;
{
	static	char buf[MAX_LINE_LEN];
	char	*t;

	if (strlen(s) >= MAX_LINE_LEN) {
		(void) fprintf(stderr, "Command data too long: %s\n", s);
		(void) strcpy(buf, "");
	}
	else  {
		/* remove trailing white space	*/
		t = s + strlen(s) - 1;
		while (isspace(*t) && t != s) t--;
		*(++t) = '\0';

		(void) strcpy(buf, s);
	}

	*data = buf;	
}

/*
 *	init_icommand
 *
 *	initialize an ICommand struct. provide initial memory allocation.
 */
init_icommand(icommand)
	ICommand	*icommand;
{
	icommand->last_frame = 0;
	icommand->current_frame = 0;

	icommand->cmd.name = "Noop";
	icommand->cmd.data = NULL;

	icommand->cmd.dst_frames.num = 0;
	icommand->cmd.dst_frames.fc = (FrameCount *) malloc(
		SMALL_MALLOC_BLOCK * sizeof (FrameCount)
	);
	if (! icommand->cmd.dst_frames.fc) {
		perror("malloc()");
		return(-1);
	}
	icommand->cmd.dst_frames.num_alloced = SMALL_MALLOC_BLOCK;

	icommand->cmd.src_frames.num = 0;
	icommand->cmd.src_frames.fc = (FrameCount *) malloc(
		SMALL_MALLOC_BLOCK * sizeof (FrameCount)
	);
	if (! icommand->cmd.src_frames.fc) {
		perror("malloc()");
		return(-1);
	}
	icommand->cmd.src_frames.num_alloced = SMALL_MALLOC_BLOCK;

	return(1);
}


/*
 *	get_command()
 *	[exported]
 *
 *	get the next user command and its data.  The command returned
 *	may not be valid. All that is guaranteed is that the command
 *	and its data were syntatically correct. 
 *	The format of the command is: 
 *
 *		[frames] [command name] [(frames | data)]
 *
 *	where 'frames' is a list of frames for command 'command name'
 *	to operate on.
 *
 * on entry
 *	*ic		: has been initialized with init_icommand
 * on exit
 *	*ic		: contains a potential command and its data
 *	return		: one of (GET_EOLN, GET_EOF, GET_SYNTAX, 
 *			  GET_OUTOFRANGE, GET_SYMBOL)
 */
get_command(ic)
	ICommand	*ic;
{
	int	type;

	char	*prompt = "ictrans> ";

	short	loop = 1;
	int	status = 0;	/* error status	*/

	ic->cmd.name = "Noop";
	ic->cmd.data = NULL;
	ic->cmd.src_frames.num = 0;
	ic->cmd.dst_frames.num = 0;

	if (! ic->batch || ic->force_prompt) {
		(void) fprintf (ic->fp, "%s", prompt);
	}
	type = my_yylex();	/* get a token from data stream	*/
	while(loop) {	/* parse source address	*/
		switch (type) {

		case FRAME_TYPE:
			status |= conv_frame(yytext, &ic->cmd.src_frames,
				ic->last_frame, ic->current_frame);

#ifdef	DEBUG
			(void) fprintf(stderr, "src frame %d \n", 
				ic->cmd.src_frames.fc[ic->cmd.src_frames.num-1]
				.start_frame);
#endif
			type = my_yylex();	/* get next token	*/
			break;
		case FRAME_LIST_TYPE:
			status |= conv_frame_list(yytext, &ic->cmd.src_frames,
				ic->last_frame, ic->current_frame);
#ifdef	DEBUG
			(void) fprintf(stderr, "src frame %d plus %d\n", 
				ic->cmd.src_frames.fc[ic->cmd.src_frames.num-1]
				.start_frame,
				ic->cmd.src_frames.fc[ic->cmd.src_frames.num-1]
				.num_frames);
#endif
			type = my_yylex();	/* get next token	*/
			break;

		default:
			loop = 0;
			break;

		}
	}

	switch (type) {	/* parse command name	*/
		case COMMAND_TYPE:
			conv_name(yytext, &ic->cmd.name);

#ifdef	DEBUG
			(void) fprintf(stderr, "Command is %s\n", yytext);
#endif
			type = my_yylex();
			break;

		default:
			break;
	}

	if (type == DATA_TYPE) {	/* parse data 	*/
		conv_data(yytext, &ic->cmd.data);
#ifdef	DEBUG
		(void) fprintf(stderr, "data: %s\n", yytext);
#endif
		type = my_yylex();
	}
	else {	/* look for a set of destination frames	*/
	
		loop = 1;
		while (loop) {	/* parse destination address	*/
		switch (type) {

		case FRAME_TYPE:
			status |= conv_frame(yytext, &ic->cmd.dst_frames,
				ic->last_frame, ic->current_frame);
#ifdef	DEBUG
			(void) fprintf(stderr, "src frame %d \n", 
				ic->cmd.dst_frames.fc[ic->cmd.dst_frames.num-1]
				.start_frame);
#endif
			type = my_yylex();
			break;
		case FRAME_LIST_TYPE:
			status |= conv_frame_list(yytext, &ic->cmd.dst_frames,
				ic->last_frame, ic->current_frame);
#ifdef	DEBUG
			(void) fprintf(stderr, "dst frame %d plus %d\n", 
				ic->cmd.dst_frames.fc[ic->cmd.dst_frames.num-1]
				.start_frame,
				ic->cmd.dst_frames.fc[ic->cmd.dst_frames.num-1]
				.num_frames);
#endif
			type = my_yylex();
			break;

		default:
			loop = 0;
			break;

		}
	}	/* while	*/
	}	/* else	*/

	/*
	 * if an address was specified but no command was given give
	 * the default command
	 */
	if (ic->cmd.name == (char *) NULL && ic->cmd.src_frames.num != 0) {
		ic->cmd.name = DEFAULT_COMMAND;
	}

	/*
	 * command termination
	 */
	switch (type)  {
		case END_LINE:	/* normal termination	*/
			if (status != 0) {
				return(GET_OUTOFRANGE);
			}
			return(GET_EOLN);
		case ERROR_TYPE:
			return(GET_SYMBOL);
		case END_FILE:
			return(GET_EOF);
		default:
			return(GET_SYNTAX);
	}
	
}

