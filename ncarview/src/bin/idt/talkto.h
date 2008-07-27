/* 
 * $Id: talkto.h,v 1.5 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_talkto_
#define	_talkto_

#define	SYNC	0
#define	ASYNC	1

#define	PROMPT	"ictrans> "

#define	HISTORY
#define	HISTORY_FILE	"./.idthist"



int	OpenTranslator(
#ifdef	NeedFuncProto
	int	channel,
	char	**argv,
	int	hfd
#endif
);

void	CloseTranslator(
#ifdef	NeedFuncProto
	int	channel
#endif
);

char	*TalkTo(
#ifdef	NeedFuncProto
	int	id,
	char	*command_string,
	int	mode
#endif
);

void	SignalTo(
#ifdef	NeedFuncProto
	int	id,
	int	signal
#endif
);

void	Message(
#ifdef	NeedFuncProto
	int	id,
	char	*s
#endif
);

void	ErrorMessage(
#ifdef	NeedFuncProto
	int	id,
	char	*s
#endif
);

#endif
