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
