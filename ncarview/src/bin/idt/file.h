#ifndef	_file_
#define	_file_

#include "idt.h"

extern	char	*GetFiles(
#ifdef	NeedFuncProto
	char	*file_filter,
	int	*longest
#endif
);

extern	void SetFileSelection(
#ifdef	NeedFuncProto
	char	*file,
	FuncPtrPasser	select_action
#endif
);
	
extern	char	*GetFileSelection();

#endif	/*	_file_	*/
