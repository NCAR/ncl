/*
 *	$Id: glob.h,v 1.1 1994-03-09 01:04:38 clyne Exp $
 */

#ifndef	_glob_h_
#define	_glob_h_

extern	void	glob(
#ifdef	NeedFuncProto
	const char	*s,
	char	***r_argv,
	int	*r_argc
#endif
);

#endif	/* _glob_h_	*/
