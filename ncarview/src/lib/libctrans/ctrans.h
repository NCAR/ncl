
/*
 *      $Id: ctrans.h,v 1.1 1992-07-16 18:07:21 clyne Exp $
 */
/*
 *	File:		ctrans.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 10 11:22:23 MDT 1992
 *
 *	Description:	header file for ctrans.c
 */

#include <cgm_tools.h>
#include <ncarv.h>

#ifndef	_ctrans_
#define	_ctrans_

typedef	enum	{
	FATAL = -2,
	WARN = -1,
	EOM = 0,
	OK = 1
	} CtransRC;


#define	EOM	0	/* End of Metafile	*/

extern	CtransRC	init_ctrans(
#ifdef	NeedFuncProto
	int*,		/* argc		*/
	char**,		/* argv		*/ 
	char*,		/* prog_name	*/
	char*,		/* gcap		*/
	char*,		/* fcap		*/
	boolean,	/* stand_alone	*/
	boolean	/* batch	*/
#endif
);




extern	CtransRC	init_metafile(
#ifdef	NeedFuncProto
	int,		/* record,	*/
	Cgm_fd		/*  cgm_fd	*/
#endif
);

extern	void	close_metafile(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	ctrans(
#ifdef	NeedFuncProto
	int		/* record	*/)
#endif
);

extern	CtransRC	ctrans_merge(
#ifdef	NeedFuncProto
	int,		/* record1	*/ 	
	int		/* record2	*/
#endif
);

extern	CtransRC	SetDevice(
#ifdef	NeedFuncProto
	char*		/* gcap		*/
#endif
);

extern	CtransRC	SetFont(
#ifdef	NeedFuncProto
	char*		/* fcap		*/
#endif
);

extern	void	SetDefaultPalette(
#ifdef	NeedFuncProto
	char*		/* pal_fname	*/
#endif
);

extern	void	GraphicsMode(
#ifdef	NeedFuncProto
	boolean		/* on	*/
#endif
);

extern	void	close_ctrans(
#ifdef	NeedFuncProto
#endif
);

extern	char	*getGcapname(
#ifdef	NeedFuncProto
	char*		/* gcap		*/
#endif
);

extern	char	*getFcapname(
#ifdef	NeedFuncProto
	char*		/* fcap		*/
#endif
);

extern	boolean	IsOutputToTty(
#ifdef	NeedFuncProto
#endif
);

extern	char	*ReadCtransMsg(
#ifdef	NeedFuncProto
#endif
);

extern	void	CtransClear(
#ifdef	NeedFuncProto
#endif
);

/*
**
**	yet another ctrans programming interface. 
**
*/



extern	CtransRC	CtransOpenBatch(
#ifdef	NeedFuncProto
	char*,		/* device_name	*/
	char*,		/* font_name	*/
	char*,		/* metafile	*/
	int,		/* dev_argc	*/
	char**		/* dev_argv	*/
#endif
);

extern	CtransRC	CtransSetMetafile(
#ifdef	NeedFuncProto
	char*,		/* metafile	*/
#endif
);

extern	CtransRC	CtransPlotFrame(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	CtransClearDisplay(
#ifdef	NeedFuncProto
#endif
);

void	CtransCloseBatch(
#ifdef	NeedFuncProto
#endif
);

extern	CtransRC	CtransLSeekBatch(
#ifdef	NeedFuncProto
	unsigned,	/* offset	*/
	unsigned	/* whence	*/
#endif
);

void	CtransGraphicsMode(
#ifdef	NeedFuncProto
	boolean		/* on	*/
#endif
);

CtransGetErrorNumber(
#ifdef	NeedFuncProto
#endif
);


char	*CtransGetErrorMessage(
#ifdef	NeedFuncProto
#endif
);

#endif
