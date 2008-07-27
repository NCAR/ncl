/*
 *      $Id: animate.h,v 1.6 2008-07-27 03:22:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		animate.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 14 09:34:00 MDT 1992
 *
 *	Description:	interface to animate.c
 */
#ifndef	_animate_
#define	_animate_

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <ncarg/c.h>

typedef	struct	AnimatePixmapStruct_	{
	Pixmap				pixmap;
	Bool				pixmap_created;
	int				whoami;
	struct	AnimatePixmapStruct_	*next;
	struct	AnimatePixmapStruct_	*prev;
	} AnimatePixmap_;
	

typedef	struct	AnimatePixmapsStruct {
	AnimatePixmap_	*ap_, 
			*current;
	Display		*dpy;
	Widget		canvas;		/* canvas to animate in		*/
	Window		win;		/* window to animate in		*/
	int		size;		/* number of images in ap_	*/
	int		src_x,		/* src X passed to XPutImage	*/
			src_y,		/* src Y passed to XPutImage	*/
			dst_x,		/* dst X passed to XPutImage	*/
			dst_y,		/* dst y passed to XPutImage	*/
			width,		/* src width passed to XPutImage*/
			height;		/* src height passed to XPutImage*/
	int		(*ap)();
	int		(*not_ap)();
	int		(*load)();
	int		(*display)();
	int		(*current_image)();
	void		(*display_next)();
	void		(*display_prev)();
	} AnimatePixmaps;

typedef	struct	AnimateStruct	{
	int	num_frames;	/* number of images in $images		*/
	Bool	do_loop;	/* loop animations			*/
	Bool	do_stop;	/* stop an animation			*/
	Bool	is_animate;	/* true when in continous animation	*/
	unsigned int	delay;	/* delay between frames in 1/100 sec	*/
	XtAppContext	app;
	AnimatePixmaps	ap;	/* the pixmaps to animate		*/
} AnimateType;


extern	AnimateType	*AnimateOpen(
#ifdef	NeedFuncProto
	Display		*dpy,
	Widget		canvas,
	int		n,
        int     	src_x,
	int		src_y,
	int		dst_x,
	int		dst_y,
	unsigned int	width,
	unsigned int	height
#endif
);

extern	int	AnimateLoad(
#ifdef	NeedFuncProto
	AnimateType	*a,
	XImage          *ximage,
	unsigned        index
#endif
);

extern	int	AnimateDisplayImage(
#ifdef	NeedFuncProto
	AnimateType		*a,
	unsigned int	number
#endif
);

extern	void	AnimateDisplayNext(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

extern	void	AnimateDisplayPrev(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

extern	void	AnimateDispContForward(
#ifdef	NeedFuncProto
	AnimateType     *a,
	void            (*update_func)(),
	Voidptr         data
#endif
);

extern	void	AnimateDispContReverse(
#ifdef	NeedFuncProto
	AnimateType     *a,
	void            (*update_func)(),
	Voidptr         data
#endif
);

extern	void	AnimateStop(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

extern	void	AnimateLoop(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

extern	int	AnimateGetImageNum(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

extern	void	AnimateSetDelay(
#ifdef	NeedFuncProto
	AnimateType	*a,
	unsigned int	time
#endif
);

extern	int	AnimateClose(
#ifdef	NeedFuncProto
	AnimateType	*a
#endif
);

#endif	/* _animate_	*/
