/*
 *	$Id: hplj.h,v 1.4 1993-01-19 16:35:29 clyne Exp $
 */
#ifndef	_hplj_
#define	_hplh_

#define	HPLJ_RESET	"E"
#define	HPLJ_PORTRAIT	"&l0O"
#define	HPLJ_LANDSCAPE	"&l1O"
#define	HPLJ_ENCODING	""
#define	HPLJ_START	"*r1A"
#define	HPLJ_END	"*rB"
#define	HPLJ_EJECT	"&l0H"
#define	HPLJ_POSITION	"*p%dx%dY"
#define	HPLJ_TRANSFER	"*b%dW"
#define	HPLJ_RESOLUTION	"*t%dR"


#define	HPLJ_PAPER_WIDTH	8.5
#define	HPLJ_PAPER_HEIGHT	11.0
#define	HPLJ_MAX_RES		300	/* default resolution 300dpi	*/

typedef	struct	HPLJ_Info	{
	int		do_compress;
	int		orientation;		
	int		dpi;
	char		*trans_data;
	int		row_size;
	int		image_size;
	int		start_x;
	int		start_y;
	} HPLJ_Info;

#endif	/* _hplj_ */
