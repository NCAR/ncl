/*
 *	$Id: hplj.h,v 1.2 1991-08-16 11:14:02 clyne Exp $
 */
#ifndef	_hplj_
#define	_hplh_

#define	HPLJ_RESET	"E"
#define	HPLJ_PORTRAIT	"&l00"
#define	HPLJ_LANDSCAPE	"&l10"
#define	HPLJ_ENCODING	""
#define	HPLJ_START	"*r1A"
#define	HPLJ_END	"*rB"
#define	HPLJ_EJECT	"&l0H"


#define	HPLJ_PAPER_WIDTH	8.5
#define	HPLJ_PAPER_HEIGHT	11.0
#define	HPLJ_MAX_RES		300	/* default resolution 300dpi	*/

typedef	struct	HPLJ_Ras_	{
	char	*reset;
	char	*orientation;
	char	*encoding;
	char	*position;
	char	*start_graph;
	unsigned char	*data;
	char	*end_graph;
	char	*eject;
	} HPLJ_Ras;

typedef	struct	HPLJ_Info	{
	HPLJ_Ras	ras;
	char		*trans_data;
	int		row_size;
	int		image_size;
	int		dpi;
	} HPLJ_Info;

#endif	_hplj_
