/*
 *	$Id: commondev.h,v 1.4 1992-04-03 20:56:28 clyne Exp $
 */
#ifndef	_commondev_
#define	_commondev_

typedef	struct	ComDev_ {
	char	*name;
	void	(*open)();
	void	(*close)();
	void	(*point_flush)();
	void	(*line)();
	void	(*devline)();
	void	(*setlinestyle)();
	void	(*setlinecolour)();
	void	(*setfillcolour)();
	void	(*setlinewidth)();
	void	(*update_color_table)();
	} ComDev;
	

#endif
