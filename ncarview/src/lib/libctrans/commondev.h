/*
 *	$Id: commondev.h,v 1.5 1993-04-27 20:32:33 clyne Exp $
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
	int	(*update_color_table)();
	} ComDev;
	

#endif
