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
	} ComDev;
	

#endif
