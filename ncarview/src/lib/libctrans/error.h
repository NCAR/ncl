#ifndef	_ctrans_error_
#define	_ctrans_error_

#define	ERR_OK		0
#define	ERR_NO_DEVICE	1	/* No such device			*/
#define	ERR_INIT_DEVICE	2	/* Can't opening device			*/
#define	ERR_NO_FONT	3	/* No such font				*/
#define	ERR_INIT_FONT	4	/* Error opening font			*/
#define	ERR_INV_STATE	5	/* Invalid state for operation		*/
#define	ERR_OPEN_META	6	/* Can't open metafile			*/
#define	ERR_PAR_META	7	/* Error in metafile encoding 		*/
#define	ERR_INV_META	8	/* Illegal or invalid metafile element	*/
#define	ERR_INV_ARG	9	/* Invalid argument			*/

extern	int	CtransSetError_();
extern	int	CtransGetErrorNumber_();
extern	char	*CtransGetErrorMessage_();

#endif	_ctrans_error_
