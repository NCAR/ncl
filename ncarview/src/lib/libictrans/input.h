
#ifndef	_input_
#define	_input_

#define	my_getc(f)	(stringIO ? STRING_GETC : getc((f)))
#define	STRING_GETC	(*inBufPtr ? (int) *inBufPtr++ : 0)


extern	char	*inBufPtr;
extern	int	stringIO;

#endif	/* _input_	*/
