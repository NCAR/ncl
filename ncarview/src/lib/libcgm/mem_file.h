/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

typedef	struct	{
	char	*name;		/* name of file			*/
	unsigned char	*_base;	/* the file			*/
	unsigned char	*_ptr;	/* pointer into _base		*/
	long	*size;		/* space allocated in bytes	*/
	long	*len;		/* lengh of file in bytes	*/
	int	*r_size;	/* size of a record in bytes	*/
	} CGM_iobuf;
	

	

/*
 * number of records initialy allocated to a newly created file
 */
#define F_INIT_SIZE	10
