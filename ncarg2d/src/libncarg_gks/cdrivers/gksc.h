/*
 *	$Id: gksc.h,v 1.1 1994-03-30 02:11:26 fred Exp $
 */
/*
 *      File:		gksc.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	This file defines a gksc which is used to shuttle 
 *			GKS instructions to a particular device driver's 
 *			support function.
 */

#ifndef	_gksc_
#define	_gksc_

#ifdef __STDC__
typedef void*           GKSC_Ptr;
#else
typedef char*           GKSC_Ptr;
#endif

/*
 *	The following structures define the format of data expected by
 *	the gks device drivers. The following complex data types need to 
 *	be supported by each device, Points, Floats, Ints, String, 
 *	Indexes, and RGBs. Within each complex data type is a pointer
 *	to a list of simple device dependent data. The 'simple data' types 
 *	declared as type GKSC_Ptr and must be cast to the appropriate type
 *	for each device. Each complex type also contains a field which
 *	specifies the size of a single element of 'simple data'. The next
 *	field specifes how many elements of valid data are stored in the 
 *	simple  data list. The field after that specifies how much memory
 *	has been allocated to the simple data list. Finally each Complex
 *	type contains a pointer to a device dependent function for converting
 *	raw gks data to the simple data type. The format of the raw data
 *	depends on which Complex data type formating is being performed for.
 */

typedef	struct	ComplexData_	{
	GKSC_Ptr	list;		/* a list of points		*/
	unsigned int	size,		/* sizeof (list[0])		*/
			num,		/* num elements in p_list	*/
			list_size;	/* mem alloc'd to p_lsit	*/
	void		(*convert)();	/* convert reals to points	*/
	} ComplexData;

typedef	ComplexData	Points;
typedef	ComplexData	String;
typedef	ComplexData	Ints;
typedef	ComplexData	Floats;
typedef	ComplexData	Indexes;
typedef	ComplexData	RGBs;

/*
 *	the GKSC structure. A GKSC contains all the information about a
 *	particular instance of a device driver. It also contains a single
 *	GKS instruction with all its data to be executed. The return value
 *	for the gks function should be zero upon success else it should be
 *	the appopriate gks error number.
 */
typedef	struct	GKSC_	{
	int		gd;		/* gksc descriptor		*/
	unsigned int	opcode;		/* GKS instruction to execute	*/
	Points		p;		/* point data			*/
	String		s;		/* string data			*/
	Ints		i;		/* integer data			*/
	Floats		f;		/* floating point data		*/
	Indexes		x;		/* color indeces		*/
	RGBs		rgb;		/* rgb color triples		*/
	GKSC_Ptr		ddp;	/* device dependent data	*/
	int	(**operations)();	/* device functions		*/
	} GKSC;

/*
 * flags for setting direction of conversion of data
 */
#define	RAW_TO_COOKED	0
#define	COOKED_TO_RAW	1

/*
 *	amount of memory initialy allocated to the list field of each
 *	complex type. WARNING! WriteToGKSC() assumes that this number
 *	is at least 5
 */
#define	INITIAL_MAL	10


#define	ClearGKSC(gksc) \
	((gksc)->p.num = (gksc)->s.num = (gksc)->i.num = \
	(gksc)->f.num = (gksc)->x.num = (gksc)->rgb.num = 0)


#define	ExecGKSC(gksc) \
	((*((gksc)->operations[(gksc)->opcode]))(gksc))



/*
 *	a list of points. 
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	fxs, fys, points, num_f), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), fxs is a list of normalized 
 *	x coords of type float, fys is a list of normalized y coords of type 
 *	float, points is a pointer to the Points structure, num_f is an 
 *	int giving the number of elements in fxs (fys).
 */

/*
 *	a list of floats
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	fxs, f, num_f), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), fxs is a list of floats,
 *	f is a pointer to this Floats structure, and num_f is an
 *	int giving the number of elements in fxs.
 */
/*
 *	a list of ints
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	ints, i, num_i), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), 'ints' is a list of integers,
 *	'i' is a pointer to this Ints structure, and 'num_i' is an
 *	int giving the number of elements in 'ints'.
 *
 */
/*
 *	a list of chars (a string)
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	chars, s, num_s), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), 'chars' is a list 
 *	of integers, 's' is a pointer to this String structure, and 'num_s' 
 *	is an int giving the number of elements in 'chars'.
 */

/*
 *	a list of color indices
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	indexes, x, num_x), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), 'indexes' is a list 
 *	of integers, 'x' is a pointer to this Indexes_ structure, and 'num_x' 
 *	is an int giving the number of elements in 'indexes'.
 */

/*
 *	 a list of rgb triplets
 *
 *	The conversion function expects the following input parameters (ddp, 
 *	fxs, rgb, num_rgb), where 'ddp' is a pointer to device dependent 
 *	data supplied by the gksc (type GKSC_Ptr), 'fxs' is a list 
 *	of normalized floats containing color intensities red, green, blue, 
 *	red, green, blue, red, etc, etc., 'rgb' is a pointer to this RGBs 
 *	structure, and 'num_rgb' is an int giving the number of triples 
 *	in 'fxs'.
 */

	
/*
 *	some error codes
 */
#define	ERR_LOCAL	-113	/* misc. error for X driver             */
#define	ERR_INV_OPCODE	-109	/* invalid or unsupported opcode	*/
#define	ERR_INV_WK_ID	20	/* invalid workstation id		*/
#define	ERR_OPN_DEV	26	/* error opening device			*/
#define	ERR_INV_RECT	51	/* transformation rect. def. is invalid	*/
#define	ERR_INV_LINE	63	/* invalid line type			*/
#define	ERR_INV_MARKER	67	/* invalid polymarker type		*/
#define	ERR_INV_FONT	71	/* invalid text font			*/
#define	ERR_INV_FILL	77	/* invalid fill style			*/
#define	ERR_INV_HATCH	80	/* invalid fill hatch style		*/
#define	ERR_FILL_PAT	83	/* Fill style pattern not supported	*/
#define	ERR_CELL_DIM	91	/* dimensions of color array invalid	*/
#define	ERR_INV_IND	94	/* invalid or undefined color index	*/
#define	ERR_INV_CODE	101	/* invalid code in string		*/
#define	ERR_INV_ESCAPE	180	/* escape function not supported	*/


extern	GKSC    *CreateGKSC(
#ifdef	NeedFuncProto
        char    *dev_name
#endif
);

extern	void    FreeGKSC(
#ifdef	NeedFuncProto
        GKSC    *gksc
#endif
);

extern	int	WriteToGKSC(
#ifdef	NeedFuncProto
        GKSC    *gksc,
        int     gks_opcode,
        int     total_i,
        int     num_i_sent,
	int	*ints,
        int     total_x,
        int	num_x_sent,
        int	*indexes,
        int     total_f,
        int	num_f_sent,
        float	*fxs, 
	float	*fys,
        int	total_c,
	int	 num_c_sent,
	int	*chars
#endif
);

int     ReadFromGKSC(
#ifdef	NeedFuncProto
        GKSC	*gksc,
        int	gks_opcode,
        int	*total_i,
	int	*num_i_sent,
	int	*ints,
	int     *total_x,
	int	*num_x_sent,
	int	*indexes,
	int     *total_f,
	int	*num_f_sent,
	float   *fxs,
	float	*fys,
	int     *total_c,
	int	*num_c_sent,
	int	*chars
#endif
);

extern	GKSC    *IndexToGKSC(
#ifdef	NeedFuncProto
	int index
#endif
);

extern	int    GKSCToIndex(
#ifdef	NeedFuncProto
	GKSC *gksc
#endif
);



#endif	/*	_gksc_	*/
