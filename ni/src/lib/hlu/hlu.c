/*
 *      $Id: hlu.c,v 1.5 1993-11-02 15:57:56 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlu.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Aug 28 09:05:49 MDT 1992
 *
 *	Description:	This file contains simple utility functions that
 *			are needed by layer programmers as well as app
 *			writers.
 */

#include <ncarg/hlu/hluP.h>
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#include <ncarg/c.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/BaseP.h>

/************************************************************************
 *									*
 * These functions are used for memory management			*
 *									*
 ************************************************************************/

/*
 * Function:	NhlMalloc
 *
 * Description:	This function is our interface to the regular malloc
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	unsigned int	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NhlMalloc
#if	__STDC__
(
	unsigned int	size	/* size of memory requested	*/
)
#else
(size)
	unsigned int	size;	/* size of memory requested	*/
#endif
{
	void *ptr;

	if(size == 0)
		return NULL;

	ptr = (void *)malloc(size);

	if(ptr == NULL)
		NhlPError(FATAL,errno,"NhlMalloc Failed");

	return(ptr);
}

/*
 * Function:	NhlCalloc
 *
 * Description:	This function is our interface to the regular calloc
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	unsigned int	num	number of elements
 *		unsigned int	size	size of each element
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NhlCalloc
#if	__STDC__
(
	unsigned int	num,	/* number of elements		*/
	unsigned int	size	/* size of each element		*/
)
#else
(num,size)
	 unsigned int	num;	/* number of elements		*/
	unsigned int	size;	/* size of each element		*/
#endif
{
	void *ptr;

	if((num * size) == 0)
		return NULL;

	ptr = (void *)calloc(num, size);

	if(ptr == NULL)
		NhlPError(FATAL,errno,"NhlCalloc Failed");

	return(ptr);
}

/*
 * Function:	NhlRealloc
 *
 * Description:	This function is our interface to the regular realloc
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	void		*ptr	pointer to old memory
 *		unsigned int	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NhlRealloc
#if	__STDC__
(
	void		*ptr,	/* pointer to old memory	*/
	unsigned int	size	/* size of memory requested	*/
)
#else
(ptr,size)
	void		*ptr;	/* pointer to old memory	*/
	unsigned int	size;	/* size of memory requested	*/
#endif
{
	void *tptr;

	if(ptr == NULL)
		return NhlMalloc(size);
	else{
		tptr = (void *)realloc(ptr,size);

		if(tptr == NULL)
			NhlPError(FATAL,errno,"NhlRealloc Failed");

		return(tptr);
	}
}

/*
 * Function:	NhlFree
 *
 * Description:	This function is our interface to the regular free
 *		system call.  We are using it so we can do error handleing
 *		for memory allocation in one place and so we can impliment
 *		our own memory management code if we need to.
 *
 * In Args:	void		*ptr	pointer to memory to free
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlFree
#if	__STDC__
(
	void		*ptr	/* pointer to memory to free	*/
)
#else
(ptr)
	void		*ptr;	/* pointer to memory to free	*/
#endif
{

	if(ptr == NULL)
		return(NOERROR);

	else{
#if	defined(__sgi) || defined(_HPUX_SOURCE) || defined(__CLCC__)

		free(ptr);
		return NOERROR;
#else
		register int ret;

		ret = free(ptr);

		if(ret == 0){
			NhlPError(WARNING,errno,"Error in NhlFree");
			return(WARNING);
		}
		else{
			return(NOERROR);
		}
#endif
	}
}

/************************************************************************
 *									*
 * These functions manage the global Layer Table			*
 *									*
 ************************************************************************/

static Layer *LayerTable = NULL;
static int num_layers = 0;
static int table_len = 0;

/*
 * Function:	_NhlGetLayer
 *
 * Description:	This function is used to retrieve a layer of the given id
 *		from the internal layer table.
 *
 * In Args:	int	id	The id of the layer requested
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	Layer - the layer with the given id or NULL
 * Side Effect:	
 */
Layer
_NhlGetLayer
#if	__STDC__
(
	int	id	/* The layer id of the requested layer	*/
)
#else
(id)
	int	id;	/* The layer id of the requested layer	*/
#endif
{
	if((id >= table_len) || (id < 0))
		return((Layer)NULL);

	return(LayerTable[id]);
}

/*
 * Function:	_NhlAddLayer
 *
 * Description:	This function is used to add a layer into the global Layer
 *		Table.  It first determines if there is enough space allocated
 *		and then enters the given layer into the Table. It also
 *		updates the layer->base.pid to the correct entry in the table.
 *
 * In Args:	Layer	l	The layer to enter into the table
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	pid or NhlErrorTypes pid > 0 and Error if < 0 - Never return 0
 * Side Effect:	_NhlLayerTable can change - to grow.
 */
int
_NhlAddLayer
#if	__STDC__
(
	Layer	l	/* The layer to enter into the table	*/
)
#else
(l)
	Layer	l;	/* The layer to enter into the table	*/
#endif
{
	register int i;

	/*
	 * Increase size of table if needed
	 */
	if(table_len < num_layers + 1){
		LayerTable = NhlRealloc(LayerTable,
			(unsigned)((table_len + LAYERLISTINC) * sizeof(Layer)));
		if(LayerTable == NULL){
			NhlPError(FATAL,12,
				"Unable to Increase size of Layer Table");
			return(FATAL);
		}

		for(i=table_len; i < (table_len + LAYERLISTINC); i++)
			LayerTable[i] = (Layer)NULL;
		table_len += LAYERLISTINC;
	}

	for(i=0; i < table_len; i++){
		if(LayerTable[i] == (Layer)NULL){
			LayerTable[i] = l;
			num_layers++;
			l->base.id = i;
			return(i);
		}
	}

	/*
	 * Error message!
	 */
	return(FATAL);
}

/*
 * Function:	_NhlRemoveLayer
 *
 * Description:	This function is used to remove a layer from the global Layer
 *		Table.  
 *
 * In Args:	Layer	l	The layer to remove from the table
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlRemoveLayer
#if	__STDC__
(
	Layer	l	/* The layer to remove from the table	*/
)
#else
(l)
	Layer	l;	/* The layer to remove from the table	*/
#endif
{
	if(l == (Layer)NULL){
		/*
		 * ERROR - request to remove a null layer
		 */
		NhlPError(WARNING,E_UNKNOWN,
					"_NhlRemoveLayer:Can't rm NULL layer");
		return(WARNING);
	}

	if(LayerTable[l->base.id] != l){
		/*
		 * ERROR - layers id doesn't match internal table
		 */
		NhlPError(WARNING,E_UNKNOWN,
			"_NhlRemoveLayer:layer id doesn't match layer Table");
		return(WARNING);
	}

	LayerTable[l->base.id] = (Layer)NULL;
	num_layers--;

	return(NOERROR);
}

/************************************************************************
*									*
*	The rest of these functions are misc. utilty functions that	*
*	were needed.							*
*									*
************************************************************************/

/*
 * Function:	NhlName
 *
 * Description:	This function returns the name of a given plot specified
 *		by pid.
 *
 * In Args:	int	pid;	id of a plot
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	Const char *
 * Side Effect:	
 */
Const char *
NhlName
#if	__STDC__
(
	int	pid	/* id of a plot	*/
)
#else
(pid)
	int	pid;	/* id of a plot	*/
#endif
{
	Layer	tmp = _NhlGetLayer(pid);

	if(tmp == NULL){
		NhlPError(FATAL,E_UNKNOWN,"Unable to access plot with pid:%d",
									pid);
		return NULL;
	}

	return tmp->base.name;
}

/*
 * Function:	NhlClassName
 *
 * Description: Returns class name of object
 *
 * In Args:	pid	integer id of object whose class is requested
 *
 * Out Args:	NONE
 *
 * Return Values: returns result of _NhlClassName call	
 *
 * Side Effects: NONE
 */
Const char *
NhlClassName
#if	__STDC__
(
	int pid
)
#else
(pid)
int pid;
#endif
{
	Layer	l = _NhlGetLayer(pid);

	return(_NhlClassName(_NhlClass(l)));
}

/*
 * Function:	_NhlClassName
 *
 * Description:	This function returns the name of a given class
 *
 * In Args:	LayerClass	lc;	pointer to class struct
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	Const char *
 * Side Effect:	
 */
Const char *
_NhlClassName
#if	__STDC__
(
	LayerClass	lc	/* pointer to class struct	*/
)
#else
(lc)
	LayerClass	lc;	/* pointer to class struct	*/
#endif
{
	return lc->base_class.class_name;
}

/*
 * Function:	_NhlClass
 *
 * Description:	This function returns the given Layer's class pointer.
 *
 * In Args:	Layer l;	layer
 *
 * Out Args:	
 *
 * Scope:	Global, Public
 * Returns:	LayerClass
 * Side Effect:	
 */
LayerClass
_NhlClass
#if	__STDC__
(
	Layer l		/* Instance pointer */
)
#else
(l)
	Layer l;
#endif
{
	return(l->base.layer_class);
}

/*
 * Function:	_NhlIsFloatRes
 *
 * Description:	This function takes a resource name and determines if it
 *		is a float resource by checking if the last char is a 'F'
 *
 * In Args:	res_name	resource name
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlBoolean - True if res is a float res
 * Side Effect:	
 */
NhlBoolean
_NhlIsFloatRes
#if	__STDC__
(
	NhlString	res_name	/* resource name	*/
)
#else
(res_name)
	NhlString	res_name;	/* resource name	*/
#endif
{
	char	*index = res_name;

	while(*index != '\0') index++;

	if(*(index - 1) == 'F')
		return(True);
	else
		return(False);
}

/*
 * Function:	_NhlResolvePath
 *
 * Description:	This function takes a pathname and returns a pathname with
 *		all "~"'s and environment "$var"'s resolved.
 *
 * In Args:	char 	*rawfname	fname as provided
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	char *
 * Side Effect:	
 */
Const char
*_NhlResolvePath
#if	__STDC__
(
	Const char	*rawfname	/* fname as provided	*/
)
#else
(rawfname)
	Const char	*rawfname;	/* fname as provided	*/
#endif
{
	static char	fname[MAXFNAMELEN];
	char		tmpfname[MAXFNAMELEN];
	char		buffer[MAXFNAMELEN];
	char		*piece = NULL;
	char		*tptr;
	struct passwd	*pw = NULL;
	NhlBoolean	first = True;

	if(rawfname == NULL){
		return(NULL);
	}

	fname[0] = '\0';

	strcpy(tmpfname,rawfname);
	strcpy(buffer,PATHDELIMITER);
	if(tmpfname[0] == buffer[0])
		strcpy(fname,PATHDELIMITER);
	piece = strtok(tmpfname,PATHDELIMITER);

	while(piece != NULL){

		if(first)
			first = False;
		else
			strcat(fname,PATHDELIMITER);

		switch(*piece){

			case '~':

				if(*(piece+1) != '\0'){	/* different username */

					strcpy(buffer,(piece + 1));
					pw = getpwnam(buffer);
				}
				else			/* this username      */
					pw = getpwuid(getuid());

				if(pw == NULL){
					NhlPError(FATAL,E_UNKNOWN,
						"Unable to Resolve \'~\' in %s",
								rawfname);
					return(NULL);
				}
				strcat(fname,pw->pw_dir);

				break;

			case '$':

				strcpy(buffer,(piece + 1));
				tptr = getenv(buffer);

				if(tptr == NULL){
					NhlPError(FATAL,E_UNKNOWN,
						"Unable to Resolve %s in %s",
								piece,rawfname);
					return(NULL);
				}
				strcat(fname,tptr);

				break;

			default:

				strcat(fname,piece);
		}

		piece = strtok(NULL,PATHDELIMITER);
	}

	return(fname);
}

/*
 * Function:	_NhlGetSysResFile
 *
 * Description:	This function returns the name of the system resource file.
 *		It saves the name in static data so the next time it is
 *		requested it doesn't need to look in the system again.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	const char *
 * Side Effect:	
 */
Const char
*_NhlGetSysResFile
#if	__STDC__
(
	void	/* No args	*/
)
#else
()
#endif
{
	static char		fname[MAXFNAMELEN];
	const char		*tmp = NULL;
	static NhlBoolean	init = False;

	if(!init){

		tmp = getenv(SYSRESENVNAME);
		if(tmp == NULL){
			tmp = GetNCARGPath("lib");

			if(tmp == NULL)
				return NULL;

			strcpy(fname,tmp);
			strcat(fname,"/ncarg/");
			strcat(fname,DEFSYSRESFNAME);
		}
		else
			strcpy(fname,_NhlResolvePath(tmp));

		init = True;
	}

	return(fname);
}

/*
 * Function:	_NhlGetUsrResFile
 *
 * Description:	This function returns the name of the user resource file.
 *		It saves the name in static data so the next time it is
 *		requested it doesn't need to look in the system again.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	const char *
 * Side Effect:	
 */
Const char 
*_NhlGetUsrResFile
#if	__STDC__
(
	void	/* No args	*/
)
#else
()
#endif
{
	static char		fname[MAXFNAMELEN];
	char			*tmp = NULL;
	static NhlBoolean	init = False;

	if(!init){

		tmp = getenv(USRRESENVNAME);
		if(tmp == NULL)
			tmp = DEFUSRRESFNAME;

		(void)strcpy(fname,_NhlResolvePath(tmp));
		init = True;
	}

	return(fname);
}

/*
 * Function:	NhlSetSArg
 *
 * Description:	This function sets an Arg in an arglist so the arg list
 *		can be used in a NhlALCreate, NhlALSetValues.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	void
 * Side Effect:	
 */
/*VARARGS2*/
void
NhlSetSArg
#if	NeedVarArgProto
(
	NhlSArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
)
#else
(arg,resname,va_alist)
	NhlSArg		*arg;		/* arg to set		*/
	NhlString	resname;	/* resource to set	*/
	va_dcl				/* value to set arg to	*/
#endif
{
	va_list		ap;
	double		tmp;

	arg->name = resname;

	VA_START(ap,resname);
	if(_NhlIsFloatRes(resname)){
		tmp = va_arg(ap,double);
		*(float *)&(arg->value) = (float)tmp;
	}
	else
		arg->value = va_arg(ap,_NhlArgVal);

	va_end(ap);
}

/*
 * Function:	_NhlSArgToSetArgList
 *
 * Description:	This function is used to allocate a _NhlArgList given an
 *		NhlSArgList.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
void
_NhlSArgToSetArgList
#if	__STDC__
(
	_NhlExtArgList	args,	/* arglist return	*/
	NhlSArgList	sargs,	/* public arglist	*/
	int		nargs	/* num args		*/
)
#else
(args,sargs,nargs)
	_NhlExtArgList	args;	/* arglist return	*/
	NhlSArgList	sargs;	/* public arglist	*/
	int		nargs;	/* num args		*/
#endif
{
	register int	i;

	for(i=0;i < nargs; i++){
		args[i].quark = NrmStringToQuark(sargs[i].name);
		args[i].value = sargs[i].value;
		args[i].type = NrmNULLQUARK;
	}

	return;
}
/*
 * Function:	NhlSetGArg
 *
 * Description:	This function sets an Arg in an arglist so the arg list
 *		can be used in NhlALGetValues.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	void
 * Side Effect:	
 */
/*VARARGS2*/
void
NhlSetGArg
#if	NeedVarArgProto
(
	NhlGArg		*arg,		/* arg to set		*/
	NhlString	resname,	/* resource to set	*/
	...				/* value to set arg to	*/
)
#else
(arg,resname,va_alist)
	NhlGArg		*arg;		/* arg to set		*/
	NhlString	resname;	/* resource to set	*/
	va_dcl				/* value to set arg to	*/
#endif
{
	va_list		ap;

	arg->resname = resname;

	VA_START(ap,resname);
	arg->value = va_arg(ap,_NhlArgVal);
	va_end(ap);
}

/*
 * Function:	_NhlGArgToGetArgList
 *
 * Description:	This function is used to allocate a _NhlArgList given an
 *		NhlGArgList.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
void
_NhlGArgToGetArgList
#if	__STDC__
(
	_NhlExtArgList	args,	/* arglist return	*/
	NhlGArgList	gargs,	/* public arglist	*/
	int		nargs	/* num args		*/
)
#else
(args,gargs,nargs)
	_NhlExtArgList	args;	/* arglist return	*/
	NhlGArgList	gargs;	/* public arglist	*/
	int		nargs;	/* num args		*/
#endif
{
	register int	i;

	for(i=0;i < nargs; i++){
		args[i].quark = NrmStringToQuark(gargs[i].resname);
		args[i].value = gargs[i].value;
		args[i].type = NrmNULLQUARK;
	}

	return;
}

/*
 * Function:	_NhlInherit
 *
 * Description:	This function is used to provide a pointer to check against
 *		if a function is supposed to be inherited from it's
 *		superclass.  This function should never be called so
 *		it prints out an error message if it is.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Private Global
 * Returns:	void
 * Side Effect:	
 */
void
_NhlInherit
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlPError(FATAL,E_UNKNOWN,
				"_NhlInherit- Inheritance resolved improperly");
	return;
}


/*
 * Function:	_NhlArgIsSet
 *
 * Description:	returns true if the given resource name is set in the given
 *		arg list.  otherwise returns false.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlBoolean
 * Side Effect:	
 */
NhlBoolean _NhlArgIsSet
#if     __STDC__
(
        _NhlArgList args,
        int     num_args,
        char    *resource_name
)
#else
(args,num_args,resource_name)
        _NhlArgList args;
        int     num_args;
        char    *resource_name;
#endif
{
	int quark = NrmStringToQuark(resource_name),i;
	_NhlArgList step = args;

	for(i = 0; i<num_args; i++) {
		if(step[i].quark == quark) 
			return(True);
	}
	return(False);
}

/*
 * Function:	_NhlCreateGenArray
 *
 * Description:	This function is used to define the size/shape of arrays passed
 *		in as resources. If num_dimensions is -1111 then pass back an
 *		empty NhlGenArray - Data=NULL,num_dimensions=num_elements=0
 *		and len_dimensions = NULL.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
_NhlCreateGenArray
#if	__STDC__
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions,/* number of dimensions	*/
	NhlBoolean	copy_data	/* copy data pointer?	*/
)
#else
(data,type,size,num_dimensions,len_dimensions,copy_data)
	NhlPointer	data;			/* data array		*/
	NhlString	type;			/* type of each element	*/
	unsigned int	size;			/* size of each element	*/
	int		num_dimensions;		/* number of dimensions	*/
	int		*len_dimensions;	/* number of dimensions	*/
	NhlBoolean	copy_data;		/* copy data pointer?	*/
#endif
{
	static NhlBoolean	first_time = True;
	static NrmQuark		QString;
	NhlGenArray		gen = NULL;
	int			i;

	if(first_time){
		QString = NrmStringToQuark(NhlTString);
		first_time = False;
	}

	if((num_dimensions < 1) && (num_dimensions != -1111)){
		NHLPERROR((FATAL,E_UNKNOWN,
		"NhlGenArrayCreate:Arrays must have at least one dimension"));
		return NULL;
	}

	gen = NhlMalloc(sizeof(NhlGenArrayRec));

	if(gen == NULL)
		return NULL;

	gen->typeQ = NrmStringToQuark(type);
	gen->size = size;

	if(num_dimensions == -1111){
		gen->num_dimensions = 0;
		gen->len_dimensions = NULL;
		gen->num_elements = 0;
		gen->data = NULL;
		gen->my_data = False;
	}
	else{
		gen->num_dimensions = num_dimensions;
		if(gen->num_dimensions == 1){
			gen->num_elements = *len_dimensions;
			gen->len_dimensions = &gen->num_elements;
		}
		else{
			gen->len_dimensions =
					NhlMalloc(num_dimensions * sizeof(int));
			if(gen->len_dimensions == NULL)
				return NULL;
			gen->num_elements = 1;
			for(i=0;i < num_dimensions;i++){
				gen->len_dimensions[i] = len_dimensions[i];
				gen->num_elements *= len_dimensions[i];
			}
		}

		if(copy_data){
			gen->data = NhlMalloc(gen->num_elements * gen->size);
			if(gen->data == NULL)
				return NULL;

			/*
			 * If the individual elements are NhlString's then we
			 * know how to copy them.
			 */
			if((gen->typeQ == QString) &&
					(gen->size == sizeof(NhlString))){
				NhlString	*otable = data;
				NhlString	*ntable = gen->data;

				for(i=0;i<gen->num_elements;i++){
					if(otable[i] == NULL){
						ntable[i] = NULL;
					}
					else{
						ntable[i] =
						NhlMalloc(strlen(otable[i])+1);
						if(ntable[i] == NULL)
							return NULL;
						strcpy(ntable[i],otable[i]);
					}
				}
			}
			else{
				memcpy(gen->data,data,
						gen->num_elements * gen->size);
			}
			gen->my_data = True;
		}
		else{
			gen->data = data;
			gen->my_data = False;
		}
	}

	return gen;
}

/*
 * Function:	NhlCreateGenArray
 *
 * Description:	This function is used by the user to define the size/shape
 *		of arrays passed in as resources. It does not copy the
 *		data - it only keeps a pointer to it.  The data pointer
 *		memory belongs to the user.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
NhlCreateGenArray
#if	__STDC__
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	int		*len_dimensions	/* number of dimensions	*/
)
#else
(data,type,size,num_dimensions,len_dimensions)
	NhlPointer	data;			/* data array		*/
	NhlString	type;			/* type of each element	*/
	unsigned int	size;			/* size of each element	*/
	int		num_dimensions;		/* number of dimensions	*/
	int		*len_dimensions;	/* number of dimensions	*/
#endif
{
	return
	_NhlCreateGenArray(data,type,size,num_dimensions,len_dimensions,False);
}

/*
 * Function:	_NhlCopyGenArray
 *
 * Description:	This function copies an NhlGenArray and allocates an
 *		NhlGenArray. It copies the "data" part of the GenArray
 *		if copy_data is true - otherwise the new GenArray just
 *		references the same data pointer.
 *
 * In Args:	
 *		NhlGenArray	gen		generic array pointer
 *		NhlBoolean	copy_data	copy data?
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlGenArray
 * Side Effect:	
 */
NhlGenArray
_NhlCopyGenArray
#if	__STDC__
(
	NhlGenArray	gen,		/* generic array pointer	*/
	NhlBoolean	copy_data	/* copy data?			*/
)
#else
(gen,copy_data)
	NhlGenArray	gen;		/* generic array pointer	*/
	NhlBoolean	copy_data;	/* copy data?			*/
#endif
{
	return _NhlCreateGenArray(gen->data,NrmQuarkToString(gen->typeQ),
		gen->size,gen->num_dimensions,gen->len_dimensions,copy_data);
}

/*
 * Function:	NhlFreeGenArray
 *
 * Description:	This function is used by the user to destroy an NhlGenArray
 *		description record after they are done using it.  This
 *		function does not free the "data" part of the NhlGenArray
 *		the "data" pointer memory belongs to the user.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	NhlGenArray
 * Side Effect:	
 */
void
NhlFreeGenArray
#if	__STDC__
(
	NhlGenArray	gen	/* gen array to free	*/
)
#else
(gen)
	NhlGenArray	gen;	/* gen array to free	*/
#endif
{
	static NhlBoolean	first_time = True;
	static NrmQuark		QString;

	if(first_time){
		QString = NrmStringToQuark(NhlTString);
		first_time = False;
	}

	if(gen == NULL)
		return;

	if(gen->my_data){
		if(gen->typeQ == QString){
			int i;
			NhlString	*table = gen->data;

			for(i=0;i<gen->num_elements;i++)
				(void)NhlFree(table[i]);
		}
		(void)NhlFree(gen->data);
	}
	if(gen->len_dimensions != &gen->num_elements)
		(void)NhlFree(gen->len_dimensions);

	(void)NhlFree(gen);

	return;
}


/*
 * Function:    _NhlValidatedGenArrayCopy
 *
 * Description: Copies Generic Arrays, with checking, from gfrom to gto.
 *
 *		Pointer gto must point to an allocated Generic Array that
 *		is assumed to be valid and serves as a template for 
 *		validated the user-supplied gfrom Generic Array. It need not
 *		contain any actual data (num_elements may be 0).
 *		The type, type size, number of dimensions, and
 *		length of all dimensions except the first, must match 
 *		between both arrays for the copy to proceed. The first
 *		dimension of the gfrom array may be greater or smaller 
 *		than the first dimension of the gto array. If greater the
 *		gto array is freed if non-NULL, and the routine performs a 
 *		complete copy. Otherwise, it copies the data only into
 *		the space already assigned to the gto array. In this 
 *		situation, the setting of the exact_count parameter controls
 *		whether the num_elements field of the gto array is possibly
 *		adjusted (downward only) to reflect the actual number of 
 *		elements in the gfrom array.
 *
 * In Args:     NhlGenArray	gfrom -- the generic array that is copied. 
 *		int		max_el   -- the maximum number of elements
 *					    permitted
 *		NhlBoolean	copy_data -- whether to allocate storage
 *					     space for the data.
 *		NhlBoolean	exact_count -- whether to copy the size
 *					       info when a data only copy is
 *					       performed. If False, the
 *					     num_elements and len_dimension[0]
 *					     of gto will reflect the allocated
 *					       space in gto rather than the
 *					       number of elements copied from
 *					       the gfrom array.
 *		char *		res_name -- The associated resource name
 *		char *		caller -- name of the user calling routine
 *
 * Out Args:	NhlGenArray *	gto -- Pointer to a destination Generic Array.
 *				       All its fields are assumed to contain 
 *				       valid information.
 *
 * Scope:       Global Private
 *
 * Returns:     If successful NOERROR; FATAL on memory allocation errors;
 *              WARNING if the from GenArray is invalid in some way.
 *
 * Side Effect: When string data is copied, all NULL string pointers are 
 *		replaced with single-byte strings containing only a NULL
 *		terminator.
 *
 */

NhlErrorTypes _NhlValidatedGenArrayCopy
#if __STDC__
	(NhlGenArray	*gto, 
	 NhlGenArray	gfrom,
	 int		max_el,
	 NhlBoolean	copy_data,
	 NhlBoolean	exact_count,
	 char		*res_name,
	 char		*caller)
#else
(gto,gfrom,max_el,copy_data,exact_count,res_name,caller)
	NhlGenArray	*gto; 
	NhlGenArray	gfrom;
	int		max_el;
	NhlBoolean	copy_data;
	NhlBoolean	exact_count;
	char		*res_name;
	char		*caller;
#endif
{
	char		*e_text;
	int		i;
	int		el_count;
	static NrmQuark Qstring;
	static NhlBoolean first = True;

	if (first) {
		Qstring = NrmStringToQuark(NhlTString);
		first = False;
	}

	if (gfrom == NULL || (*gto) == NULL) {
		e_text = 
		 "%s: %s NULL array passed in: copy not performed";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING;
	}
	if (gfrom->num_elements <= 0) {
		e_text = 
		 "%s: %s invalid element count: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING;
	}
	else if (gfrom->num_elements > max_el) {
		e_text =
		 "%s: %s exceeds maximum number of elements, %d: ignoring";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name,max_el);
		return WARNING;
	}
	if (gfrom->num_dimensions != (*gto)->num_dimensions) {
		e_text = 
		 "%s: %s invalid dimensionality: copy not performed";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING;
	}
/*
 * All dimensions except the first (index 0) must match in length
 */
	el_count = gfrom->len_dimensions[0];
	for (i=1; i<gfrom->num_dimensions; i++) {
		el_count *= gfrom->len_dimensions[i];
		if (gfrom->len_dimensions[i] != (*gto)->len_dimensions[i]) {
			e_text = 
			    "%s: %s dimensional mismatch: copy not performed";
			NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
			return WARNING;
		}
	}
	if (el_count != gfrom->num_elements) {
		e_text = 
		 "%s: %s invalid element count: copy not performed";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING;
	}

	if (gfrom->typeQ != (*gto)->typeQ) {
		e_text = "%s: %s type mismatch: copy not performed";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING; 
	}
	if (gfrom->size != (*gto)->size) {
		e_text = "%s: %s type size mismatch: copy not performed";
		NhlPError(WARNING,E_UNKNOWN,e_text,caller,res_name);
		return WARNING; 
	}
	
	if (gfrom->num_elements > (*gto)->num_elements) {
		NhlFreeGenArray((*gto));
		if (((*gto) = _NhlCopyGenArray(gfrom,copy_data)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(FATAL,E_UNKNOWN,e_text,caller,res_name);
			return FATAL;
		}
		return NOERROR;
	}
	else if (!copy_data)
			(*gto)->data = gfrom->data;
	else if (gfrom->typeQ != Qstring) {
		memcpy((void *)(*gto)->data, (Const void *)gfrom->data,
		       gfrom->num_elements * gfrom->size);
	} 
	else {
		NhlString *from = (NhlString *) gfrom->data;
		NhlString *to = (NhlString *) (*gto)->data;
		for (i=0; i<gfrom->num_elements; i++,to++,from++) {
			if (*from == NULL) {
				*to = (NhlString) 
					NhlRealloc(*to,
						   strlen("")+1);
				if (*to == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(*to,"");
			}
			else if (*to == NULL) {
				*to = (NhlString) NhlMalloc(strlen(*from)+1);
				if (*to == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(*to,*from);
			}
			else if (strcmp(*to,*from)) {
				*to = (NhlString) 
					NhlRealloc(*to, strlen(*from)+1);
				if (*to == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(FATAL,E_UNKNOWN,e_text,
						  caller,res_name);
					return FATAL;
				}
				strcpy(*to,*from);
			}
		}
		if (exact_count) {
			to = (NhlString *)(*gto)->data + gfrom->num_elements;
			for (i=gfrom->num_elements;
			     i<(*gto)->num_elements; i++) {
				NhlFree(*to++);
			}
		}
				
	}

/* 
 * ASSERT: (*gto)->num_elements >= gfrom->num_elements
 */
	if (exact_count) {
		(*gto)->num_elements = gfrom->num_elements;
		(*gto)->len_dimensions[0] = gfrom->len_dimensions[0];
	}

	return NOERROR;

}
