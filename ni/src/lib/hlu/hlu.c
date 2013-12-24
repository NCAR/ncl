/*
 *      $Id: hlu.c,v 1.44 2009-07-10 19:54:04 huangwei Exp $
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
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/FortranP.h>
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/c.h>
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
 * In Args:	ng_usize_t	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NhlMalloc
#if	NhlNeedProto
(
	ng_usize_t	size	/* size of memory requested	*/
)
#else
(size)
	ng_usize_t	size;	/* size of memory requested	*/
#endif
{
	void *ptr;

	if(size == 0)
		return NULL;

	ptr = (void *)malloc(size);

	if(ptr == NULL)
		NhlPError(NhlFATAL,errno,"NhlMalloc Failed");

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
 * In Args:	ng_usize_t	num	number of elements requested
 * In Args:	ng_usize_t	size	size of each element
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested, each byte of memory is set to 0
 * Side Effect:	
 */
void
*NhlCalloc
#if	NhlNeedProto
(
	ng_usize_t      num,     /* number of elements */
	ng_usize_t	size	/* size of each element	*/
)
#else
(num,size)
	ng_usize_t      num;     /* number of elements */
	ng_usize_t	size;	/* size of each element	*/
#endif
{
	void *ptr;

	if(size == 0 || num == 0)
		return NULL;

	ptr = (void *)calloc(num, size);

	if(ptr == NULL)
		NhlPError(NhlFATAL,errno,"NhlCalloc Failed");

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
 *		ng_usize_t	size	size of memory requested
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	pointer to memory of the size requested
 * Side Effect:	
 */
void
*NhlRealloc
#if	NhlNeedProto
(
	void		*ptr,	/* pointer to old memory	*/
	ng_usize_t	size	/* size of memory requested	*/
)
#else
(ptr,size)
	void		*ptr;	/* pointer to old memory	*/
	ng_usize_t	size;	/* size of memory requested	*/
#endif
{
	void *tptr;

	if(ptr == NULL)
		return NhlMalloc(size);
	else{
		tptr = (void *)realloc(ptr,size);

		if(tptr == NULL)
			NhlPError(NhlFATAL,errno,"NhlRealloc Failed");

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
#if	NhlNeedProto
(
	void		*ptr	/* pointer to memory to free	*/
)
#else
(ptr)
	void		*ptr;	/* pointer to memory to free	*/
#endif
{

	if(ptr == NULL)
		return(NhlNOERROR);

	else{
		(void)free(ptr);
		return NhlNOERROR;
	}
}

/************************************************************************
 *									*
 * These functions manage the global NhlLayer Table			*
 *									*
 ************************************************************************/

static NhlLayer *LayerTable = NULL;
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
 * Returns:	NhlLayer - the layer with the given id or NULL
 * Side Effect:	
 */
NhlLayer
_NhlGetLayer
#if	NhlNeedProto
(
	int	id	/* The layer id of the requested layer	*/
)
#else
(id)
	int	id;	/* The layer id of the requested layer	*/
#endif
{
	register int index = id - 1;

	if((index >= table_len) || (index < 0))
		return((NhlLayer)NULL);

	return(LayerTable[index]);
}

/*
 * Function:	_NhlAddLayer
 *
 * Description:	This function is used to add a layer into the global NhlLayer
 *		Table.  It first determines if there is enough space allocated
 *		and then enters the given layer into the Table. It also
 *		updates the layer->base.pid to the correct entry in the table.
 *
 * In Args:	NhlLayer	l	The layer to enter into the table
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	pid or NhlErrorTypes pid > 0 and Error if < 0 - Never return 0
 * Side Effect:	_NhlLayerTable can change - to grow.
 */
int
_NhlAddLayer
#if	NhlNeedProto
(
	NhlLayer	l	/* The layer to enter into the table	*/
)
#else
(l)
	NhlLayer	l;	/* The layer to enter into the table	*/
#endif
{
	register int i;
	static int last_alloc = 0;

	/*
	 * Increase size of table if needed
	 */
	if(table_len < num_layers + 11){
		LayerTable = NhlRealloc(LayerTable,
		(unsigned)((table_len + _NhlLAYERLISTINC) * sizeof(NhlLayer)));
		if(LayerTable == NULL){
			NhlPError(NhlFATAL,12,
				"Unable to Increase size of NhlLayer Table");
			return(NhlFATAL);
		}

		for(i=table_len; i < (table_len + _NhlLAYERLISTINC); i++)
			LayerTable[i] = (NhlLayer)NULL;
		table_len += _NhlLAYERLISTINC;
	}

	for(i=last_alloc; i < table_len; i++){
		if(LayerTable[i] == (NhlLayer)NULL){
			LayerTable[i] = l;
			num_layers++;
			l->base.id = i+1;
			last_alloc = i;
			return(i+1);
		}
	}
	/*
	 * If we fall threw to here, it means there wasn't any free space
	 * at the end of the list.  We need to start searching again
	 * from the begining of the list since there should be at least
	 * 10 free spaces.
	 */
	for(i=0;i < last_alloc; i++){
		if(LayerTable[i] == (NhlLayer)NULL){
			LayerTable[i] = l;
			num_layers++;
			l->base.id = i+1;
			last_alloc = i;
			return(i+1);
		}
	}

	/*
	 * Error message!
	 */
	return(NhlFATAL);
}

/*
 * Function:	_NhlRemoveLayer
 *
 * Description:	This function is used to remove a layer from the global NhlLayer
 *		Table.  
 *
 * In Args:	NhlLayer	l	The layer to remove from the table
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
_NhlRemoveLayer
#if	NhlNeedProto
(
	NhlLayer	l	/* The layer to remove from the table	*/
)
#else
(l)
	NhlLayer	l;	/* The layer to remove from the table	*/
#endif
{
	register int index;

	if(l == (NhlLayer)NULL){
		/*
		 * ERROR - request to remove a null layer
		 */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
					"_NhlRemoveLayer:Can't rm NULL layer");
		return(NhlWARNING);
	}

	index = l->base.id-1;
	if(LayerTable[index] != l){
		/*
		 * ERROR - layers id doesn't match internal table
		 */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"_NhlRemoveLayer:layer id doesn't match layer Table");
		return(NhlWARNING);
	}

	LayerTable[index] = (NhlLayer)NULL;
	num_layers--;

	return(NhlNOERROR);
}

/*
 * Function:	DestroyLayerTree
 *
 * Description:	This function takes a layer id, and traverses to the top
 *		level parent of that object and destroys it.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
DestroyLayerTree
#if	NhlNeedProto
(
	int	id
)
#else
(id)
	int	id;
#endif
{
	NhlLayer	l = _NhlGetLayer(id);

	if(l == NULL)
		return;

	/*
	 * 1 should be the id of the default app_class object, so we really
	 * don't want to destroy it until last, so we don't follow the
	 * parent tree all the way to the top.
	 */
	if(l->base.parent != NULL){
		DestroyLayerTree(l->base.parent->base.id);
		return;
	}

	(void)NhlDestroy(id);

	return;
}

/*
 * Function:	_NhlDestroyLayerTable
 *
 * Description:	This function is used to clean the LayerTable for close.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlDestroyLayerTable
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	int i;

	for(i=table_len-1;i >= 0 && num_layers > 0;i--){
		if(LayerTable[i] != NULL)
			DestroyLayerTree(i+1);
		if(LayerTable[i] != NULL)
			NhlPError(NhlWARNING,NhlEUNKNOWN,
					"Unable to destroy layer %d ???",i+1);
	}

	if (num_layers > 0)
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Not all Layers destroyed?");

	table_len = 0;
	num_layers = 0;
	(void)NhlFree(LayerTable);
	LayerTable = NULL;

	return;
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
#if	NhlNeedProto
(
	int	pid	/* id of a plot	*/
)
#else
(pid)
	int	pid;	/* id of a plot	*/
#endif
{
	NhlLayer	tmp = _NhlGetLayer(pid);

	if(tmp == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Unable to access object with id:%d",pid);
		return ("(null)");
	}

	return tmp->base.name;
}

/*
 * Function:	nhlpfname
 *
 * Description:	Fortran callable function to return the name of an object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	fills the "name" parameter with the name or " "'s
 * Side Effect:	
 */
/*ARGSUSED*/
void
_NHLCALLF(nhlpfname,NHLPFNAME)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	name,
	int		*name_len,
	int		*err
)
#else
(id,name,name_len,err)
	int		*id;
	_NhlFString	name;
	int		*name_len;
	int		*err;
#endif
{
	Const char	*cstr;

	cstr = NhlName(*id);
	*err = _NhlCstrToFstr(name,*name_len,(NhlString)cstr);

	return;
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
#if	NhlNeedProto
(
	int pid
)
#else
(pid)
int pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l != NULL) {
		return(_NhlClassName(_NhlClass(l)));
	} else {
		return(NULL);
	}
}

/*
 * Function:	nhlpfclassname
 *
 * Description:	Fortran callable function to return the classname of an object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	fills the "name" parameter with the name or " "'s
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfclassname,NHLPFCLASSNAME)
#if	NhlNeedProto
(
	int		*id,
	_NhlFString	name,
	int		*name_len,
	int		*err
)
#else
(id,name,name_len,err)
	int		*id;
	_NhlFString	name;
	int		*name_len;
	int		*err;
#endif
{
	Const char	*cstr;

	cstr = NhlClassName(*id);
	*err = _NhlCstrToFstr(name,*name_len,(NhlString)cstr);
	return;
}

/*
 * Function:	_NhlClassName
 *
 * Description:	This function returns the name of a given class
 *
 * In Args:	NhlClass	lc;	pointer to class struct
 *
 * Out Args:	
 *
 * Scope:	global public
 * Returns:	Const char *
 * Side Effect:	
 */
Const char *
_NhlClassName
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class struct	*/
)
#else
(lc)
	NhlClass	lc;	/* pointer to class struct	*/
#endif
{
	if(lc != NULL) {
		return lc->base_class.class_name;
	} else {
		return(NULL);
	}
}

/*
 * Function:	_NhlClass
 *
 * Description:	This function returns the given NhlLayer's class pointer.
 *
 * In Args:	NhlLayer l;	layer
 *
 * Out Args:	
 *
 * Scope:	Global, Public
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NhlClass
#if	NhlNeedProto
(
	NhlLayer l		/* Instance pointer */
)
#else
(l)
	NhlLayer l;
#endif
{
	if(l != NULL ) {
		return(l->base.layer_class);
	} else {
		return(NULL);
	}
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
#if	NhlNeedProto
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
NhlDOCTAG(NhlSetSArg)
/*VARARGS2*/
void
NhlSetSArg
#if	NhlNeedVarArgProto
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

	arg->name = resname;

	VA_START(ap,resname);
	if(_NhlIsFloatRes(resname)){
		arg->value.dblval = va_arg(ap,double);
	}
	else
		arg->value.lngval = va_arg(ap,long);

	va_end(ap);
}

int
_NhlCompareArg
#if	NhlNeedProto
(
	Const void	*ov,
	Const void	*tv
)
#else
(ov,tv)
	Const void	*ov;
	Const void	*tv;
#endif
{
	_NhlArg	*one = (_NhlArg*)ov;
	_NhlArg	*two = (_NhlArg*)tv;

	if(one->quark < two->quark)
		return -1;
	else if(one->quark > two->quark)
		return 1;
	return 0;
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
#if	NhlNeedProto
(
	_NhlArgList	args,	/* arglist return	*/
	NhlSArgList	sargs,	/* public arglist	*/
	int		nargs	/* num args		*/
)
#else
(args,sargs,nargs)
	_NhlArgList	args;	/* arglist return	*/
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

	qsort(args,nargs,sizeof(_NhlArg),_NhlCompareArg);

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
#if	NhlNeedVarArgProto
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
	arg->value.ptrval = va_arg(ap,NhlPointer);
	va_end(ap);
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
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
				"_NhlInherit- Inheritance resolved improperly");
	return;
}

/*
 * Function:	_NhlArgIsSet
 *
 * Description:	
 *		Returns the relative placement of the arg in the list if it
 *		is there. (1 + the index)  Returns False (0) if arg is not
 *		in the list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlBoolean
 * Side Effect:	
 */
int _NhlArgIsSet
#if     NhlNeedProto
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
			return(i+1);
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
_NhlAllocCreateGenArray
#if	NhlNeedProto
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions,/* number of dimensions	*/
	NhlBoolean	copy_data,	/* copy data pointer?	*/
	_NhlAllocFunc	alloc_func	/* func to alloc	*/
)
#else
(data,type,size,num_dimensions,len_dimensions,copy_data,alloc_func)
	NhlPointer	data;			/* data array		*/
	NhlString	type;			/* type of each element	*/
	unsigned int	size;			/* size of each element	*/
	int	num_dimensions;		/* number of dimensions	*/
	ng_size_t	*len_dimensions;	/* number of dimensions	*/
	NhlBoolean	copy_data;		/* copy data pointer?	*/
	_NhlAllocFunc	alloc_func;		/* func to alloc	*/
#endif
{
	static NrmQuark		QString = NrmNULLQUARK;
	NhlGenArray		gen = NULL;
	int			i;

	if(QString == NrmNULLQUARK)
		QString = NrmStringToQuark(NhlTString);

	gen = alloc_func(sizeof(NhlGenArrayRec));

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

		return gen;
	}

	/*
	 * HACK!!! - If there is only one dim - then len_dimensions can
	 * be NULL - with the num_elements in num_dimensions.
	 */
	if(!len_dimensions || (num_dimensions == 1)){
		gen->num_dimensions = 1;
		if(!len_dimensions)
			gen->num_elements = num_dimensions;
		else
			gen->num_elements = *len_dimensions;
		gen->len_dimensions = &gen->num_elements;
	}
	else{
		gen->num_dimensions = num_dimensions;
		gen->len_dimensions = alloc_func(num_dimensions * sizeof(ng_size_t));
		if(gen->len_dimensions == NULL)
			return NULL;
		gen->num_elements = 1;
		for(i=0;i < num_dimensions;i++){
			gen->len_dimensions[i] = len_dimensions[i];
			gen->num_elements *= len_dimensions[i];
		}
	}

	if(copy_data && data){
		gen->data = alloc_func(gen->num_elements * gen->size);
		if(gen->data == NULL)
			return NULL;

		/*
		 * If the individual elements are NhlString's then we
		 * know how to copy them.
		 */
		if((gen->typeQ == QString) && (gen->size == sizeof(NhlString))){
			NhlString	*otable = data;
			NhlString	*ntable = gen->data;

			for(i=0;i<gen->num_elements;i++){
				if(otable[i] == NULL){
					ntable[i] = NULL;
				}
				else{
					ntable[i] =
						alloc_func(strlen(otable[i])+1);
					if(ntable[i] == NULL)
						return NULL;
					strcpy(ntable[i],otable[i]);
				}
			}
		}
		else{
			memcpy(gen->data,data,gen->num_elements * gen->size);
		}
		gen->my_data = True;
	}
	else if(copy_data){
		gen->data = NULL;
		gen->my_data = True;
	}
	else{
		gen->data = data;
		gen->my_data = False;
	}

	return gen;
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
#if	NhlNeedProto
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions,/* number of dimensions	*/
	NhlBoolean	copy_data	/* copy data pointer?	*/
)
#else
(data,type,size,num_dimensions,len_dimensions,copy_data)
	NhlPointer	data;			/* data array		*/
	NhlString	type;			/* type of each element	*/
	unsigned int	size;			/* size of each element	*/
	int		num_dimensions;		/* number of dimensions	*/
	ng_size_t	*len_dimensions;	/* number of dimensions	*/
	NhlBoolean	copy_data;		/* copy data pointer?	*/
#endif
{
	return _NhlAllocCreateGenArray(data,type,size,num_dimensions,
					len_dimensions,copy_data,NhlMalloc);
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
#if	NhlNeedProto
(
	NhlPointer	data,		/* data array		*/
	NhlString	type,		/* type of each element	*/
	unsigned int	size,		/* size of each element	*/
	int		num_dimensions,	/* number of dimensions	*/
	ng_size_t	*len_dimensions	/* number of dimensions	*/
)
#else
(data,type,size,num_dimensions,len_dimensions)
	NhlPointer	data;			/* data array		*/
	NhlString	type;			/* type of each element	*/
	unsigned int	size;			/* size of each element	*/
	int		num_dimensions;		/* number of dimensions	*/
	ng_size_t	*len_dimensions;	/* number of dimensions	*/
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
#if	NhlNeedProto
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
	if(!gen)
		return NULL;

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
#if	NhlNeedProto
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
			int	 i;
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
 *		validating the user-supplied gfrom Generic Array. It need not
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
 * Returns:     If successful NhlNOERROR; NhlFATAL on memory allocation errors;
 *              NhlWARNING if the from GenArray is invalid in some way.
 *
 * Side Effect: When string data is copied, all NULL string pointers are 
 *		replaced with single-byte strings containing only a NULL
 *		terminator.
 *
 */

NhlErrorTypes _NhlValidatedGenArrayCopy
#if NhlNeedProto
	(NhlGenArray	*gto, 
	 NhlGenArray	gfrom,
	 ng_size_t	max_el,
	 NhlBoolean	copy_data,
	 NhlBoolean	exact_count,
	 char		*res_name,
	 char		*caller)
#else
(gto,gfrom,max_el,copy_data,exact_count,res_name,caller)
	NhlGenArray	*gto; 
	NhlGenArray	gfrom;
	ng_size_t	max_el;
	NhlBoolean	copy_data;
	NhlBoolean	exact_count;
	char		*res_name;
	char		*caller;
#endif
{
	char		*e_text;
	ng_size_t	i;
	ng_size_t	el_count;
	static NrmQuark Qstring;
	static NhlBoolean first = True;

	if (first) {
		Qstring = NrmStringToQuark(NhlTString);
		first = False;
	}

	if (gfrom == NULL || (*gto) == NULL) {
		e_text = 
		 "%s: %s NULL array passed in: copy not performed";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING;
	}
	if (gfrom->num_elements <= 0) {
		e_text = 
		 "%s: %s invalid element count: ignoring";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING;
	}
	else if (gfrom->num_elements > max_el) {
		e_text =
		 "%s: %s exceeds maximum number of elements, %d: ignoring";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name,max_el);
		return NhlWARNING;
	}
	if (gfrom->num_dimensions != (*gto)->num_dimensions) {
		e_text = 
		 "%s: %s invalid dimensionality: copy not performed";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING;
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
			return NhlWARNING;
		}
	}
	if (el_count != gfrom->num_elements) {
		e_text = 
		 "%s: %s invalid element count: copy not performed";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING;
	}

	if (!_NhlIsSubtypeQ((*gto)->typeQ,gfrom->typeQ)){
		e_text = "%s: %s type mismatch: copy not performed";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING; 
	}
	if (gfrom->size != (*gto)->size) {
		e_text = "%s: %s type size mismatch: copy not performed";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,caller,res_name);
		return NhlWARNING; 
	}
	
	if (gfrom->num_elements > (*gto)->num_elements) {
		NhlFreeGenArray((*gto));
		if (((*gto) = _NhlCopyGenArray(gfrom,copy_data)) == NULL) {
			e_text = "%s: error copying %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,caller,res_name);
			return NhlFATAL;
		}
		return NhlNOERROR;
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						  caller,res_name);
					return NhlFATAL;
				}
				strcpy(*to,"");
			}
			else if (*to == NULL) {
				*to = (NhlString) NhlMalloc(strlen(*from)+1);
				if (*to == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						  caller,res_name);
					return NhlFATAL;
				}
				strcpy(*to,*from);
			}
			else if (strcmp(*to,*from)) {
				*to = (NhlString) 
					NhlRealloc(*to, strlen(*from)+1);
				if (*to == NULL) {
					e_text = "%s: error copying %s string";
					NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						  caller,res_name);
					return NhlFATAL;
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

	return NhlNOERROR;

}

/*
 * Function:	_NhlTmpFile
 *
 * Description:	This function emulates the Standard C function 'tmpfile'
 *		except that it allows the directory location of the file
 *		to be controlled. If the environment variable "TMPDIR"
 *		is set the file will be located in TMPDIR; else it will
 *		located in the directory given by GetNCARGPath("tmp"). 
 *		Since the file is immediately unlinked, its name is 
 *		irrelevant.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	FILE * or NULL on failure
 * Side Effect:	
 */
FILE *
_NhlTmpFile
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	char *entry_name = "_NhlTmpName";
	struct stat sbuf;
	char *e_text;
	char buffer[_NhlMAXFNAMELEN+20];
	char *ptr;
	FILE *fp;
	static int	a = 0;
	int tint;

	strcpy(buffer,(char *)GetNCARGPath("tmp"));
	if(stat(buffer,&sbuf) != 0){
		NhlPError(NhlFATAL,errno,"Unable to stat \"%s\" tmp dir",
									buffer);
		return NULL;
	}

	strcat(buffer,_NhlPATHDELIMITER);
	strcat(buffer,"HLU");

	ptr = buffer + strlen(buffer);
	sprintf(ptr,"%d",getpid());
	ptr = buffer + strlen(buffer) + 3;
	*ptr-- = '\0';
	tint = a++;
	*ptr-- = (tint % 10) + '0';
	tint /= 10;
	*ptr-- = (tint % 10) + '0';
	tint /= 10;
	*ptr = (tint % 10) + '0';

	if ((fp = fopen(buffer,"w+")) == NULL) {
		e_text = "%s, error opening temporary file";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	if (unlink(buffer) < 0) {
		e_text = "%s, error unlinking temporary file";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NULL;
	}

	return fp;
}

/*
 * Function:	_NhlCopyToVal
 *
 * Description:	copies the memory pointed to by "src" to an _NhlArgVal union.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void
_NhlCopyToVal
#if	NhlNeedProto
(
	NhlPointer	src,
	_NhlArgVal	*dst,
	unsigned int	size
)
#else
(src,dst,size)
	NhlPointer	src;
	_NhlArgVal	*dst;
	unsigned int	size;
#endif
{ 
    if      (size == sizeof(long)) dst->lngval = *(long*)src;
    else if (size == sizeof(short)) dst->shrtval = *(short*)src;
    else if (size == sizeof(unsigned short)) dst->ushortval = *(unsigned short*)src;
    else if (size == sizeof(NhlPointer)) dst->ptrval = *(NhlPointer*)src;
    else if (size == sizeof(char))	dst->charval = *(char*)src;
    else if (size == sizeof(char*))	dst->strval = *(char**)src;
    else if (size == sizeof(_NhlArgVal)) *dst = *(_NhlArgVal*)src;
    else
        memcpy((NhlPointer)dst,(NhlPointer)&src,size);
}

/*
 * Function:	_NhlLLErrCheckPrnt
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
_NhlLLErrCheckPrnt
#if	NhlNeedProto
(
	NhlErrorTypes	level,
	NhlString	calling_func
)
#else
(level,calling_func)
	NhlErrorTypes	level;
	NhlString	calling_func;
#endif
{
	int	err_num;

	if(c_nerro(&err_num)){
		if(err_num != _NhlGKSERRNUM){
			if(calling_func)
			NhlPError(level,NhlEUNKNOWN,"%s:libncarg Error:%s",
						calling_func,c_semess(0));
			else
			NhlPError(level,NhlEUNKNOWN,"libncarg Error:%s",
								c_semess(0));
		}
		c_errof();
		return True;
	}

	return False;
}

/*
 * Function:	_NhlResUnset
 *
 * Description:	This function is used to indicate that a particular resource
 *		was not set.  It is used as the NhlTProcedure function
 *		in the resource list for the resource in question.  It
 *		relies on the the NhlBoolean "is_set" resource to be
 *		directly before it in the Layer structure.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlErrorTypes
_NhlResUnset
#if	NhlNeedProto
(
	NrmName		name,
	NrmClass	cname,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,cname,base,offset)
	NrmName		name;
	NrmClass	cname;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	char *cl = (char *)base;
	NhlBoolean *set = (NhlBoolean *)(cl + offset - sizeof(NhlBoolean));

	*set = False;

	return NhlNOERROR;
}

/*
 * Function:	NhlClassIsSubclass
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
NhlClassIsSubclass
#if	NhlNeedProto
(
	NhlClass	cptr,
	NhlClass	ref_cptr
)
#else
(cptr,ref_cptr)
	NhlClass	cptr;
	NhlClass	ref_cptr;
#endif
{
	if(!cptr || !ref_cptr)
		return False;

	if(cptr == ref_cptr)
		return True;

	return NhlClassIsSubclass(cptr->base_class.superclass,ref_cptr);
}

/*
 * Function:	_NhlIsClass
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
_NhlIsClass
#if	NhlNeedProto
(
	NhlLayer	l,
	NhlClass	cl
)
#else
(l,cl)
	NhlLayer	l;
	NhlClass	cl;
#endif
{
	if(!l || !cl)
		return False;

	return NhlClassIsSubclass(l->base.layer_class,cl);
}

/*
 * Function:	NhlIsClass
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
NhlIsClass
#if	NhlNeedProto
(
	int		id,
	NhlClass	cl
)
#else
(id,cl)
	int		id;
	NhlClass	cl;
#endif
{
	NhlLayer	l = _NhlGetLayer(id);

	if(!l)
		return False;

	return _NhlIsClass(l,cl);
}

/*
 * Function:	NhlClassOfObject
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlClass
NhlClassOfObject
#if	NhlNeedProto
(
	int	id
)
#else
(id)
	int	id;
#endif
{
	NhlLayer	l = _NhlGetLayer(id);

	if(!l)
		return NULL;

	return _NhlClass(l);
}

/*
 * Function:	_NhlGetWorkstationLayer
 *
 * Description:	This function is used to retrieve the Workstation pointer
 *		from a layer object.
 *
 * In Args:	
 *		NhlLayer	layer	Layer to get workstation pointer from
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlLayer
 * Side Effect:	
 */
NhlLayer
_NhlGetWorkstationLayer
#if	NhlNeedProto
(
	NhlLayer	layer
)
#else
(layer)
	NhlLayer	layer;
#endif
{
	if(_NhlIsBase(layer))
		return layer->base.wkptr;
	return NULL;
}

int
NhlGetParentWorkstation
(
	int	plotid
)
{
	NhlLayer	plot_ptr = _NhlGetLayer(plotid);

	if(!plot_ptr || !_NhlIsBase(plot_ptr) || !plot_ptr->base.wkptr)
		return -1;

	return plot_ptr->base.wkptr->base.id;
}

static _NhlCBList
GetObjCBList
(
	NhlLayer	l,
	NhlString	cbname,
	NhlBoolean	create
)
{
	NhlClass		lc = l->base.layer_class;
	_NhlCookedObjCBList	cbl =
				(_NhlCookedObjCBList)lc->base_class.callbacks;
	int			ncbl = lc->base_class.num_callbacks;
	int			i;
	_NhlCBList		*cblptr;
	NrmQuark		cbquark = NrmStringToQuark(cbname);

	if(!ncbl || !cbl)
		return NULL;

	for(i=0;i<ncbl;i++){
		if(cbl[i].cbquark == cbquark){
			cblptr = (_NhlCBList *)((char*)l + cbl[i].offset);
			if(create && !*cblptr){
				*cblptr = _NhlCBCreate(cbl[i].hash_mult,
						       cbl[i].add_hash,
						       cbl[i].call_hash,
						       cbl[i].task_proc,
						       (NhlPointer)l);
			}
			return *cblptr;
		}
	}

	return NULL;
}

/*
 * Function:	_NhlAddObjCallback
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
_NhlCB
_NhlAddObjCallback
(
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	func,
	NhlArgVal	udata
)
{
	return _NhlCBAdd(GetObjCBList(l,cbname,True),sel,func,udata);
}

/*
 * Function:	_NhlCallObjCallbacks
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void
_NhlCallObjCallbacks
(
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	NhlArgVal	cbdata
)
{
	_NhlCBCallCallbacks(GetObjCBList(l,cbname,False),sel,cbdata);
	return;
}


/*
 * Function:	_NhlIterateObjCallbacks
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void
_NhlIterateObjCallbacks
(
	NhlLayer	l,
	NhlString	cbname,
 	_NhlCBTask	task,
	NhlArgVal	cbdata
)
{
	_NhlCBIterate(GetObjCBList(l,cbname,False),task,cbdata);
	return;
}

static _NhlCBList
GetClassCBList
(
	NhlClass	lc,
	NhlString	cbname,
	NhlBoolean	create
)
{
	_NhlCookedClassCBList	cbl =
			(_NhlCookedClassCBList)lc->base_class.class_callbacks;
	int			ncbl = lc->base_class.num_class_callbacks;
	int			i;
	_NhlCBList		*cblptr;
	NrmQuark		cbquark = NrmStringToQuark(cbname);

	if(!ncbl || !cbl)
		return NULL;

	for(i=0;i<ncbl;i++){
		if(cbl[i].cbquark == cbquark){
			cblptr = (_NhlCBList *)&cbl[i].cblist;
			if(create && !*cblptr){
				*cblptr = _NhlCBCreate(cbl[i].hash_mult,
						       cbl[i].add_hash,
						       cbl[i].call_hash,
						       cbl[i].task_proc,
						       (NhlPointer)lc);
			}
			return *cblptr;
		}
	}

	return NULL;
}

/*
 * Function:	_NhlAddClassCallback
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
_NhlCB
_NhlAddClassCallback
(
	NhlClass	lc,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	f,
	NhlArgVal	udata
)
{
	char		func[]="_NhlAddClassCallback";
	NhlErrorTypes	ret;

	ret = _NhlInitializeClass(lc);
	if(ret < NhlWARNING){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"%s:Unable to add class callback to uninitialized class",func));
		return NULL;
	}

	return _NhlCBAdd(GetClassCBList(lc,cbname,True),sel,f,udata);
}

/*
 * Function:	_NhlCallClassCallbacks
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void
_NhlCallClassCallbacks
(
	NhlClass	lc,
	NhlString	cbname,
	NhlArgVal	sel,
	NhlArgVal	cbdata
)
{
	_NhlCBCallCallbacks(GetClassCBList(lc,cbname,False),sel,cbdata);
	return;
}


/*
 * Function:	_NhlIterateClassCallbacks
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void
_NhlIterateClassCallbacks
(
	NhlClass	lc,
	NhlString	cbname,
 	_NhlCBTask	task,
	NhlArgVal	cbdata
)
{
	_NhlCBIterate(GetClassCBList(lc,cbname,False),task,cbdata);
	return;
}
