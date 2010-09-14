/*
 *      $Id: niohlu.c,v 1.1 2009-05-15 00:49:28 dbrown Exp $
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
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#include "nioBaseP.h"

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
#if	NhlNeedProto
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
		NhlPError(NhlFATAL,errno,"NhlMalloc Failed");

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
#if	NhlNeedProto
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
	int		num_dimensions;		/* number of dimensions	*/
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
		gen->len_dimensions = alloc_func(num_dimensions * sizeof(int));
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

#if 0
/*
 * Function:	InitializeClass
 *
 * Description:	This function is used to Initialize the Class structure
 *		of the given class.  It calls the Initialize function
 *		within the given class to do this.  Also, It initializes
 *		the given Classes superclass before doing itself.
 *
 * In Args:	NhlClass	lc;	Class to initialize
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	The structure pointed to by lc is initialized.
 */
static NhlErrorTypes
InitializeClass
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	NhlErrorTypes ansestor, thisclass, classpart;
	NhlClass step;
	int inited = 0x01;

	if(lc->base_class.class_inited) return(NhlNOERROR);

	step = lc;
	while(step != NULL) {
		if(step == NhlobjClass)
			inited |= (_NhlObjClassFlag);
		else if(step == NhlbaseClass)
			inited |= ( _NhlBaseClassFlag );  
		step = step->base_class.superclass;
	}

	if((lc->base_class.superclass != NULL)
		&&(!(lc->base_class.superclass->base_class.class_inited))){

		ansestor = InitializeClass(lc->base_class.superclass);
		if(ansestor < NhlWARNING)
			return(ansestor);
	}
	else
		ansestor = NhlNOERROR;

	if(lc->base_class.class_initialize != NULL){
		thisclass = (*(lc->base_class.class_initialize))();
		if(thisclass < NhlWARNING)
			return(thisclass);
	}
	else
		thisclass = NhlNOERROR;

	lc->base_class.class_inited = inited;


	return(MIN(ansestor,thisclass));
}

/*
 * Function:	_NhlInitializeClass
 *
 * Description:	Global function to init a layer class. This is needed if
 *		The class you are writing depends upon a type defined
 *		in another class.  The converters for the type need to
 *		be installed.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlDOCTAG(_NhlInitializeClass)
NhlErrorTypes
_NhlInitializeClass
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class to be initalized	*/
) 
#else
(lc) 
	NhlClass	lc;	/* pointer to class to be initalized	*/
#endif
{
	return(InitializeClass(lc));
}
#endif
