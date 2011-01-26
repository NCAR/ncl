/*
 *      $Id: ResList.c,v 1.21 2009-07-10 19:54:04 huangwei Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ResList.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Feb 3 16:04:23 MST 1994
 *
 *	Description:	This file contains fuctions and data used to
 *			impliment the RL interface to the hlu libarary.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/VarArg.h>

static _NhlRLHead *ListTable = NULL;
static int num_lists = 0;
static int table_len = 0;
static NrmQuark	floatQ;
static NrmQuark	intQ;
static NrmQuark	longQ;
static NrmQuark	stringQ;
static NrmQuark	genQ;
static NrmQuark	pointerQ;
static NrmQuark	expMDQ;
static NrmQuark	expMDTypeQ;
static NrmQuark	expQ;
static NrmQuark	expTypeQ;
static NrmQuark	charQ;
static NrmQuark	byteQ;
static NrmQuark	shortQ;
static NrmQuark	doubleQ;
static NrmQuark	int64Q;
static NrmQuark	ushortQ;
static NrmQuark	uintQ;
static NrmQuark	ulongQ;
static NrmQuark	uint64Q;

static void InitRLList(
#if	NhlNeedProto
	void
#endif
);

/*
 * Function:	GetHead
 *
 * Description:	returns a pointer to the HEAD given the id
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlRLHead
 * Side Effect:	
 */
static _NhlRLHead*
GetHead
#if	NhlNeedProto
(
	int	id
)
#else
(id)
	int	id;
#endif
{
	if((id < 1) || (id > table_len) || (ListTable[id-1] == NULL)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid RL id %d",id);
		return NULL;
	}

	return &ListTable[id-1];
}

/*
 * Function:	CreateNode
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
static _NhlRLNode
CreateNode
#if	NhlNeedProto
(
	int		nameQ,
	int		typeQ,
	_NhlArgVal	value,
	unsigned int	size,
	_NhlFreeFunc	free_func
)
#else
(nameQ,typeQ,value,size,free_func)
	int		nameQ;
	int		typeQ;
	_NhlArgVal	value;
	unsinged int	size;
	_NhlFreeFunc	free_func;
#endif
{
	_NhlRLNode	node = NhlMalloc(sizeof(_NhlRLNodeRec));

	if(node == NULL)
		return NULL;

	node->nameQ = nameQ;
	node->typeQ = typeQ;
	node->value = value;
	node->size = size;
	node->free_func = free_func;
	node->left = NULL;
	node->right = NULL;

	return node;
}

/*
 * Function:	GetNodePtr
 *
 * Description:	return pointer to pointer that should contain the given nameQ.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	_NhlRLNode*
 * Side Effect:	
 */
static _NhlRLNode*
GetNodePtr
#if	NhlNeedProto
(
	_NhlRLNode	*head,
	int		nameQ
)
#else
(head,nameQ)
	_NhlRLNode	*head;
	int		nameQ;
#endif
{
	if((*head == NULL) || (nameQ == (*head)->nameQ)){
		return head;
	}

	if(nameQ < (*head)->nameQ)
		return GetNodePtr(&(*head)->left,nameQ);
	else
		return GetNodePtr(&(*head)->right,nameQ);

}

/*
 * Function:	CleanNode
 *
 * Description:	This function is used to clean the memory out of a given
 *		node.
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
CleanNode
#if	NhlNeedProto
(
	_NhlRLNode	node
)
#else
(node)
	_NhlRLNode	node;
#endif
{
	if(node == NULL)
		return;

	if(node->free_func)
		(*(node->free_func))((NhlPointer)node->value.ptrval);

	return;
}

/*
 * Function:	_NhlRLInsert
 *
 * Description:	Insert this info into the given RL list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlBoolean - True if successful
 * Side Effect:	
 */
NhlBoolean
_NhlRLInsert
#if	NhlNeedProto
(
	int		id,
	NhlRLType	type_action,
	int		nameQ,
	int		typeQ,
	_NhlArgVal	value,
	unsigned int	size,
	_NhlFreeFunc	free_func
)
#else
(id,type_action,nameQ,typeQ,value,size,free_func)
	int		id;
	NhlRLType	type_action;
	int		nameQ;
	int		typeQ;
	_NhlArgVal	value;
	unsigned int	size;
	_NhlFreeFunc	free_func;
#endif
{
	_NhlRLHead	*head;
	_NhlRLNode	*node;

	head = GetHead(id);

	if(head == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlRLInsert:Unable to retrieve RL list id=%d",id);
		return False;
	}

	if((*head)->list_type != type_action){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"_NhlRLInsert:RL list id=%d has wrong NhlRLType for action requested",
									id);
		return False;
	}

	node = GetNodePtr(&(*head)->list,nameQ);

	if(*node != NULL){
		CleanNode(*node);
		(*node)->typeQ = typeQ;
		(*node)->value = value;
		(*node)->size = size;
		(*node)->free_func = free_func;
	}
	else{
		if((*head)->num >= _NhlMAXARGLIST){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
						"RL cannot get any larger");
			return False;
		}
		*node = CreateNode(nameQ,typeQ,value,size,free_func);
		if(*node == NULL){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return False;
		}
		(*head)->num++;
	}

	return True;
}

/*
 * Function:	NhlRLCreate
 *
 * Description:	This function is used to allocate an RL list.
 *
 * In Args:	
 *		NhlRLType	list_type
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	int
 * Side Effect:	
 */
int
NhlRLCreate
#if	NhlNeedProto
(
	NhlRLType	list_type
)
#else
(list_type)
	NhlRLType	list_type;
#endif
{
	static NhlBoolean	initialized = False;
	register int		i;
	_NhlRLHead		new = NULL;

	if(!initialized){
		InitRLList();
		initialized = True;
	}

	/*
	 * Increase size of table if needed
	 */
	if(table_len < num_lists + 1){
		ListTable = NhlRealloc(ListTable,(unsigned)
			((table_len+_NhlLAYERLISTINC)*sizeof(_NhlRLHead)));
		if(ListTable == NULL){
			NhlPError(NhlFATAL,ENOMEM,
					"Unable to Allocate an RL list");
			table_len = 0;
			num_lists = 0;
			return NhlFATAL;
		}

		for(i=table_len; i < (table_len + _NhlLAYERLISTINC); i++)
			ListTable[i] = (_NhlRLHead)NULL;
		table_len += _NhlLAYERLISTINC;
	}

	new = NhlMalloc(sizeof(_NhlRLHeadRec));
	if(new == NULL){
		NhlPError(NhlFATAL,ENOMEM,"Unable to Allocate an RL list");
		return NhlFATAL;
	}
	new->num = 0;
	new->list_type = list_type;
	new->list = NULL;

	for(i=0; i < table_len; i++){
		if(ListTable[i] == (_NhlRLHead)NULL){
			ListTable[i] = new;
			num_lists++;
			return(i+1);
		}
	}

	/*
	 * shouldn't get here.
	 */
	NhlPError(NhlFATAL,NhlEUNKNOWN,"How did this happen?");
	return NhlFATAL;
}

/*
 * Function:	FreeRLNode
 *
 * Description:	free an RL node tree.
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
FreeRLNode
#if	NhlNeedProto
(
	_NhlRLNode	node
)
#else
(node)
	_NhlRLNode	node;
#endif
{
	if(node == NULL)
		return;

	FreeRLNode(node->left);
	FreeRLNode(node->right);

	CleanNode(node);
	NhlFree(node);

	return;
}

/*
 * Function:	NhlRLDestroy
 *
 * Description:	This function is used to free an RL list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
NhlRLDestroy
#if	NhlNeedProto
(
	int	id
)
#else
(id)
	int	id;
#endif
{
	_NhlRLHead	*head;

	if(!id)
		return;

	head = GetHead(id);

	if(head == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlRLDestroy:Unable to retrieve RL list id=%d",id);
		return;
	}

	FreeRLNode((*head)->list);
	NhlFree(*head);
	*head = NULL;
	num_lists--;

	return;
}

/*
 * Function:	NhlRLClear
 *
 * Description:	Clear out all the resource specifications in a given RL list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
NhlRLClear
#if	NhlNeedProto
(
	int	id
)
#else
(id)
	int	id;
#endif
{
	_NhlRLHead	*head;

	head = GetHead(id);

	if(head == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlRLDestroy:Unable to retrieve RL list id=%d",id);
		return;
	}

	FreeRLNode((*head)->list);
	(*head)->list = NULL;
	(*head)->num = 0;

	return;
}

/*
 * Function:	NhlRLUnSet
 *
 * Description:	Used to remove a res specification from an RL list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global
 * Returns:	void
 * Side Effect:	
 */
void
NhlRLUnSet
#if	NhlNeedProto
(
	int		id,
	NhlString	name
)
#else
(id,name)
	int		id;
	NhlString	name;
#endif
{
	_NhlRLHead	*head;
	_NhlRLNode	*node,*node2,tnode;
	NrmQuark	nameQ = NrmStringToQuark(name);

	head = GetHead(id);
	if(head == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlRLUnSet:Unable to find RL list with id=%d",id);
		return;
	}

	node = GetNodePtr(&(*head)->list,nameQ);

	if(*node != NULL){
		/*
		 * Move everything from the right tree into the left one,
		 * then move the left one up to the current level and
		 * free the node.
		 */
		tnode = *node;
		if ((*node)->right) {
			node2 = GetNodePtr(&(*node)->left,
					   (*node)->right->nameQ);
			*node2 = (*node)->right;
		}
		*node = (*node)->left;
		tnode->left = NULL;
		tnode->right = NULL;
		FreeRLNode(tnode);
		(*head)->num--;
	}

	return;
}

/*
 * Function:	NhlRLIsSet
 *
 * Description:	Allows the user to query if a given resource name is already
 *		specified in the RL list.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlBoolean
 * Side Effect:	
 */
NhlBoolean
NhlRLIsSet
#if	NhlNeedProto
(
	int		id,
	NhlString	name
)
#else
(id,name)
	int		id;
	NhlString	name;
#endif
{
	_NhlRLHead	*head;
	_NhlRLNode	*node;
	NrmQuark	nameQ = NrmStringToQuark(name);

	head = GetHead(id);
	if(head == NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlRLIsSet:Unable to find RL list with id=%d",id);
		return False;
	}

	node = GetNodePtr(&(*head)->list,nameQ);

	if(*node == NULL)
		return False;
	else
		return True;
}

/*
 * Function:	NhlRLSet
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSet
#if	NhlNeedVarArgProto
(
	int		id,
	NhlString	name,
	NhlString	type,
	...
)
#else
(id,name,type,va_alist)
	int		id;
	NhlString	name;
	NhlString	type;
	va_dcl
#endif
{
	char		func[]="NhlRLSet";
	va_list		ap;
	_NhlArgVal	value;
	unsigned int	size = 0;
	NrmQuark	typeQ = NrmStringToQuark(type);

	NhlINITVAR(value);
	/*
	 * default type is "int" since the ANSI standard says STDARGS should
	 * propagate all "integral" types to int.
	 */
	VA_START(ap,type);
	if(_NhlIsSubtypeQ(byteQ,typeQ)){
		value.charval = (char)va_arg(ap,int);
		size = sizeof(char);
	}
	else if(_NhlIsSubtypeQ(charQ,typeQ)){
		value.charval = (char)va_arg(ap,int);
		size = sizeof(char);
	}
	else if(_NhlIsSubtypeQ(shortQ,typeQ)){
		value.shrtval = (short)va_arg(ap,int);
		size = sizeof(short);
	}
	else if(_NhlIsSubtypeQ(ushortQ,typeQ)){
		value.ushortval = (unsigned short)va_arg(ap,int);
		size = sizeof(unsigned short);
	}
	else if(_NhlIsSubtypeQ(intQ,typeQ)){	/* gets all enumerated types */
		value.intval = va_arg(ap,int);
		size = sizeof(int);
	}
	else if(_NhlIsSubtypeQ(uintQ,typeQ)){	/* gets all enumerated types */
		value.uintval = va_arg(ap,long);
		size = sizeof(unsigned int);
	}
	else if(_NhlIsSubtypeQ(longQ,typeQ)){
		value.lngval = va_arg(ap,long);
		size = sizeof(long);
	}
	else if(_NhlIsSubtypeQ(ulongQ,typeQ)){
		value.ulongval = va_arg(ap,long long);
		size = sizeof(unsigned long);
	}
	else if(_NhlIsSubtypeQ(int64Q,typeQ)){
		value.int64val = va_arg(ap,long long);
		size = sizeof(long long);
	}
	else if(_NhlIsSubtypeQ(uint64Q,typeQ)){
		value.uint64val = va_arg(ap,unsigned long long);
		size = sizeof(unsigned long long);
	}
	else if(_NhlIsSubtypeQ(floatQ,typeQ)){
		value.fltval = (float)va_arg(ap,double);
		size = sizeof(float);
	}
	else if(_NhlIsSubtypeQ(doubleQ,typeQ)){
		value.dblval = va_arg(ap,double);
		size = sizeof(double);
	}
	else if(_NhlIsSubtypeQ(stringQ,typeQ)){
		value.strval = (NhlString)va_arg(ap,NhlString);
		size = sizeof(NhlString);
	}
	else if(_NhlIsSubtypeQ(genQ,typeQ)){	/* gets all GenArray types */
		value.ptrval = (NhlPointer)va_arg(ap,NhlPointer);
		size = sizeof(NhlGenArray);
	}
	else if(_NhlIsSubtypeQ(pointerQ,typeQ)){
		value.ptrval = (NhlPointer)va_arg(ap,NhlPointer);
		size = sizeof(NhlGenArray);
	} else if(typeQ == NrmNULLQUARK) {
		value.ptrval = (NhlPointer)va_arg(ap,NhlPointer);
		size = sizeof(NhlPointer);
	} 
 	else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Unsupported type:%s",
								func,type);
		return NhlFATAL;
	}
	va_end(ap);

	if(_NhlRLInsert(id,NhlSETRL,NrmStringToQuark(name),typeQ,value,size,
									NULL))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:    NhlRLSetShort
 *
 * Description: This function is used to set a resource with the value
 *              specified as the given type.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect:
 */
NhlErrorTypes
NhlRLSetShort
#if     NhlNeedProto
(
        int             id,
        NhlString       name,
        short           value
)
#else
(id,name,value)
        int             id;
        NhlString       name;
        short           value;
#endif
{
        return NhlRLSet(id,name,NhlTShort,value);
}

/*
 * Function:    NhlRLSetUshort
 * 
 * Description: This function is used to set a resource with the value
 *              specified as the given type. 
 *
 * In Args:
 * 
 * Out Args:
 * 
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect: 
 */
NhlErrorTypes
NhlRLSetUshort
#if     NhlNeedProto
(
        int             id,
        NhlString       name,
        unsigned short  value
)
#else
(id,name,value)
        int             id;
        NhlString       name;
        unsigned short  value;
#endif
{
        return NhlRLSet(id,name,NhlTUshort,value);
}

/*
 * Function:	NhlRLSetInteger
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetInteger
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	int		value;
#endif
{
	return NhlRLSet(id,name,NhlTInteger,value);
}

/*
 * Function:    NhlRLSetUint
 *
 * Description: This function is used to set a resource with the value
 *              specified as the given type.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect:
 */
NhlErrorTypes
NhlRLSetUint
#if     NhlNeedProto
(
        int             id,
        NhlString       name,
        unsigned int    value
)
#else
(id,name,value)
        int             id;
        NhlString       name;
        unsigned int    value;
#endif
{
        return NhlRLSet(id,name,NhlTUint,value);
}

/*
 * Function:	NhlRLSetLong
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetLong
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	long		value;
#endif
{
	return NhlRLSet(id,name,NhlTLong,value);
}

/*
 * Function:    NhlRLSetUlong
 * 
 * Description: This function is used to set a resource with the value
 *              specified as the given type. 
 *
 * In Args:
 * 
 * Out Args:
 * 
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect: 
 */
NhlErrorTypes
NhlRLSetUlong
#if     NhlNeedProto
(
        int             id,
        NhlString       name,
        unsigned long   value
)
#else
(id,name,value)
        int             id;
        NhlString       name;
        unsigned long   value;
#endif
{
        return NhlRLSet(id,name,NhlTUlong,value);
}

/*
 * Function:    NhlRLSetInt64
 *
 * Description: This function is used to set a resource with the value
 *              specified as the given type.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect:
 */
NhlErrorTypes
NhlRLSetInt64
#if     NhlNeedProto
(
        int             id,
        NhlString       name,
        long long       value
)
#else
(id,name,value)
        int             id;
        NhlString       name;
        long long       value;
#endif
{
        return NhlRLSet(id,name,NhlTInt64,value);
}

/*
 * Function:    NhlRLSetUint64
 *
 * Description: This function is used to set a resource with the value
 *              specified as the given type.
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:       global
 * Returns:     NhlErrorTypes
 * Side Effect:
 */
NhlErrorTypes
NhlRLSetUint64
#if     NhlNeedProto
(
        int                id,
        NhlString          name,
        unsigned long long value
)
#else
(id,name,value)
        int                id;
        NhlString          name;
        unsigned long long value;
#endif
{
        return NhlRLSet(id,name,NhlTUint64,value);
}

/*
 * Function:	NhlRLSetFloat
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetFloat
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	float		value;
#endif
{
	return NhlRLSet(id,name,NhlTFloat,value);
}

/*
 * Function:	NhlRLSetDouble
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetDouble
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	double		value;
#endif
{
	return NhlRLSet(id,name,NhlTDouble,value);
}

/*
 * Function:	NhlRLSetString
 *
 * Description:	This function is used to set a resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetString
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	NhlString	value;
#endif
{
	return NhlRLSet(id,name,NhlTString,value);
}

/*
 * Function:	NhlRLSetMDArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetMDArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlPointer	data,
	NhlString	type,
	ng_size_t	size,
	int		num_dimensions,
	ng_size_t	*len_dimensions
)
#else
(id,name,data,type,size,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	NhlPointer	data;
	NhlString	type;
	ng_size_t	size;
	int		num_dimensions;
	ng_size_t	*len_dimensions;
#endif
{
	_NhlArgVal	gen = {0};
	
	gen.ptrval = _NhlCreateGenArray(data,type,size,num_dimensions,
					len_dimensions,False);
	if(gen.ptrval == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	if(_NhlRLInsert(id,NhlSETRL,NrmStringToQuark(name),genQ,gen,
			sizeof(NhlGenArray),(_NhlFreeFunc)NhlFreeGenArray))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLSetMDIntegerArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetMDIntegerArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		*data,
	int		num_dimensions,
	ng_size_t	*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	int		*data;
	int		num_dimensions;
	ng_size_t	*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTInteger,sizeof(int),
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLSetMDLongArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetMDLongArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		*data,
	int		num_dimensions,
	ng_size_t	*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	long		*data;
	int		num_dimensions;
	ng_size_t	*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTLong,sizeof(long),
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLSetMDFloatArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetMDFloatArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		*data,
	int		num_dimensions,
	ng_size_t	*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	float		*data;
	int		num_dimensions;
	ng_size_t	*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTFloat,sizeof(float),
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLSetMDDoubleArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetMDDoubleArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		*data,
	int		num_dimensions,
	ng_size_t	*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	double		*data;
	int		num_dimensions;
	ng_size_t	*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTDouble,sizeof(double),
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLSetArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlPointer	data,
	NhlString	type,
	ng_size_t	size,
	ng_size_t	num_elements
)
#else
(id,name,data,type,size,num_elements)
	int		id;
	NhlString	name;
	NhlPointer	data;
	NhlString	type;
	ng_size_t	size;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,type,size,1,&num_elements);
}

/*
 * Function:	NhlRLSetIntegerArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetIntegerArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		*data,
	ng_size_t	num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	int		*data;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTInteger,sizeof(int),1,
								&num_elements);
}

/*
 * Function:	NhlRLSetLongArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetLongArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		*data,
	ng_size_t		num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	long		*data;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTLong,sizeof(long),1,
								&num_elements);
}

/*
 * Function:	NhlRLSetFloatArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetFloatArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		*data,
	ng_size_t	num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	float		*data;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTFloat,sizeof(float),1,
								&num_elements);
}

/*
 * Function:	NhlRLSetDoubleArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetDoubleArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		*data,
	ng_size_t	num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	double		*data;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTDouble,sizeof(double),1,
								&num_elements);
}

/*
 * Function:	NhlRLSetStringArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLSetStringArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	*data,
	ng_size_t	num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	NhlString	*data;
	ng_size_t	num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTString,sizeof(NhlString),1,
								&num_elements);
}

/*
 * Function:	NhlRLGet
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified. The "..." field must be a Pointer to the type
 *		specified or there will probably be a seg-fault.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGet
#if	NhlNeedVarArgProto
(
	int		id,
	NhlString	name,
	NhlString	type,
	...
)
#else
(id,name,type,va_alist)
	int		id;
	NhlString	name;
	NhlString	type;
	va_dcl
#endif
{
	va_list		ap;
	_NhlArgVal	value;
	unsigned int	size = 0;
	NrmQuark	typeQ = NrmStringToQuark(type);

	NhlINITVAR(value);

	/*
	 * default type is "long"
	 */
	if(typeQ == byteQ){
		size = sizeof(char);
	}
	else if(typeQ == charQ){
		size = sizeof(char);
	}
	else if(typeQ == shortQ){
		size = sizeof(short);
	}
	else if(_NhlIsSubtypeQ(intQ,typeQ)){	/* gets all enumerated types */
		size = sizeof(int);
	}
	else if(typeQ == floatQ){
		size = sizeof(float);
	}
	else if(typeQ == stringQ){
		size = sizeof(NhlString);
	}
	else if(typeQ == doubleQ){
		size = sizeof(double);
	}
	else if(_NhlIsSubtypeQ(genQ,typeQ)){	/* gets all GenArray types */
		size = sizeof(NhlGenArray);
	}
	else{
		size = sizeof(long);
	}

	VA_START(ap,type);
	value.ptrval = va_arg(ap,NhlPointer);
	va_end(ap);

	if(_NhlRLInsert(id,NhlGETRL,NrmStringToQuark(name),
					NrmStringToQuark(type),value,size,NULL))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLGetInteger
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetInteger
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		*value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	int		*value;
#endif
{
	return NhlRLGet(id,name,NhlTInteger,value);
}

/*
 * Function:	NhlRLGetLong
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetLong
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		*value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	long		*value;
#endif
{
	return NhlRLGet(id,name,NhlTLong,value);
}

/*
 * Function:	NhlRLGetFloat
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetFloat
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		*value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	float		*value;
#endif
{
	return NhlRLGet(id,name,NhlTFloat,value);
}

/*
 * Function:	NhlRLGetDouble
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetDouble
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		*value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	double		*value;
#endif
{
	return NhlRLGet(id,name,NhlTDouble,value);
}

/*
 * Function:	NhlRLGetString
 *
 * Description:	This function is used to retrieve a resource into the address
 *		specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetString
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	*value
)
#else
(id,name,value)
	int		id;
	NhlString	name;
	NhlString	*value;
#endif
{
	return NhlRLGet(id,name,NhlTString,value);
}

/*
 * Function:	CvtGenToExpMDArray
 *
 * Description:	This function is used to convert a "gen" array to an "exp"
 *		array.  This is how the addresses the user passed in actually
 *		get set with the data in the "gen" array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtGenToExpMDArray
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[] = "CvtGenToExpMDArray";
	NhlGenArray	gen;
	_NhlExpArray	exp;
	NhlString	type;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called w/improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = (NhlGenArray)from->data.ptrval;
	exp = (_NhlExpArray)to->data.ptrval;

	if(gen == NULL){
		*exp->data = NULL;
		*exp->len_dimensions = NULL;
		*exp->num_dimensions = 0;
		if (exp->size != NULL) *exp->size  = 0;
		*exp->type = NULL;

		return NhlNOERROR;
	}

	if(gen->len_dimensions == &gen->num_elements){
		*exp->len_dimensions = NhlMalloc(sizeof(int));
		if(*exp->len_dimensions == NULL){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			to->size = 0;
			return NhlFATAL;
		}
		**exp->len_dimensions = gen->num_elements;
	}
	else{
		*exp->len_dimensions = gen->len_dimensions;
		/* give ownership to exp */
		gen->len_dimensions = &gen->num_elements;
	}
	*exp->num_dimensions = gen->num_dimensions;
	type = NrmQuarkToString(gen->typeQ);
	*exp->type = NhlMalloc(strlen(type) + 1);
	strcpy(*exp->type,type);
	*exp->size = gen->size;
	if(!gen->my_data){
		/*
		 * Probably came from Scalar To GenArray
		 * converter and we should get some memory.
		 */
		NhlGenArray	tgen;

		tgen = _NhlCopyGenArray(gen,True);
		if(tgen == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		*exp->data = tgen->data;
		tgen->my_data = False;
		NhlFreeGenArray(tgen);
	}
	else{
		/* give ownership to exp */
		gen->my_data = False;
		*exp->data = gen->data;
	}

	return ret;
}

/*
 * Function:	NhlRLGetMDArray
 *
 * Description:	This function is used to retrieve an array resource into the
 *		address specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetMDArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlPointer	*data,
	NhlString	*type,
	unsigned int	*size,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,data,type,size,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	NhlPointer	*data;
	NhlString	*type;
	unsigned int	*size;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	_NhlExpArray	exp = NULL;
	_NhlArgVal	expval;
	
	exp = NhlMalloc(sizeof(_NhlExpArrayRec));
	if(exp == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	exp->num_dimensions = num_dimensions;
	exp->len_dimensions = len_dimensions;
	exp->type = type;
	exp->size = size;
	exp->data = data;

	expval.ptrval = exp;

	if(_NhlRLInsert(id,NhlGETRL,NrmStringToQuark(name),expMDQ,expval,
				sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	CvtGenToExpTypeMDArray
 *
 * Description:	This function is used to convert a "gen" array to an "exp"
 *		array.  This is how the addresses the user passed in actually
 *		get set with the data in the "gen" array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtGenToExpTypeMDArray
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[] = "CvtGenToExpTypeMDArray";
	NhlGenArray	gen,convgen;
	_NhlExpArray	exp;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = (NhlGenArray)from->data.ptrval;
	exp = (_NhlExpArray)to->data.ptrval;

	if(gen == NULL){
		*exp->data = NULL;
		*exp->len_dimensions = NULL;
		*exp->num_dimensions = 0;
		if (exp->size != NULL) *exp->size  = 0;

		return NhlNOERROR;
	}

	/* get data if possible */
	if(exp->type_req == gen->typeQ){
		convgen = gen;
	}
	else{
		NrmValue	fromval, toval;
		char		fromtype[_NhlMAXRESNAMLEN];
		char		totype[_NhlMAXRESNAMLEN];

		strcpy(fromtype,NrmQuarkToString(gen->typeQ));
		strcat(fromtype,NhlTGenArray);

		strcpy(totype,NrmQuarkToString(exp->type_req));
		strcat(totype,NhlTGenArray);

		fromval.size = sizeof(NhlGenArray);
		fromval.data.ptrval = gen;

		convgen = NULL;
		toval.size = sizeof(NhlGenArray);
		toval.data.ptrval = &convgen;

		ret = NhlReConvertData(fromtype,totype,&fromval,&toval);
		
		if((ret < NhlWARNING) || (convgen == NULL)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert %s to %s",func,fromtype,
									totype);
			return NhlFATAL;
		}
	}

	if(!convgen->my_data){
		/*
		 * Probably came from Scalar To GenArray
		 * converter and we should get some memory.
		 */
		NhlGenArray	tgen;

		tgen = _NhlCopyGenArray(convgen,True);
		if(tgen == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		*exp->data = tgen->data;
		tgen->my_data = False;
		NhlFreeGenArray(tgen);
	}
	else{
		/* give ownership to exp */
		*exp->data = convgen->data;
		convgen->my_data = False;
	}

	if(gen->len_dimensions == &gen->num_elements){
		*exp->len_dimensions = NhlMalloc(sizeof(int));
		if(*exp->len_dimensions == NULL){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			to->size = 0;
			return NhlFATAL;
		}
		**exp->len_dimensions = gen->num_elements;
	}
	else{
		*exp->len_dimensions = gen->len_dimensions;
		/* give ownership to exp */
		gen->len_dimensions = &gen->num_elements;
	}
	*exp->num_dimensions = gen->num_dimensions;

	return ret;
}

/*
 * Function:	NhlRLGetMDTypeArray
 *
 * Description:	This function is used to retrieve the resource given as an
 *		array of the type specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NhlRLGetMDTypeArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NrmQuark	type,
	unsigned int	size,
	NhlPointer	*data,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,type,size,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	NrmQuark	type;
	unsigned int	size;
	NhlPointer	*data;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	_NhlExpArray	exp = NULL;
	_NhlArgVal	expval;
	
	exp = NhlMalloc(sizeof(_NhlExpArrayRec));
	if(exp == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	exp->num_dimensions = num_dimensions;
	exp->len_dimensions = len_dimensions;
	exp->type = NULL;
	exp->size = NULL;
	exp->data = data;

	exp->type_req = type;
	exp->size_req = size;

	expval.ptrval = exp;

	if(_NhlRLInsert(id,NhlGETRL,NrmStringToQuark(name),expMDTypeQ,expval,
				sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLGetMDIntegerArray
 *
 * Description:	This function is used to retrieve the resource given as an
 *		array of the type specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetMDIntegerArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		**data,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	int		**data;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	return NhlRLGetMDTypeArray(id,name,intQ,sizeof(int),(NhlPointer*)data,
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLGetMDLongArray
 *
 * Description:	This function is used to retrieve the resource given as an
 *		array of the type specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetMDLongArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		**data,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	long		**data;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	return NhlRLGetMDTypeArray(id,name,longQ,sizeof(long),(NhlPointer*)data,
						num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLGetMDFloatArray
 *
 * Description:	This function is used to retrieve the resource given as an
 *		array of the type specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetMDFloatArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		**data,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	float		**data;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	return NhlRLGetMDTypeArray(id,name,floatQ,sizeof(float),
			(NhlPointer*)data,num_dimensions,len_dimensions);
}

/*
 * Function:	NhlRLGetMDDoubleArray
 *
 * Description:	This function is used to retrieve the resource given as an
 *		array of the type specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetMDDoubleArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		**data,
	int		*num_dimensions,
	ng_size_t	**len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	double		**data;
	int		*num_dimensions;
	ng_size_t	**len_dimensions;
#endif
{
	return NhlRLGetMDTypeArray(id,name,doubleQ,sizeof(double),
			(NhlPointer*)data,num_dimensions,len_dimensions);
}

/*
 * Function:	CvtGenToExpArray
 *
 * Description:	This function is used to convert a "gen" array to an "exp"
 *		array.  This is how the addresses the user passed in actually
 *		get set with the data in the "gen" array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtGenToExpArray
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[] = "CvtGenToExpArray";
	NhlErrorTypes	ret = NhlNOERROR;
	NhlGenArray	gen;
	_NhlExpArray	exp;
	int		i;
	NhlString	type;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Called w/improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = (NhlGenArray)from->data.ptrval;
	exp = (_NhlExpArray)to->data.ptrval;

	if(gen == NULL){
		for(i=0;i < exp->num_dim_req;i++)
			(*exp->len_dimensions)[i] = 0;
		*exp->type = NULL;
		if (exp->size != NULL) *exp->size  = 0;
		*exp->data = NULL;

		return NhlNOERROR;
	}

	if(gen->num_dimensions == exp->num_dim_req){
		for(i=0;i < exp->num_dim_req;i++)
			(*exp->len_dimensions)[i] = gen->len_dimensions[i];
	}
	else{
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Array resource has %d dimensions:Can't convert to %d dimensions",
			func,gen->num_dimensions,exp->num_dim_req);
		to->size = 0;
		return NhlFATAL;
	}
	type = NrmQuarkToString(gen->typeQ);
	*exp->type = NhlMalloc(strlen(type) + 1);
	strcpy(*exp->type,type);
	*exp->size = gen->size;
	if(!gen->my_data){
		NhlGenArray	tgen;
		/*
		 * Came from another converter so we should get some memory.
		 */
		tgen = _NhlCopyGenArray(gen,True);
		if(tgen == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		*exp->data = tgen->data;
		tgen->my_data = False;
		NhlFreeGenArray(tgen);
	}
	else{
		/* give ownership to exp */
		*exp->data = gen->data;
		gen->my_data = False;
	}

	return ret;
}

/*
 * Function:	NhlRLGetDimArray
 *
 * Description:	This function is used to retrieve a (N)D array resource into the
 *		address specified.
 *		This may turn into a public function at some point if it is
 *		useful.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NhlRLGetDimArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlPointer	*data,
	NhlString	*type,
	unsigned int	*size,
	int		num_dim,		/* IN */
	ng_size_t	**len_dimensions
)
#else
(id,name,data,type,size,num_dim,len_dimensions)
	int		id;
	NhlString	name;
	NhlPointer	*data;
	NhlString	*type;
	unsigned int	*size;
	int		num_dim;		/* IN */
	ng_size_t	**len_dimensions;
#endif
{
	_NhlExpArray	exp = NULL;
	_NhlArgVal	expval;
	
	exp = NhlMalloc(sizeof(_NhlExpArrayRec));
	if(exp == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	exp->num_dim_req = num_dim;
	if(num_dim == 1){
		/*
		 * for 1D case, the len_dimensions is actually a pointer to
		 * the (int*) the user passed in as "num_elements".
		 */
		exp->num_elements = *len_dimensions;
		exp->len_dimensions = &exp->num_elements;
	}
	else
		exp->len_dimensions = len_dimensions;

	exp->type = type;
	exp->size = size;
	exp->data = data;

	expval.ptrval = exp;

	if(_NhlRLInsert(id,NhlGETRL,NrmStringToQuark(name),expQ,expval,
				sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLGetArray
 *
 * Description:	This function is used to retrieve a 1D array resource into the
 *		address specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlPointer	*data,
	NhlString	*type,
	unsigned int	*size,
	ng_size_t	*num_elements
)
#else
(id,name,data,type,size,num_elements)
	int		id;
	NhlString	name;
	NhlPointer	*data;
	NhlString	*type;
	unsigned int	*size;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetDimArray(id,name,data,type,size,1,&num_elements);
}

/*
 * Function:	CvtGenToExpTypeArray
 *
 * Description:	This function is used to convert a "gen" array to an "exp"
 *		array.  This is how the addresses the user passed in actually
 *		get set with the data in the "gen" array.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
CvtGenToExpTypeArray
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[]="CvtGenToExpTypeArray";
	int		i;
	NhlGenArray	gen,convgen;
	_NhlExpArray	exp;
	NhlErrorTypes	ret = NhlNOERROR;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called w/improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	gen = (NhlGenArray)from->data.ptrval;
	exp = (_NhlExpArray)to->data.ptrval;

	if(gen == NULL){
		for(i=0;i < exp->num_dim_req; i++)
			(*exp->len_dimensions)[i] = 0;
		*exp->data = NULL;

		return NhlNOERROR;
	}

	if(gen->num_dimensions == exp->num_dim_req){
		for(i=0;i < exp->num_dim_req;i++)
			(*exp->len_dimensions)[i] = gen->len_dimensions[i];
	}
	else{
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Array resource has %d dimensions:Can't convert to %d dimensions",
				func,gen->num_dimensions,exp->num_dim_req);
		to->size = 0;
		return NhlFATAL;
	}

	/* get data if possible */
	if(exp->type_req == gen->typeQ){
		convgen = gen;
	}
	else{
		NrmValue	fromval, toval;
		char		fromtype[_NhlMAXRESNAMLEN];
		char		totype[_NhlMAXRESNAMLEN];

		strcpy(fromtype,NrmQuarkToString(gen->typeQ));
		strcat(fromtype,NhlTGenArray);

		strcpy(totype,NrmQuarkToString(exp->type_req));
		strcat(totype,NhlTGenArray);

		fromval.size = sizeof(NhlGenArray);
		fromval.data.ptrval = gen;

		convgen = NULL;
		toval.size = sizeof(NhlGenArray);
		toval.data.ptrval = &convgen;

		ret = NhlReConvertData(fromtype,totype,&fromval,&toval);

		if((ret < NhlWARNING) || (convgen == NULL)){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert %s to %s",func,fromtype,
									totype);
			return NhlFATAL;
		}
	}

	if(!convgen->my_data){
		NhlGenArray	tgen;

		tgen = _NhlCopyGenArray(convgen,True);
		if(tgen == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}
		*exp->data = tgen->data;
		tgen->my_data = False;
		NhlFreeGenArray(tgen);
	}
	else{
		*exp->data = convgen->data;
		convgen->my_data = False;
	}

	return ret;
}

/*
 * Function:	NhlRLGetTypeDimArray
 *
 * Description:	This function is used to retrieve a (N)D array resource into the
 *		address specified.
 *		This may turn into a public function at some point if it is
 *		useful.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NhlRLGetTypeDimArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	type,
	unsigned int	size,
	NhlPointer	*data,
	ng_size_t	num_dim,		/* IN */
	ng_size_t	**len_dimensions
)
#else
(id,name,type,size,data,num_dim,len_dimensions)
	int		id;
	NhlString	name;
	NhlString	type;
	unsigned int	size;
	NhlPointer	*data;
	ng_size_t	num_dim;		/* IN */
	ng_size_t	**len_dimensions;
#endif
{
	_NhlExpArray	exp = NULL;
	_NhlArgVal	expval;

	exp = NhlMalloc(sizeof(_NhlExpArrayRec));
	if(exp == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	exp->num_dim_req = num_dim;
	if(num_dim == 1){
		/*
		 * for 1D case, the len_dimensions is actually a pointer to
		 * the (int*) the user passed in as "num_elements".
		 */
		exp->num_elements = *len_dimensions;
		exp->len_dimensions = &exp->num_elements;
	}
	else
		exp->len_dimensions = len_dimensions;

	exp->type = NULL;
	exp->size = NULL;
	exp->data = data;

	exp->type_req = NrmStringToQuark(type);
	exp->size_req = size;

	expval.ptrval = exp;

	if(_NhlRLInsert(id,NhlGETRL,NrmStringToQuark(name),expTypeQ,expval,
				sizeof(NhlPointer),(_NhlFreeFunc)NhlFree))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLGetTypeArray
 *
 * Description:	This function is used to retrieve a 1D array resource into the
 *		address specified.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
NhlRLGetTypeArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	type,
	unsigned int	size,
	NhlPointer	*data,
	ng_size_t	*num_elements
)
#else
(id,name,type,size,data,num_elements)
	int		id;
	NhlString	name;
	NhlString	type;
	unsigned int	size;
	NhlPointer	*data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeDimArray(id,name,type,size,data,1,&num_elements);
}

/*
 * Function:	NhlRLGetIntegerArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetIntegerArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		**data,
	ng_size_t	*num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	int		**data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeArray(id,name,NhlTInteger,sizeof(int),
						(NhlPointer*)data,num_elements);
}

/*
 * Function:	NhlRLGetLongArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetLongArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	long		**data,
	ng_size_t	*num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	long		**data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeArray(id,name,NhlTLong,sizeof(long),
						(NhlPointer*)data,num_elements);
}

/*
 * Function:	NhlRLGetFloatArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetFloatArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	float		**data,
	ng_size_t	*num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	float		**data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeArray(id,name,NhlTFloat,sizeof(float),
						(NhlPointer*)data,num_elements);
}

/*
 * Function:	NhlRLGetDoubleArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetDoubleArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	double		**data,
	ng_size_t	*num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	double		**data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeArray(id,name,NhlTDouble,sizeof(double),
						(NhlPointer*)data,num_elements);
}

/*
 * Function:	NhlRLGetStringArray
 *
 * Description:	This function is used to set an array resource with the value
 *		specified as the given type.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlRLGetStringArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	NhlString	**data,
	ng_size_t	*num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	NhlString	**data;
	ng_size_t	*num_elements;
#endif
{
	return NhlRLGetTypeArray(id,name,NhlTString,sizeof(NhlString),
						(NhlPointer*)data,num_elements);
}

/*
 * Function:	CopyNodeToArgList
 *
 * Description:	append the current node to the ArgList
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
CopyNodeToArgList
#if	NhlNeedProto
(
	_NhlRLNode	node,
	_NhlArgList	args,
	int		*nargs
)
#else
(node,args,nargs)
	_NhlRLNode	node;
	_NhlArgList	args;
	int		*nargs;
#endif
{
	if(node == NULL)
		return;

	CopyNodeToArgList(node->left,args,nargs);

	args[*nargs].quark = node->nameQ;
	args[*nargs].type = node->typeQ;
	args[*nargs].value = node->value;
	args[*nargs].size = node->size;
	(*nargs)++;

	CopyNodeToArgList(node->right,args,nargs);

	return;
}

/*
 * Function:	_NhlRLToArgList
 *
 * Description:	Converts an RL list to an _NhlArg list for the method
 *		functions.
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
_NhlRLToArgList
#if	NhlNeedProto
(
	int		id,
	NhlRLType	action,
	_NhlArgList	args,
	int		*nargs
)
#else
(id,action,args,nargs)
	int		id;
	NhlRLType	action;
	_NhlArgList	args;
	int		*nargs;
#endif
{
	_NhlRLHead	*head;

	*nargs = 0;

	if(id == 0)
		return True;

	head = GetHead(id);
	if(head == NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"_NhlRLToArgList:Unable to find RL list with id=%d",id);
		return False;
	}

	if((*head)->list_type != action){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"_NhlRLToArgList:RL list incompatible with action");
		return False;
	}

	CopyNodeToArgList((*head)->list,args,nargs);

	return True;
}

/*
 * Function:	_NhlDestroyRLList
 *
 * Description:	This function is used to free the ListTable.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
_NhlDestroyRLList
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	int i;

	for(i=0;i < table_len && num_lists > 0;i++)
		if(ListTable[i] != NULL)
			NhlRLDestroy(i+1);

	if(num_lists > 0)
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Not all RL lists destroyed?");

	table_len = 0;
	num_lists = 0;
	(void)NhlFree(ListTable);
	ListTable = NULL;

	return;
}

/*
 * Function:	InitRLList
 *
 * Description:	This function is used to initialize the RL interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	void
 * Side Effect:	
 */
static void
InitRLList
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	byteQ = NrmStringToQuark(NhlTByte);
	charQ = NrmStringToQuark(NhlTCharacter);
	doubleQ = NrmStringToQuark(NhlTDouble);
	floatQ = NrmStringToQuark(NhlTFloat);
	shortQ = NrmStringToQuark(NhlTShort);
	stringQ = NrmStringToQuark(NhlTString);
	intQ = NrmStringToQuark(NhlTInteger);
	longQ = NrmStringToQuark(NhlTLong);
	genQ = NrmStringToQuark(NhlTGenArray);
	pointerQ = NrmStringToQuark(NhlTPointer);
	expMDQ = NrmStringToQuark(_NhlTExpMDArray);
	expMDTypeQ = NrmStringToQuark(_NhlTExpMDTypeArray);
	expQ = NrmStringToQuark(_NhlTExpArray);
	expTypeQ = NrmStringToQuark(_NhlTExpTypeArray);

	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTExpMDArray,
					CvtGenToExpMDArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,_NhlTExpMDArray,
						NhlTScalar,NhlTGenArray);

	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTExpMDTypeArray,
				CvtGenToExpTypeMDArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,_NhlTExpMDTypeArray,
						NhlTScalar,NhlTGenArray);

	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTExpArray,
					CvtGenToExpArray,NULL,0,False,NULL);
	(void)_NhlRegSymConv(NULL,NhlTScalar,_NhlTExpArray,NhlTScalar,
								NhlTGenArray);

	(void)NhlRegisterConverter(NULL,NhlTGenArray,_NhlTExpTypeArray,
					CvtGenToExpTypeArray,NULL,0,False,NULL);
	return;
}
