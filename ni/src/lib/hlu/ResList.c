/*
 *      $Id: ResList.c,v 1.1 1994-02-08 20:15:37 boote Exp $
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
#include <ncarg/hlu/VarArg.h>

static _NhlRLHead *ListTable = NULL;
static int num_lists = 0;
static int table_len = 0;
static NrmQuark	floatQ;
static NrmQuark	genQ;

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
#if	__STDC__
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
#if	__STDC__
(
	int		nameQ,
	int		typeQ,
	_NhlArgVal	value,
	_NhlFreeFunc	free_func
)
#else
(nameQ,typeQ,value,free_func)
	int		nameQ;
	int		typeQ;
	_NhlArgVal	value;
	_NhlFreeFunc	free_func;
#endif
{
	_NhlRLNode	node = NhlMalloc(sizeof(_NhlRLNodeRec));

	if(node == NULL)
		return NULL;

	node->nameQ = nameQ;
	node->typeQ = typeQ;
	node->value = value;
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
#if	__STDC__
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
#if	__STDC__
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
		(*(node->free_func))((NhlPointer)node->value);

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
#if	__STDC__
(
	int		id,
	NhlRLType	type_action,
	int		nameQ,
	int		typeQ,
	_NhlArgVal	value,
	_NhlFreeFunc	free_func
)
#else
(id,type_action,nameQ,typeQ,value,free_func)
	int		id;
	NhlRLType	type_action;
	int		nameQ;
	int		typeQ;
	_NhlArgVal	value;
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
		(*node)->free_func = free_func;
	}
	else{
		if((*head)->num >= _NhlMAXARGLIST){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
						"RL cannot get any larger");
			return False;
		}
		*node = CreateNode(nameQ,typeQ,value,free_func);
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
#if	__STDC__
(
	NhlRLType	list_type
)
#else
(list_type)
	NhlRLType	list_type;
#endif
{
	register int	i;
	_NhlRLHead	new = NULL;

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
#if	__STDC__
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
#if	__STDC__
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
	NhlFree(*head);
	*head = NULL;
	num_lists--;

	return;
}

/*
 * Function:	_NhlInitRLList
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
void
_NhlInitRLList
#if	__STDC__
(
	void
)
#else
()
#endif
{
	floatQ = NrmStringToQuark(NhlTFloat);
	genQ = NrmStringToQuark(NhlTGenArray);

	return;
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
#if	__STDC__
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
#if	__STDC__
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
#if	__STDC__
(
	int	id,
	char	*name
)
#else
(id,name)
	int	id;
	char	*name;
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
		node2 = GetNodePtr(&(*node)->left,(*node)->right->nameQ);
		*node2 = (*node)->right;
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
#if	__STDC__
(
	int	id,
	char	*name
)
#else
(id,name)
	int	id;
	char	*name;
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
#if	NeedVarArgProto
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
	double		tmp;
	_NhlArgVal	value;
	NrmQuark	typeQ = NrmStringToQuark(type);

	VA_START(ap,type);
	if(typeQ == floatQ){
		tmp = va_arg(ap,double);
		*(float*)&value = (float)tmp;
	}
	else
		value = va_arg(ap,_NhlArgVal);

	va_end(ap);

	if(_NhlRLInsert(id,NhlSETRL,NrmStringToQuark(name),typeQ,value,NULL))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLSetInt
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
NhlRLSetInt
#if	NeedVarArgProto
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
#if	NeedVarArgProto
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
#if	NeedVarArgProto
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
	unsigned int	size,
	int		num_dimensions,
	int		*len_dimensions
)
#else
(id,name,data,type,size,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	NhlPointer	data;
	NhlString	type;
	unsigned int	size;
	int		num_dimensions;
	int		*len_dimensions;
#endif
{
	NhlGenArray	gen = NULL;

	gen = _NhlCreateGenArray(data,type,size,num_dimensions,len_dimensions,
									False);
	if(gen == NULL){
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NhlFATAL;
	}

	if(_NhlRLInsert(id,NhlSETRL,NrmStringToQuark(name),genQ,(_NhlArgVal)gen,
						(_NhlFreeFunc)NhlFreeGenArray))
		return NhlNOERROR;
	else
		return NhlFATAL;
}

/*
 * Function:	NhlRLSetMDIntArray
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
NhlRLSetMDIntArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		*data,
	int		num_dimensions,
	int		*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	int		*data;
	int		num_dimensions;
	int		*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTInteger,sizeof(int),
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
	int		*len_dimensions
)
#else
(id,name,data,num_dimensions,len_dimensions)
	int		id;
	NhlString	name;
	float		*data;
	int		num_dimensions;
	int		*len_dimensions;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTFloat,sizeof(float),
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
	unsigned int	size,
	int		num_elements
)
#else
(id,name,data,type,size,num_elements)
	int		id;
	NhlString	name;
	NhlPointer	data;
	NhlString	type;
	unsigned int	size;
	int		num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,type,size,1,&num_elements);
}

/*
 * Function:	NhlRLSetIntArray
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
NhlRLSetIntArray
#if	NhlNeedProto
(
	int		id,
	NhlString	name,
	int		*data,
	int		num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	int		*data;
	int		num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTInteger,sizeof(int),1,
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
	int		num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	float		*data;
	int		num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTFloat,sizeof(float),1,
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
	int		num_elements
)
#else
(id,name,data,num_elements)
	int		id;
	NhlString	name;
	NhlString	*data;
	int		num_elements;
#endif
{
	return NhlRLSetMDArray(id,name,data,NhlTString,sizeof(NhlString),1,
								&num_elements);
}

/*
 * Function:	CopyNodeToArgList
 *
 * Description:	append the current node to the ExtArgList
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
#if	__STDC__
(
	_NhlRLNode	node,
	_NhlExtArgList	args,
	int		*nargs
)
#else
(node,args,nargs)
	_NhlRLNode	node;
	_NhlExtArgList	args;
	int		*nargs;
#endif
{
	if(node == NULL)
		return;

	CopyNodeToArgList(node->left,args,nargs);

	args[*nargs].quark = node->nameQ;
	args[*nargs].value = node->value;
	args[*nargs].type = node->typeQ;
	(*nargs)++;

	CopyNodeToArgList(node->right,args,nargs);

	return;
}

/*
 * Function:	_NhlRLToArgList
 *
 * Description:	Converts an RL list to an _NhlExtArg list for the method
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
#if	__STDC__
(
	int		id,
	NhlRLType	action,
	_NhlExtArgList	args,
	int		*nargs
)
#else
(id,action,args,nargs)
	int		id;
	NhlRLType	action;
	_NhlExtArgList	args;
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
