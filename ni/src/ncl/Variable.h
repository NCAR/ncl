
/*
 *      $Id: Variable.h,v 1.3 1993-10-18 16:11:11 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun 28 10:53:34 MDT 1993
 *
 *	Description:	
 */

#ifndef _NCVariable_h
#define _NCVariable_h



typedef struct _NclVarInfo {
	char	varname[NCL_MAX_STRING];
	int level;
	int datatype;
	unsigned int offset;
}NclVarInfo; 


typedef struct _NclVarRec {
	int kind;
	char *name;	/* not variables name but what type of object */ 
	void	*val;
	int n_dims;
	unsigned int	dim_sizes[NCL_MAX_DIMENSIONS];
	int	datatype;
	struct _NclVarDimRec *diminfo;
	struct _NclVarCoordRec *coordinfo;
	struct _NclVarAttRec *attinfo;
	struct _NclSymbol  *sym_ent;
}NclVarRec;

typedef struct _NclVarDimRec{
	int kind;
	int	ndims;
	char	*dim_names[NCL_MAX_DIMENSIONS];
	int     dim_nums[NCL_MAX_DIMENSIONS];
}NclDimRec;

typedef struct _NclVarAttRec{
	int 	kind;
	int	natts;
	char	*att_names[NCL_MAX_ATTRIBUTES];
	unsigned int 	att_sizes[NCL_MAX_ATTRIBUTES];
	void	*att_values[NCL_MAX_ATTRIBUTES];
}NclAttRec;
	
typedef struct _NclVarCoordStruct {
	int kind;
	struct _NclSymbol *coord_vars[NCL_MAX_DIMENSIONS];
}NclCoordStruct;
#endif  /*_NCVariable_h*/
