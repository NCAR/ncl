
/*
 *      $Id: Variable.h,v 1.1 1993-09-24 23:41:14 ethan Exp $
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
	struct _NclSymbol  *sym_ent;
	int	datatype;
	struct _NclVarDimRec *dims;
	struct _NclVarAttRec *atts;
	struct _NclVarCoordRec *coords;
	void	*val;
}NclVarRec;

typedef struct _NclVarDimRec{
	int kind;
	int	ndims;
	unsigned int	dim_sizes[NCL_MAX_DIMENSIONS];
	char	*dim_names[NCL_MAX_DIMENSIONS];
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
