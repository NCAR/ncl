
#ifndef NclDataDefs_h
#define NclDataDefs_h

#ifdef DEBUG
#include <assert.h>
/*
#define ASSERT(expr) (assert(expr))
*/
#define ASSERT(expr)
#else
#define ASSERT(expr)
#endif

typedef enum  {
NOTDEFINED,
TEMPORARY,
PERMANENT,
STATIC
} NclStatus;


typedef enum  {
Ncl_None =                      00,
Ncl_Obj = 			01,
Ncl_Data = 			02, 
Ncl_MultiDValData = 		04,
Ncl_Typedouble =		010,
Ncl_Typefloat = 	  	020,
Ncl_Typelong = 	  		040,
Ncl_Typeint = 		 	0100,
Ncl_Typeshort = 	 	0200,
Ncl_Typebyte = 	 		0400,
Ncl_Typestring = 		01000,
Ncl_Typechar= 			02000,
Ncl_Typeobj= 			04000,
Ncl_Var = 			010000,
Ncl_Att = 			020000,
Ncl_Typelogical =    		040000,
Ncl_HLUObj = 			0100000,
Ncl_File = 			0200000,
Ncl_FileVar = 			0400000,
Ncl_HLUVar = 			01000000,
Ncl_CoordVar = 			02000000,
Ncl_Type = 			04000000,
Ncl_MultiDValnclfileData =	010000000,
Ncl_MultiDValHLUObjData = 	020000000,
Ncl_OneDValCoordData = 		040000000
} NclObjTypes;

/*
typdef enum {
Ncl_MultiDValdoubleData = 	  010,
Ncl_MultiDValfloatData = 	  020,
Ncl_MultiDVallongData = 	  040,
Ncl_MultiDValintData = 		 0100,
Ncl_MultiDValshortData = 	 0200,
Ncl_MultiDValbyteData = 	 0400,
Ncl_MultiDValstringData = 	01000,
Ncl_MultiDValcharData = 	02000,
Ncl_MultiDVallogicalData = 	0100000,
Ncl_MultiDValnclfileData =     020000,
Ncl_MultiDValHLUObjData =      040000
} NclMultiDValDataTypes;
*/

/*
* allows for selection of basic data value type
*/
#define NCL_VAL_TYPE_MASK ((unsigned int)(Ncl_Typedouble | Ncl_Typefloat | Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort | Ncl_Typebyte | Ncl_Typestring | Ncl_Typechar| Ncl_Typelogical))

/*
* allows for selection of basic variable data value type 
*/
#define NCL_VAR_TYPE_MASK ((unsigned int) Ncl_Var | Ncl_FileVar | Ncl_CoordVar) 

#define NCL_COORD_MASK ((unsigned int) Ncl_OneDValCoordData)

/*
* allows for selection of numeric value types which represent one 
* group of data types that can be coerced
*/
#define NCL_TYPE_NUMERIC_MASK ((unsigned int)(Ncl_Typeint | Ncl_Typedouble | Ncl_Typebyte | Ncl_Typelong | Ncl_Typeshort | Ncl_Typefloat))

#define NCL_VAL_NUMERIC_MASK ((unsigned int)(Ncl_Typedouble | Ncl_Typefloat | Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort | Ncl_Typebyte))

#define NCL_VAL_CHARSTR_MASK ((unsigned int)(Ncl_Typestring | Ncl_Typechar))
#define NCL_HLU_MASK ((unsigned int)Ncl_MultiDValHLUObjData)

#define NCL_MD_MASK (NCL_HLU_MASK | Ncl_MultiDValData | Ncl_MultiDValnclfileData | NCL_COORD_MASK)

typedef enum  {
NCL_none = 	0,
NCL_short = 	01,
NCL_int = 	02,
NCL_long = 	04,
NCL_float =	010,
NCL_double =	020,
NCL_char = 	040,
NCL_byte = 	0100,
NCL_string = 	0200,
NCL_numeric = 	0400,
NCL_logical = 	01000,
NCL_obj = 	02000
} NclBasicDataTypes;

typedef NclQuark string; /* Makes this a quark type */
typedef char byte;
typedef int logical;
typedef int obj;

typedef union _NclScalar {
	double 	doubleval;
	float 	floatval;
	int	intval;
	long    longval;
	short	shortval;
	char	charval;
	string	stringval;
	byte    byteval;
	logical	logicalval;
	obj 	objval;
}NclScalar;

typedef struct _NclRefList{
        struct _NclObjRec *pptr;
        struct _NclRefList *next;
}NclRefList;


typedef struct _NclObjRec *NclObj;
typedef struct _NclObjClassRec *NclObjClass;
typedef struct _NclMultiDValDataRec *NclMultiDValData;
typedef struct _NclDataRec *NclData;

typedef struct _NclObjList {
	int	id;
	NclObjTypes obj_type;
	unsigned int obj_type_mask;
	NclObj	theobj;
	struct _NclObjList *next;
} NclObjList;

typedef enum {  Ncl_SUBSCR, Ncl_VECSUBSCR, Ncl_SUB_ALL, Ncl_SUB_VAL_DEF, Ncl_SUB_DEF_VAL } NclSelectionTypes;

typedef enum { COORD_VECT, COORD_RANGE, INT_VECT, INT_RANGE } NclSubTypes;

typedef struct _NclVectorSelection{
        int n_ind;
        long *ind;
        long min;
        long max;
}NclVectorSelection;

typedef struct _NclSubscriptSelection{
        long start;
        long finish;
        long stride;
}NclSubscriptSelection;


typedef struct _NclSelection{
        NclSelectionTypes sel_type;
        long dim_num;
        union {
                struct _NclSubscriptSelection sub;
                struct _NclVectorSelection  vec;
        }u;
} NclSelection;

typedef struct _NclSelectionRecord {
        struct _NclSymbol *selected_from_sym;
        struct _NclVarRec *selected_from_var;
        int n_entries;
        NclSelection selection[NCL_MAX_DIMENSIONS];
} NclSelectionRecord;


#endif /* NclDataDefs_h */
