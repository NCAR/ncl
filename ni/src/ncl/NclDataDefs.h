
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

Ncl_MultiDValdoubleData = 	  010,
Ncl_MultiDValfloatData = 	  020,
Ncl_MultiDVallongData = 	  040,
Ncl_MultiDValintData = 		 0100,
Ncl_MultiDValshortData = 	 0200,
Ncl_MultiDValbyteData = 	 0400,
Ncl_MultiDValstringData = 	01000,
Ncl_MultiDValcharData = 	02000,

Ncl_Var = 	04000,
Ncl_Att = 	010000,
Ncl_MultiDValnclfileData = 	020000,
Ncl_MultiDValHLUObjData =    040000,
Ncl_MultiDVallogicalData =    0100000,
Ncl_HLUObj = 0200000,
Ncl_File = 0400000,
Ncl_FileVar = 01000000,
Ncl_HLUVar = 02000000,
Ncl_CoordVar = 04000000
} NclObjTypes;

/*
* allows for selection of basic data value type
*/
#define NCL_VAL_TYPE_MASK ((unsigned int)(Ncl_MultiDValdoubleData | Ncl_MultiDValfloatData | Ncl_MultiDVallongData | Ncl_MultiDValintData | Ncl_MultiDValshortData | Ncl_MultiDValbyteData | Ncl_MultiDValstringData | Ncl_MultiDValcharData | Ncl_MultiDVallogicalData))

/*
* allows for selection of basic variable data value type 
*/
#define NCL_VAR_TYPE_MASK ((unsigned int) Ncl_Var | Ncl_FileVar | Ncl_CoordVar) 

/*
* allows for selection of numeric value types which represent one 
* group of data types that can be coerced
*/

#define NCL_VAL_NUMERIC_MASK ((unsigned int)(Ncl_MultiDValdoubleData | Ncl_MultiDValfloatData | Ncl_MultiDVallongData | Ncl_MultiDValintData | Ncl_MultiDValshortData | Ncl_MultiDValbyteData))
#define NCL_VAL_CHARSTR_MASK ((unsigned int)(Ncl_MultiDValstringData | Ncl_MultiDValcharData))
#define NCL_HLU_MASK ((unsigned int)Ncl_MultiDValHLUObjData)

#define NCL_MD_MASK (NCL_HLU_MASK | NCL_VAL_TYPE_MASK | Ncl_MultiDValnclfileData)

typedef enum  {
NCL_none,
NCL_short,
NCL_int,
NCL_long,
NCL_float,
NCL_double,
NCL_char,
NCL_byte,
NCL_string,
NCL_numeric,
NCL_logical,
NCL_nclfile,
NCL_obj
} NclBasicDataTypes;

typedef NclQuark string; /* Makes this a quark type */
typedef char byte;
typedef int logical;
typedef int nclfile;

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
	nclfile nclfileval;
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
