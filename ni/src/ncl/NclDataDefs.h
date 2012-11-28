
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
Ncl_Typeuint64 =                0100,
Ncl_Typeint64 =                 0200,
Ncl_Typeulong =                 0300,
Ncl_Typelong = 	  		0400,
Ncl_Typeuint =                  01000,
Ncl_Typeint = 		 	02000,
Ncl_Typeushort =                03000,
Ncl_Typeshort = 	 	04000,
Ncl_Typeubyte =                 05000,
Ncl_Typebyte= 			06000,
Ncl_Typestring = 		010000,
Ncl_Typechar = 	 		020000,
Ncl_Typeobj= 			040000,
Ncl_Var = 			0100000,
Ncl_Att = 			0200000,
Ncl_Typelogical =    		0400000,
Ncl_HLUObj = 			01000000,
Ncl_File = 			02000000,
Ncl_Group = 			03000000,
Ncl_FileVar = 			04000000,
Ncl_FileGroup = 		06000000,
Ncl_Typereference =    		07000000,
Ncl_HLUVar = 			010000000,
Ncl_CoordVar = 			020000000,
Ncl_Type = 			040000000,
Ncl_MultiDValnclfileData =	0100000000,
Ncl_MultiDValHLUObjData = 	0200000000,
Ncl_OneDValCoordData = 		0400000000,
Ncl_List = 			01000000000,
Ncl_MultiDVallistData = 	02000000000,
Ncl_ListVar = 			04000000000,
Ncl_Typelist = 			010000000000,
Ncl_Typegroup = 		020000000000,
Ncl_Typecompound = 		030000000000
} NclObjTypes;

/*
* allows for selection of basic variable data value type 
*/
#define NCL_VAR_TYPE_MASK	((unsigned long) (Ncl_Var | Ncl_FileVar | Ncl_CoordVar | Ncl_FileGroup)) 
#define NCL_COORD_MASK		((unsigned long) Ncl_OneDValCoordData)
#define NCL_HLU_MASK		((unsigned long) Ncl_MultiDValHLUObjData)
#define NCL_MDV_MASK		((unsigned long) (Ncl_MultiDValData | Ncl_MultiDValnclfileData))
#define NCL_MD_MASK		(NCL_HLU_MASK | NCL_MDV_MASK | NCL_COORD_MASK)

/*
* allows for selection of numeric value types which represent one 
* group of data types that can be coerced
*/
#define NCL_NUMERIC_TYPE_MASK	((unsigned long)(Ncl_Typeint | Ncl_Typedouble | Ncl_Typebyte | Ncl_Typelong | Ncl_Typeshort | Ncl_Typefloat))
#define NCL_ENUMERIC_TYPE_MASK	((unsigned long)(Ncl_Typeushort | Ncl_Typeuint | Ncl_Typeulong | Ncl_Typeint64 | Ncl_Typeuint64 | Ncl_Typeubyte))
#define NCL_CHARSTR_TYPE_MASK	((unsigned long)(Ncl_Typestring | Ncl_Typechar))

#define NCL_SNUMERIC_TYPE_MASK	(NCL_NUMERIC_TYPE_MASK | NCL_ENUMERIC_TYPE_MASK)

/*
* allows for selection of basic data value type
*/
#define NCL_VAL_TYPE_MASK	(((unsigned long) Ncl_Typelogical) | NCL_CHARSTR_TYPE_MASK | NCL_SNUMERIC_TYPE_MASK | Ncl_Typelist)

typedef enum  {
NCL_none = 	0,
NCL_byte = 	010,
NCL_ubyte =     011,
NCL_char = 	013,
NCL_short = 	020,
NCL_ushort =    021,
NCL_int = 	040,
NCL_uint =      041,
NCL_float =	042,
NCL_long = 	044,
NCL_ulong =     045,
NCL_int64 =     0100,
NCL_uint64 =    0101,
NCL_double =	0102,
NCL_string = 	0200,
NCL_numeric = 	01000,
NCL_enumeric = 	02000,
NCL_snumeric = 	04000,
NCL_logical = 	010000,
NCL_obj = 	020000,
NCL_list = 	040000,
NCL_group = 	0100000,
NCL_compound = 	0200000,
NCL_opaque =	0400000,
NCL_enum =	01000000,
NCL_vlen =	02000000,
NCL_reference =	04000000,
NCL_virtual =	010000000,
NCL_listarray =	020000000
} NclBasicDataTypes;

typedef char byte;
typedef unsigned char ubyte;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef long long int64;
typedef unsigned long long uint64;
typedef int logical;
typedef int obj;
typedef int list;
typedef int group;
typedef int nclcompound;
typedef int objref;

typedef union _NclScalar {
	double             doubleval;
	unsigned long long uint64val;
	long long          int64val;
	unsigned short	   ushortval;
	unsigned int	   uintval;
	unsigned long      ulongval;
	long               listval;
	float 	           floatval;
	int	           intval;
	long               longval;
	short              shortval;
	unsigned char  	   charval;
	NclQuark           stringval;
	char               byteval;
	unsigned char      ubyteval;
	logical            logicalval;
	obj                objval;
	group              groupval;
	nclcompound        compoundval;
	objref             objrefval;
}NclScalar;

typedef struct _NclRefList{
/*
        struct _NclObjRec *pptr;
*/
	int	pid;
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

typedef enum { COORD_VECT, COORD_RANGE, COORD_SINGLE, INT_VECT, INT_RANGE, INT_SINGLE } NclSubTypes;

typedef	struct _NclRangeRec {
	struct _NclMultiDValDataRec *start;
	struct _NclMultiDValDataRec *finish;
	struct _NclMultiDValDataRec *stride;
	int is_single;
} NclRangeRec;

typedef	struct _NclVecRec {
	struct _NclMultiDValDataRec *vec;
} NclVecRec;


typedef	struct _NclSubRec {
	NclSubTypes sub_type;
	char *name;
	int tolerence;  /* applies only to coordinate variables */
	union {
		struct _NclRangeRec range;
		struct _NclVecRec vec;
	}u;
} NclSubRec;

typedef enum stack_value_types { 
	NclStk_NOVAL = 0, NclStk_OFFSET = 01, 
	NclStk_VAL = 02, NclStk_VAR = 04, NclStk_SUBREC = 010,
	NclStk_PARAMLIST = 020, NclStk_RANGEREC = 040,
	NclStk_VECREC = 0100, NclStk_FILE = 0200, NclStk_GRAPHIC = 0400,
	NclStk_RETURNVAL = 01000, NclStk_STATIC_LINK = 02000, 
	NclStk_DYNAMIC_LINK = 04000, NclStk_RET_OFFSET = 010000, NclStk_LIST = 020000
	} NclStackValueTypes;


typedef struct _NclStackEntry{
	NclStackValueTypes kind;
	union {
		unsigned long   offset;
/*
* All of the following must be pointers to pointers so changes
* made such as allocating a new record can propagte to copies
* an example is an array passed to a function with two parameters
* twice.
*/
		struct _NclRangeRec range_rec;
		struct _NclVecRec vec_rec;
		struct _NclSubRec sub_rec;
		struct _NclParamRecList *the_list;
		struct _NclVarRec	*data_var;
		struct _NclFileRec	*data_group;
		struct _NclMultiDValDataRec 	*data_obj;
		struct _NclListRec 	*data_list;
	}u;
}NclStackEntry;

typedef struct _NclVectorSelection{
        ng_size_t n_ind;
        long *ind;
        long min;
        long max;
}NclVectorSelection;

typedef struct _NclSubscriptSelection{
        long start;
        long finish;
        long stride;
	int is_single;
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
