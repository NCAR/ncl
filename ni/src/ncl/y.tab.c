extern char *malloc(), *realloc();

# line 2 "ncl.y"
#include <stdio.h>
/*
#include <pfmt.h>
*/
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclDataDefs.h"
#include "Symbol.h"
#include "SrcTree.h"
#include "Machine.h"
#include <errno.h>
#include <ctype.h>
int scopelevel = 0;
extern int yydebug;
extern char yytext[];
extern int yylineno;
extern FILE *thefptr;
extern FILE *theoptr;
extern int cmd_line;
extern int cur_line_length;
extern int cur_line_number;
extern int last_line_length;
extern char cur_line_text[512];
extern int ok_to_start_vsblk;
#define ERROR(x)  NhlPError(NhlFATAL,NhlEUNKNOWN,"%s",(x))
int is_error = 0;
/*
extern int _NclTranslate(
#ifdef NhlNeedProto
void*,   
FILE* 
#endif
);
*/
extern void _NclTransTerminate(
#ifdef NhlNeedProto
void
#endif
);

extern int rec; 
extern FILE* recfp;
extern FILE* yyin;

int loading = 0;
int top_level_line;
char *cur_load_file = NULL;


# line 52 "ncl.y"
typedef union  {
	int integer;
	double real;
	char  str[NCL_MAX_STRING];
	struct _NclSymbol *sym;
	void *src_node;
	struct src_node_list *list;
	struct ncl_rcl_list *array;
} YYSTYPE;
# define EOLN 257
# define RP 258
# define LP 259
# define RBC 260
# define LBC 261
# define RBK 262
# define LBK 263
# define COLON 264
# define SEMI 265
# define MARKER 266
# define LPSLSH 267
# define SLSHRP 268
# define DIM_MARKER 269
# define INT 270
# define DIMNUM 271
# define REAL 272
# define STRING 273
# define DIM 274
# define DIMNAME 275
# define ATTNAME 276
# define COORD 277
# define FVAR 278
# define INTEGER 279
# define FLOAT 280
# define LONG 281
# define DOUBLE 282
# define BYTE 283
# define CHARACTER 284
# define GRAPHIC 285
# define STRNG 286
# define NUMERIC 287
# define FILETYPE 288
# define SHORT 289
# define LOGICAL 290
# define UNDEF 291
# define VAR 292
# define WHILE 293
# define DO 294
# define QUIT 295
# define PROC 296
# define EPROC 297
# define NPROC 298
# define IPROC 299
# define UNDEFFILEVAR 300
# define BREAK 301
# define NOPARENT 302
# define BGIN 303
# define END 304
# define FUNC 305
# define EFUNC 306
# define NFUNC 307
# define IFUNC 308
# define FDIM 309
# define IF 310
# define THEN 311
# define VBLKNAME 312
# define FILEVAR 313
# define CONTINUE 314
# define DFILE 315
# define KEYFUNC 316
# define KEYPROC 317
# define ELSE 318
# define EXTERNAL 319
# define RETURN 320
# define VSBLKGET 321
# define LOAD 322
# define NEW 323
# define OBJVAR 324
# define OBJTYPE 325
# define RECORD 326
# define VSBLKCREATE 327
# define VSBLKSET 328
# define LOCAL 329
# define STOP 330
# define OR 331
# define XOR 332
# define AND 333
# define GT 334
# define GE 335
# define LT 336
# define LE 337
# define EQ 338
# define NE 339
# define UNOP 340
# define NOT 341
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 1583 "ncl.y"

yyerror
#if __STDC__
(char *s)
#else 
(s)
	char *s;
#endif
{
	extern int is_error;
	int i,len;
	char error_buffer[NCL_MAX_STRING];
	

	is_error += 1;

	if(is_error < NCL_MAX_ERROR) {
		if(yytext[0] == '\n') {
			sprintf(error_buffer,"%s\n",cur_line_text);
			len = strlen(error_buffer);
			for(i=0; i<last_line_length-1;i++) sprintf(&(error_buffer[len+i]),"-");
			sprintf(&(error_buffer[len+last_line_length-1]),"^\n");
			if(loading) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d in file %s before or near \\n \n%s\n",s,cur_line_number,cur_load_file,error_buffer);
			} else if(cmd_line){
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d before or near \\n \n%s\n",s,cur_line_number-1,error_buffer);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d before or near \\n \n%s\n",s,cur_line_number,error_buffer);
			} 
		} else {
			sprintf(error_buffer,"%s\n",cur_line_text);
			len = strlen(error_buffer);
			for(i=0; i<cur_line_length-1;i++) sprintf(&(error_buffer[len+i]),"-");
			sprintf(&(error_buffer[len+cur_line_length-1]),"^\n");
			if(loading) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d in file %s before or near %s \n%s\n",s,cur_line_number,cur_load_file,yytext,error_buffer);
			} else if(cmd_line){
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d before or near %s \n%s\n",s,cur_line_number-1,yytext,error_buffer);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: line %d before or near %s \n%s\n",s,cur_line_number,yytext,error_buffer);
			}
		}
	} else if(is_error == NCL_MAX_ERROR) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Maximum number of errors exceeded, no more will be printed");
	}
	return(0);
}
int yyexca[] ={
-1, 0,
	257, 16,
	-2, 0,
-1, 1,
	0, -1,
	257, 16,
	-2, 0,
-1, 27,
	257, 16,
	304, 16,
	-2, 0,
-1, 84,
	257, 16,
	-2, 0,
-1, 101,
	257, 55,
	304, 55,
	-2, 0,
-1, 166,
	257, 16,
	304, 16,
	-2, 0,
-1, 167,
	257, 16,
	304, 16,
	318, 16,
	-2, 0,
-1, 186,
	257, 61,
	-2, 0,
-1, 189,
	257, 55,
	-2, 0,
-1, 235,
	257, 16,
	-2, 0,
-1, 237,
	257, 16,
	-2, 0,
-1, 303,
	257, 16,
	304, 16,
	-2, 0,
-1, 305,
	257, 16,
	304, 16,
	-2, 0,
-1, 330,
	257, 61,
	-2, 0,
-1, 344,
	257, 16,
	304, 16,
	-2, 0,
-1, 348,
	257, 16,
	-2, 0,
-1, 351,
	257, 16,
	-2, 0,
-1, 376,
	257, 16,
	304, 63,
	-2, 0,
-1, 391,
	257, 16,
	304, 16,
	-2, 0,
	};
# define YYNPROD 226
# define YYLAST 2054
int yyact[]={

   181,     9,   374,    59,    18,    18,   157,     2,    44,   342,
   333,   321,    62,   320,   365,    51,   146,   386,   272,   385,
    68,    70,    34,    56,   326,    55,    57,   325,    35,    36,
    90,    18,    88,   247,    85,   395,   394,   393,   392,   350,
   347,    99,   101,   405,    42,    41,   161,    48,    48,    47,
   390,    42,    41,    27,   126,   127,   259,   406,    64,    66,
    67,    65,   269,   128,   401,    42,    41,    89,    43,   135,
   136,   346,   138,   187,   141,    43,    63,    39,    40,   345,
    69,    48,    48,   328,    39,    40,   191,    98,    18,    43,
   166,   103,    97,    48,   304,   381,   106,   107,    39,    40,
   102,   145,   371,   190,   362,   145,   370,   187,   305,   194,
   195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
   205,   206,   207,   208,   209,   210,   211,   259,   379,   363,
   213,   117,    48,   108,   263,    42,    41,    48,   116,   114,
   301,   115,   231,   118,   187,    48,   179,   275,    42,    41,
   243,   192,   193,   153,   152,   151,   112,   230,   113,    43,
   164,   336,   150,   149,   148,   147,   234,   242,    39,    40,
    18,    18,    43,   236,   238,   257,   183,   145,   145,   261,
   163,    39,    40,   252,   255,   191,   105,   117,   104,   108,
   119,   170,   162,   184,   116,   114,   375,   115,   169,   118,
   376,   171,   190,    50,   229,   214,    49,   353,   172,   173,
   232,   233,   112,   310,   113,   100,   135,   291,   265,   271,
   270,   289,   293,   258,   186,   289,   288,   260,    84,   223,
   224,   369,   256,   223,   221,   262,   368,   338,    95,    18,
   337,    18,   382,   145,   313,    94,   119,   296,    92,   360,
    42,    41,   315,   241,   318,    93,    96,    91,   175,   155,
   117,   324,   108,   154,    73,   246,   273,   116,   114,   129,
   115,   295,   118,    83,    43,   273,    82,   297,   298,    79,
   246,   246,    78,    39,    40,   112,   246,   113,   278,   276,
   277,   280,   282,   281,   285,   286,   284,   283,   279,   287,
   344,   246,   294,   228,   246,   322,   323,    18,   219,    18,
   349,   327,   352,   340,   341,   218,    48,   358,   359,   119,
   387,   134,   188,   185,   177,   248,   176,   250,   222,    52,
   165,   143,   119,    71,   364,   220,   343,    26,   400,   399,
    24,   273,   273,   142,   332,   373,   356,   167,    18,   189,
   367,   377,    18,    53,   396,    18,   366,   372,   383,   367,
    33,   384,   117,    81,   108,   366,    80,   235,   237,   116,
    32,   139,   292,    60,   118,    58,   391,    61,   256,    12,
    18,   268,    53,   144,   388,     1,    13,    11,    10,   335,
     8,   389,     7,     6,   402,    18,   314,     5,   377,     0,
     0,   403,   404,     0,   398,     0,     0,     0,    16,     0,
   397,     0,   273,     0,   329,     0,     0,     0,     0,   273,
     0,   119,   174,     0,     0,     0,   156,   109,   111,   110,
   122,   121,   123,   120,   124,   125,   334,     0,     0,     0,
     0,   339,     0,    42,    41,   290,    28,    15,    20,    21,
    22,    19,    53,    30,     0,    27,     0,    23,     0,   248,
     0,     0,    29,   361,     0,     0,    31,    43,    35,    36,
     0,    25,    14,    38,    87,   225,    39,    40,    86,   355,
    37,    16,    17,   109,   111,   110,   122,   121,   123,   120,
   124,   125,   331,   330,   354,   319,     0,   239,   226,   227,
   309,   240,   244,     0,   348,     0,   351,     0,     0,     0,
   254,     0,   264,     0,    53,   306,    42,    41,   245,    28,
    15,    20,    21,    22,    19,     0,    30,     0,    27,   217,
    23,     0,     0,    48,     0,    29,     0,   267,     0,    31,
    43,    35,    36,    53,    25,    14,    38,    87,     0,    39,
    40,    86,   274,    37,     0,    17,   109,   111,   110,   122,
   121,   123,   120,   124,   125,     0,     0,    62,   307,   312,
   308,   311,   182,     0,     0,    68,     0,   117,    56,   108,
    55,    57,   180,     0,   116,   114,     0,   115,     0,   118,
     0,     0,     0,     0,     0,     0,    62,     0,   178,    42,
    41,   182,     0,     0,    68,    53,     0,    56,     0,    55,
    57,   180,     0,    64,    66,    67,    65,     0,     0,     0,
     0,     0,     0,    43,     0,     0,     0,     0,    42,    41,
     0,    63,    39,    40,    53,    69,   119,     0,     0,     0,
     0,     0,    64,    66,    67,    65,     0,     0,     0,    54,
     0,     0,    43,     0,     0,     0,     0,     0,     0,     0,
    63,    39,    40,     0,    69,     0,    62,     0,     0,     0,
     0,   182,     0,     0,    68,     0,     0,    56,    54,    55,
    57,   249,     0,     0,   117,     0,   108,     0,     0,     0,
     0,   116,   114,     0,   115,    53,   118,     0,    42,    41,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
     0,   113,    64,    66,    67,    65,     0,     0,     0,     0,
     0,     0,    43,    53,     0,     0,     0,     0,    62,     0,
    63,    39,    40,   316,    69,     0,    68,     0,     0,    56,
     0,    55,    57,   119,     0,     0,     0,     0,    54,     0,
     0,     0,     0,     0,     0,     0,     0,    62,     0,     0,
    42,    41,   182,     0,     0,    68,     0,     0,    56,     0,
    55,    57,     0,     0,    64,    66,    67,    65,    72,     0,
     0,     0,     0,     0,    43,     0,     0,     0,     0,    42,
    41,     0,    63,    39,    40,     0,    69,     0,     0,    74,
    75,    76,    77,    64,    66,    67,    65,     0,     0,     0,
    54,     0,     0,    43,     0,     0,     0,     0,     0,    62,
     0,    63,    39,    40,   253,    69,     0,    68,     0,     0,
    56,     0,    55,    57,     0,     0,     0,     0,     0,    54,
     0,     0,     0,   130,   131,   132,   133,   140,    62,     0,
     0,    42,    41,     0,     0,     0,    68,     0,     0,    56,
     0,    55,    57,     0,     0,    64,    66,    67,    65,     0,
     0,     0,     0,     0,     0,    43,     0,     0,     0,     0,
    42,    41,     0,    63,    39,    40,     0,    69,     0,     0,
     0,     0,     0,     0,    64,    66,    67,    65,     0,     0,
     0,    54,     0,     0,    43,     0,   137,     0,     0,    62,
     0,     0,    63,    39,    40,     0,    69,    68,     0,   117,
    56,   108,    55,    57,     0,     0,   116,   114,     0,   115,
    54,   118,     0,     0,     0,     0,     0,    62,     0,     0,
     0,    42,    41,     0,   112,    68,   113,     0,    56,     0,
    55,    57,     0,     0,     0,    64,    66,    67,    65,     0,
     0,     0,     0,     0,     0,    43,     0,     0,     0,    42,
    41,     0,     0,    63,    39,    40,     0,    69,   119,     0,
     0,     0,     0,    64,    66,    67,    65,     0,     0,     0,
     0,    54,     0,    43,     0,     0,     0,     0,     0,     0,
     0,    63,    39,    40,   117,    69,   108,    16,     0,     0,
     0,   116,   114,     0,   115,     0,   118,     0,     0,    54,
     0,     0,     0,     0,     0,     0,   117,     0,   108,   112,
     0,   113,     0,   116,   114,   299,   115,     0,   118,     0,
     0,     0,    42,    41,     0,    28,    15,    20,    21,    22,
    19,   112,    30,   113,    27,   302,    23,     0,     0,     0,
     0,    29,     0,   119,     0,    31,    43,    35,    36,   303,
    25,    14,    38,   159,     0,    39,    40,   158,     0,    37,
   117,    17,   108,     0,     0,   119,     0,   116,   114,   266,
   115,   117,   118,   108,     0,     0,     0,     0,   116,   114,
     0,   115,     0,   118,     0,   112,     0,   113,     0,     0,
     0,     0,     0,   117,     0,   108,   112,     0,   113,     0,
   116,   114,   117,   115,   108,   118,     0,     0,     0,   116,
   114,   215,   115,     0,   118,     0,     0,     0,   112,   119,
   113,     0,     0,     0,   117,     0,   108,   112,   357,   113,
   119,   116,   114,   117,   115,   108,   118,     0,     0,     0,
   116,   114,     0,   115,     0,   118,     0,     0,     0,   112,
     0,   113,   119,     0,     0,   117,     0,   108,   112,     0,
   113,   119,   116,   114,     0,   115,   117,   118,   108,     0,
     0,     0,     0,   116,   114,     0,   115,     0,   118,     0,
   112,    16,   113,   119,     0,     0,     0,     0,     0,     0,
     0,   112,   119,   113,     0,   109,   111,   110,   122,   121,
   123,   120,   124,   125,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   317,   119,     0,    42,    41,     0,    28,
    15,    20,    21,    22,    19,   119,    30,     0,    27,   380,
    23,     0,     0,     0,     0,    29,     0,     0,     0,    31,
    43,    35,    36,     0,    25,    14,    38,   159,     0,    39,
    40,   158,     0,    37,     0,    17,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    16,     0,     0,     0,
   109,   111,   110,   122,   121,   123,   120,   124,   125,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   251,     0,   109,   111,   110,   122,   121,   123,   120,   124,
   125,    42,    41,     0,    28,    15,    20,    21,    22,    19,
     0,    30,     0,    27,   378,    23,     0,     0,     0,     0,
    29,     0,     0,     0,    31,    43,    35,    36,     0,    25,
    14,    38,   159,     0,    39,    40,   158,   212,    37,     0,
    17,     0,     0,     0,     0,     0,   109,   111,   110,   122,
   121,   123,   120,   124,   125,     0,     0,   109,   111,   110,
   122,   121,   123,   120,   124,   125,     0,     0,     0,     0,
     0,     0,     0,   216,     0,     0,     0,     0,     0,   109,
   111,   110,   122,   121,   123,   120,   124,   125,   109,   111,
   110,   122,   121,   123,   120,   124,   125,     0,     0,   168,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   109,   111,   110,   122,   121,   123,   120,   124,   125,   109,
   111,   110,   122,   121,   123,   120,   124,   125,    16,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   109,   111,   110,   122,   121,   123,   120,   124,   125,
     0,     0,     0,     0,   110,   122,   121,   123,   120,   124,
   125,     0,     0,    42,    41,     0,    28,    15,    20,    21,
    22,    19,    16,    30,     0,    27,   300,    23,     0,     0,
     0,     0,    29,     0,     0,     0,    31,    43,    35,    36,
     0,    25,    14,    38,   159,     0,    39,    40,   158,     0,
    37,     0,    17,     0,     0,     0,     0,    42,    41,     0,
    28,    15,    20,    21,    22,    19,    16,    30,     0,    27,
   160,    23,     0,     0,     0,     0,    29,     0,     0,     0,
    31,    43,    35,    36,     0,    25,    14,    38,   159,     0,
    39,    40,   158,     0,    37,     0,    17,     0,     0,     0,
     0,    42,    41,     0,    28,    15,    20,    21,    22,    19,
    16,    30,     0,    27,     0,    23,     0,     0,     0,     0,
    29,     0,     0,     0,    31,    43,    35,    36,     0,    25,
    14,    38,   159,     0,    39,    40,   158,     0,    37,     0,
    17,     0,     0,     0,     0,    42,    41,     0,    28,    15,
    20,    21,    22,    19,    16,    30,     0,    27,     0,    23,
     0,     0,     0,     0,    29,     0,     0,     0,    31,    43,
    35,    36,     0,    25,    14,    38,    87,     0,    39,    40,
    86,     0,    37,     0,    17,     0,     0,     0,     0,    42,
    41,     0,    28,    15,    20,    21,    22,    19,    16,    30,
     0,    27,     0,    23,     0,     0,     0,     0,    29,     0,
     0,     0,    31,    43,    35,    36,     0,    25,    14,    38,
    46,     0,    39,    40,    45,     0,    37,     0,    17,     0,
     0,     0,     0,    42,    41,     0,    28,    15,    20,    21,
    22,    19,     0,    30,     0,    27,     0,    23,     0,     0,
     0,     0,    29,     0,     0,     0,    31,    43,    35,    36,
     0,    25,    14,    38,     4,     0,    39,    40,     3,   117,
    37,   108,    17,     0,     0,     0,   116,   114,     0,   115,
     0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   112,     0,   113,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   122,   121,
   123,   120,   124,   125 };
int yypact[]={

  1422,  1378,    59,   -67,   -70, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,   678, -1000, -1000,  -305,   272,     5,
     5,     5,     5,     5,    23,  -288,    17,  1334,  -226,   678,
 -1000, -1000, -1000, -1000,   -21,  -199,  -204,   -41,  -156, -1000,
 -1000, -1000, -1000, -1000,    59,   -85,   -87, -1000, -1000,    59,
    59,  1140, -1000,   678,   678, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,   678,    10,     5,     5,     5,     5,   678,   650,
 -1000,   678, -1000,   589, -1000, -1000, -1000, -1000,  -143, -1000,
     4,     0,  -143, -1000,  1246,  -112,   -93,  -113,   269,   678,
  1118,   -68,  -247,    -1, -1000,   337,   -83, -1000, -1000,  -166,
 -1000,   -71, -1000, -1000,    59,    59, -1000, -1000,   678,   678,
   678,   678,   678,   678,   678,   678,   678,   678,   678,   678,
   678,   678,   678,   678,   678,   678, -1000, -1000,  1109,   678,
 -1000, -1000, -1000, -1000,   -63,  1087,  1078, -1000,  1140,   271,
 -1000,  1140,    50, -1000,   291,   -30,   -34, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -143,  -143,    45,    59,  -116,  -131,
 -1000, -1000, -1000,    59,    59,   678,   225,  1334, -1000, -1000,
   337,  -247,    -6,  -109, -1000,   337,   260, -1000,   407, -1000,
   498,  1056,   560,   337, -1000,  -120,  -129,   -37,  -125,  -170,
   -46, -1000, -1000, -1000,   238,  1151,  1714,  1151,   542,   542,
   327,   327,   238,   238,   238,   238,   649,   649,   649,   649,
   649,   649, -1000,  1045, -1000,   678,  -240, -1000,   678,    59,
  -143,     9,   -38,   175,     9,   -42,    44,    13,    59, -1000,
    59,    59, -1000, -1000,   991,  1202,  -164,   751,  -210,   257,
 -1000,   337, -1000,   337,   242,   -53,   308,   -16, -1000,   498,
 -1000,   469,   969,   678,   237,  -315, -1000,  -317,    59,    59,
   678,  -294, -1000,  -297,    59,  -208,     9, -1000,  -166, -1000,
 -1000,  1140,  -319, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,     9,   119,
   -22,   -25, -1000,     9,    59,    59,  -320, -1000, -1000,   678,
  -215,  -223,  -270,  1334,  -271,  1334,   -59,   236,   221, -1000,
 -1000, -1000,   407, -1000, -1000,   884,   678,   678,  1140, -1000,
 -1000, -1000, -1000, -1000,  1140, -1000, -1000, -1000, -1000,   205,
  -200,  -175,  -250,  -143, -1000,   -26,   -31, -1000, -1000, -1000,
  -167,  -171,  -143,  -250,   152, -1000, -1000, -1000,  1040,  -176,
 -1000,   945,  -209, -1000, -1000, -1000,   -18,   678,  1140,  1140,
   678, -1000,  -308,  -310, -1000,   276, -1000, -1000, -1000, -1000,
 -1000, -1000,   276, -1000,  -254,   678,  1290,    59,  -272,  -273,
  -274,  -275, -1000,  1140,    96, -1000, -1000,  -143, -1000, -1000,
  -230,   225, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -250,
  -250, -1000,  -261, -1000, -1000,  -237, -1000 };
int yypgo[]={

     0,   321,     6,   397,   393,   392,   390,     1,   388,   387,
   386,   385,   383,     3,     0,   381,   324,   379,   377,    33,
   146,   329,   375,   373,   331,   343,   326,   778,   200,   224,
   328,   371,     2,   223,   370,   360,   322,   349,   147,    16,
    22,   340,   337,    46,    18,   347,    14,   344,   339,   338,
   336 };
int yyr1[]={

     0,    11,    11,    11,    11,    11,    11,    28,    28,    28,
    28,    28,    28,    44,    44,    43,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    17,    17,     9,     9,     9,     9,     9,     9,    45,    45,
    10,    10,    15,    15,    18,    18,    18,    34,    34,    34,
    35,    35,    35,    37,    37,    36,    36,    36,    29,    29,
    29,    33,    33,    32,    32,     8,     8,     8,     8,     7,
     7,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    27,    27,    31,    31,    41,    46,    46,    46,    46,    47,
     5,    48,     5,     5,     5,    25,    25,    24,    24,    12,
    12,    12,    12,    12,    12,    12,    12,    39,    39,    39,
    39,    39,    39,    39,    38,    38,    38,    38,    38,    38,
    38,    38,    38,    38,    38,    38,    30,    30,    30,    30,
    42,    49,     6,    50,     6,     6,     6,     3,    13,    13,
    13,    13,    13,    13,    13,    13,    13,    13,    13,    13,
    13,    13,    13,    13,    13,    13,    13,    13,    40,    40,
    40,    40,    40,    26,    26,    26,    26,    16,    16,    19,
    19,    20,    20,    20,    20,    20,    20,    20,    20,    20,
    20,    20,    20,    20,    14,    14,    14,    14,    14,    14,
    14,    14,    14,    14,    14,    14,    14,    14,    14,    14,
    14,    14,    14,    14,    14,    21,    21,    21,    21,    21,
    21,    21,    21,    21,    21,    22,    22,    22,    22,    22,
    22,    22,    22,    23,     1,     1 };
int yyr2[]={

     0,     5,     7,     9,     7,     7,     9,     5,     7,     9,
     7,     7,     9,     1,     3,     3,     1,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     5,     3,     3,     5,
     3,     3,    13,    17,    13,    17,    17,    17,     0,     2,
     3,     3,     3,     3,    15,    15,     5,    11,    11,     5,
    11,    11,     5,     5,     7,     1,     7,     3,     5,     7,
     7,     1,     7,     3,     3,    19,    23,    13,    13,     7,
     7,     5,     5,     5,     5,     3,     3,     3,     3,     5,
     7,     5,     3,     7,     5,     3,     3,     7,     7,     1,
    15,     1,    21,    15,     5,     1,     3,     3,     7,     3,
     7,     5,     9,     3,     7,     5,     9,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     7,     7,     9,     9,
     5,     1,    21,     1,    15,    15,     5,     7,     3,     5,
     7,    13,    11,     9,     7,    13,     7,     9,    13,     7,
     5,    11,     5,    11,     9,     5,    11,     7,     3,     3,
     3,     3,     3,     3,     7,     7,    11,     3,     5,     3,
     5,     3,     3,     7,     5,     5,     9,     7,     7,    11,
     9,     9,     5,     7,     3,     5,     5,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     3,     3,     3,     3,     3,
     3,     3,     7,    17,    13,     5,     5,     5,     5,     3,
     3,     3,     3,     7,     3,     7 };
int yychk[]={

 -1000,   -11,    -2,   326,   322,    -3,    -4,    -5,    -6,    -7,
    -8,    -9,   -17,   -10,   320,   295,   256,   330,   -13,   299,
   296,   297,   298,   305,   -41,   319,   -42,   303,   294,   310,
   301,   314,   -34,   -35,   -40,   316,   317,   328,   321,   324,
   325,   292,   291,   315,    -2,   326,   322,   -43,   257,   273,
   273,   -14,   -21,    45,   341,   272,   270,   273,   -22,   -13,
   -23,   -18,   259,   323,   305,   308,   306,   307,   267,   327,
   326,    61,   -27,   259,   -27,   -27,   -27,   -27,   259,   256,
   -41,   -42,   259,   256,   -28,    -2,   326,   322,   -13,   293,
   -14,   278,   269,   276,   266,   259,   277,   291,   291,   -13,
   256,   -13,   256,   -43,   273,   273,   -43,   -43,    37,   331,
   333,   332,    60,    62,    43,    45,    42,    35,    47,    94,
   337,   335,   334,   336,   338,   339,   -14,   -14,   -14,   259,
   -27,   -27,   -27,   -27,    -1,   -14,   -14,   256,   -14,   -31,
   258,   -14,   -25,   -24,   -12,   -40,   -39,   308,   307,   306,
   305,   298,   297,   296,   259,   259,   -25,    -2,   326,   322,
   304,   -43,   304,   273,   273,    61,   -14,   -45,   311,   266,
   259,   269,   276,   277,   -21,   259,   -26,   -16,   261,   -20,
   274,   -14,   264,   259,   276,   -33,   -29,   273,   -36,   -37,
   273,   256,   -43,   -43,   -14,   -14,   -14,   -14,   -14,   -14,
   -14,   -14,   -14,   -14,   -14,   -14,   -14,   -14,   -14,   -14,
   -14,   -14,   258,   -14,   268,    44,   325,   258,    44,   258,
    44,   264,   -30,   263,   264,   -30,   -25,   -25,   258,   -43,
   273,   273,   -43,   -43,   -14,   -28,    -2,   -28,    -2,   -26,
   -21,   259,   276,   259,   -26,   258,    44,   -19,   -20,   274,
   -20,   264,   -14,   264,   -26,   304,   -43,   304,   -33,   256,
   264,   304,   -43,   304,   -36,   264,    44,    -1,   -15,   302,
   -13,   -14,   -44,   -43,   -24,   -38,   280,   281,   279,   289,
   282,   284,   283,   288,   287,   285,   286,   290,   264,   263,
   270,    42,   -38,   264,   258,   258,   -44,   -43,   -43,    44,
   304,   304,   304,   318,   304,   318,   258,   -26,   -26,   258,
   266,   -16,   261,   260,   -20,   -14,   264,   264,   -14,   258,
   328,   328,   -43,   -43,   -14,   321,   321,   -43,   291,   -38,
   -29,   -33,   -47,   329,   -38,   270,    42,   262,   262,   -38,
   -44,   -44,   329,   -50,   -14,   294,   294,   310,   -28,    -2,
   310,   -28,    -2,   266,   258,   258,   -19,   264,   -14,   -14,
    44,   258,   304,   304,    -7,   -46,   -40,   -39,   262,   262,
   273,   273,   -46,    -7,   -32,    44,   -28,    -2,   304,   304,
   304,   304,   260,   -14,   -14,   327,   327,    44,   -44,   -44,
   304,   -14,   310,   310,   310,   310,   258,   -40,   -39,   -48,
   -49,   294,   -32,    -7,    -7,   304,   294 };
int yydef[]={

    -2,    -2,     0,     0,     0,    17,    18,    19,    20,    21,
    22,    23,    24,    25,     0,    27,    28,     0,     0,    75,
    76,    77,    78,     0,     0,     0,     0,    -2,     0,     0,
    30,    31,    40,    41,   138,     0,     0,     0,     0,   158,
   159,   160,   161,   162,     0,     0,     0,     1,    15,     0,
     0,    26,   184,     0,     0,   205,   206,   207,   208,   209,
   210,   211,     0,     0,   220,   219,   221,   222,     0,     0,
    29,     0,    71,     0,    72,    73,    74,    79,    95,    94,
     0,     0,    95,   136,    -2,     0,     0,     0,     0,     0,
    38,   139,     0,   150,   152,     0,   155,    84,   130,    61,
    49,    -2,    52,     2,     0,     0,     4,     5,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   185,   186,     0,     0,
   215,   216,   217,   218,     0,   224,     0,    46,   137,     0,
    81,    82,     0,    96,    97,    99,   103,   107,   108,   109,
   110,   111,   112,   113,    95,    95,     0,     0,     0,     0,
    69,     7,    70,     0,     0,     0,    -2,    -2,    39,   140,
     0,     0,   144,   146,   149,     0,     0,   163,     0,   167,
     0,   171,   172,     0,   157,     0,    -2,     0,     0,    -2,
     0,    57,     3,     6,   187,   188,   189,   190,   191,   192,
   193,   194,   195,   196,   197,   198,   199,   200,   201,   202,
   203,   204,   212,     0,   223,     0,     0,    80,     0,    13,
     0,     0,   101,     0,     0,   105,     0,     0,    13,     8,
     0,     0,    10,    11,     0,    -2,     0,    -2,     0,     0,
   143,     0,   147,     0,     0,   154,     0,     0,   169,     0,
   168,   175,   174,   182,     0,     0,    58,     0,     0,     0,
     0,     0,    53,     0,     0,     0,     0,   225,    61,    42,
    43,    83,    89,    14,    98,   100,   114,   115,   116,   117,
   118,   119,   120,   121,   122,   123,   124,   125,     0,     0,
     0,     0,   104,     0,    13,    13,   133,     9,    12,     0,
     0,     0,     0,    -2,     0,    -2,   142,     0,     0,   151,
   153,   165,     0,   164,   170,   173,   178,   177,   183,   156,
    47,    48,    59,    60,    62,    50,    51,    54,    56,     0,
    -2,     0,     0,     0,   102,     0,     0,   126,   127,   106,
     0,     0,     0,     0,    -2,    67,    68,    32,    -2,     0,
    34,    -2,     0,   141,   145,   148,     0,   176,   180,   181,
     0,   214,     0,     0,    90,    13,    85,    86,   128,   129,
    93,   135,    13,   134,     0,     0,    -2,    64,     0,     0,
     0,     0,   166,   179,     0,    44,    45,     0,    91,   131,
     0,    -2,    33,    37,    35,    36,   213,    87,    88,     0,
     0,    65,     0,    92,   132,     0,    66 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	1	/* allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"EOLN",	257,
	"RP",	258,
	"LP",	259,
	"RBC",	260,
	"LBC",	261,
	"RBK",	262,
	"LBK",	263,
	"COLON",	264,
	",",	44,
	"*",	42,
	"SEMI",	265,
	"MARKER",	266,
	"LPSLSH",	267,
	"SLSHRP",	268,
	"DIM_MARKER",	269,
	"INT",	270,
	"DIMNUM",	271,
	"REAL",	272,
	"STRING",	273,
	"DIM",	274,
	"DIMNAME",	275,
	"ATTNAME",	276,
	"COORD",	277,
	"FVAR",	278,
	"INTEGER",	279,
	"FLOAT",	280,
	"LONG",	281,
	"DOUBLE",	282,
	"BYTE",	283,
	"CHARACTER",	284,
	"GRAPHIC",	285,
	"STRNG",	286,
	"NUMERIC",	287,
	"FILETYPE",	288,
	"SHORT",	289,
	"LOGICAL",	290,
	"UNDEF",	291,
	"VAR",	292,
	"WHILE",	293,
	"DO",	294,
	"QUIT",	295,
	"PROC",	296,
	"EPROC",	297,
	"NPROC",	298,
	"IPROC",	299,
	"UNDEFFILEVAR",	300,
	"BREAK",	301,
	"NOPARENT",	302,
	"BGIN",	303,
	"END",	304,
	"FUNC",	305,
	"EFUNC",	306,
	"NFUNC",	307,
	"IFUNC",	308,
	"FDIM",	309,
	"IF",	310,
	"THEN",	311,
	"VBLKNAME",	312,
	"FILEVAR",	313,
	"CONTINUE",	314,
	"DFILE",	315,
	"KEYFUNC",	316,
	"KEYPROC",	317,
	"ELSE",	318,
	"EXTERNAL",	319,
	"RETURN",	320,
	"VSBLKGET",	321,
	"LOAD",	322,
	"NEW",	323,
	"OBJVAR",	324,
	"OBJTYPE",	325,
	"RECORD",	326,
	"VSBLKCREATE",	327,
	"VSBLKSET",	328,
	"LOCAL",	329,
	"STOP",	330,
	"=",	61,
	"OR",	331,
	"XOR",	332,
	"AND",	333,
	"GT",	334,
	"GE",	335,
	"LT",	336,
	"LE",	337,
	"EQ",	338,
	"NE",	339,
	"<",	60,
	">",	62,
	"+",	43,
	"-",	45,
	"#",	35,
	"/",	47,
	"%",	37,
	"^",	94,
	"UNOP",	340,
	"NOT",	341,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"statement_list : statement eoln",
	"statement_list : statement_list statement eoln",
	"statement_list : statement_list RECORD STRING eoln",
	"statement_list : RECORD STRING eoln",
	"statement_list : LOAD STRING eoln",
	"statement_list : statement_list LOAD STRING eoln",
	"block_statement_list : statement eoln",
	"block_statement_list : block_statement_list statement eoln",
	"block_statement_list : block_statement_list RECORD STRING eoln",
	"block_statement_list : RECORD STRING eoln",
	"block_statement_list : LOAD STRING eoln",
	"block_statement_list : block_statement_list LOAD STRING eoln",
	"opt_eoln : /* empty */",
	"opt_eoln : eoln",
	"eoln : EOLN",
	"statement : /* empty */",
	"statement : assignment",
	"statement : procedure",
	"statement : function_def",
	"statement : procedure_def",
	"statement : block",
	"statement : do",
	"statement : conditional",
	"statement : break_cont",
	"statement : visblk",
	"statement : RETURN expr",
	"statement : QUIT",
	"statement : error",
	"statement : STOP RECORD",
	"break_cont : BREAK",
	"break_cont : CONTINUE",
	"conditional : IF expr then block_statement_list END IF",
	"conditional : IF expr then block_statement_list ELSE block_statement_list END IF",
	"conditional : IF expr then statement END IF",
	"conditional : IF expr then statement ELSE block_statement_list END IF",
	"conditional : IF expr then statement ELSE statement END IF",
	"conditional : IF expr then block_statement_list ELSE statement END IF",
	"then : /* empty */",
	"then : THEN",
	"visblk : vset",
	"visblk : vget",
	"v_parent : NOPARENT",
	"v_parent : identifier",
	"vcreate : VSBLKCREATE expr OBJTYPE v_parent resource_list END VSBLKCREATE",
	"vcreate : VSBLKCREATE expr OBJTYPE v_parent resource END VSBLKCREATE",
	"vcreate : VSBLKCREATE error",
	"vset : VSBLKSET identifier resource END VSBLKSET",
	"vset : VSBLKSET identifier resource_list END VSBLKSET",
	"vset : VSBLKSET error",
	"vget : VSBLKGET identifier get_resource END VSBLKGET",
	"vget : VSBLKGET identifier get_resource_list END VSBLKGET",
	"vget : VSBLKGET error",
	"get_resource_list : get_resource eoln",
	"get_resource_list : get_resource_list get_resource eoln",
	"get_resource : /* empty */",
	"get_resource : STRING COLON UNDEF",
	"get_resource : error",
	"resource_list : resource eoln",
	"resource_list : resource_list resource eoln",
	"resource_list : resource_list error eoln",
	"resource : /* empty */",
	"resource : STRING COLON expr",
	"do_stmnt : block_statement_list",
	"do_stmnt : statement",
	"do : DO identifier '=' expr ',' expr do_stmnt END DO",
	"do : DO identifier '=' expr ',' expr ',' expr do_stmnt END DO",
	"do : DO WHILE expr block_statement_list END DO",
	"do : DO WHILE expr statement END DO",
	"block : BGIN block_statement_list END",
	"block : BGIN statement END",
	"procedure : IPROC opt_arg_list",
	"procedure : PROC opt_arg_list",
	"procedure : EPROC opt_arg_list",
	"procedure : NPROC opt_arg_list",
	"procedure : IPROC",
	"procedure : PROC",
	"procedure : EPROC",
	"procedure : NPROC",
	"procedure : FUNC opt_arg_list",
	"opt_arg_list : LP arg_list RP",
	"opt_arg_list : LP RP",
	"arg_list : expr",
	"arg_list : arg_list ',' expr",
	"func_identifier : KEYFUNC UNDEF",
	"local_list : vname",
	"local_list : pfname",
	"local_list : local_list ',' vname",
	"local_list : local_list ',' pfname",
	"function_def : func_identifier LP arg_dec_list RP opt_eoln",
	"function_def : func_identifier LP arg_dec_list RP opt_eoln block",
	"function_def : func_identifier LP arg_dec_list RP opt_eoln LOCAL local_list opt_eoln",
	"function_def : func_identifier LP arg_dec_list RP opt_eoln LOCAL local_list opt_eoln block",
	"function_def : EXTERNAL func_identifier LP arg_dec_list RP opt_eoln STRING",
	"function_def : func_identifier error",
	"arg_dec_list : /* empty */",
	"arg_dec_list : the_list",
	"the_list : declaration",
	"the_list : declaration ',' the_list",
	"declaration : vname",
	"declaration : vname COLON datatype",
	"declaration : vname dim_size_list",
	"declaration : vname dim_size_list COLON datatype",
	"declaration : pfname",
	"declaration : pfname COLON datatype",
	"declaration : pfname dim_size_list",
	"declaration : pfname dim_size_list COLON datatype",
	"pfname : IFUNC",
	"pfname : NFUNC",
	"pfname : EFUNC",
	"pfname : FUNC",
	"pfname : NPROC",
	"pfname : EPROC",
	"pfname : PROC",
	"datatype : FLOAT",
	"datatype : LONG",
	"datatype : INTEGER",
	"datatype : SHORT",
	"datatype : DOUBLE",
	"datatype : CHARACTER",
	"datatype : BYTE",
	"datatype : FILETYPE",
	"datatype : NUMERIC",
	"datatype : GRAPHIC",
	"datatype : STRNG",
	"datatype : LOGICAL",
	"dim_size_list : LBK INT RBK",
	"dim_size_list : LBK '*' RBK",
	"dim_size_list : dim_size_list LBK INT RBK",
	"dim_size_list : dim_size_list LBK '*' RBK",
	"proc_identifier : KEYPROC UNDEF",
	"procedure_def : proc_identifier LP arg_dec_list RP opt_eoln LOCAL local_list opt_eoln",
	"procedure_def : proc_identifier LP arg_dec_list RP opt_eoln LOCAL local_list opt_eoln block",
	"procedure_def : proc_identifier LP arg_dec_list RP opt_eoln",
	"procedure_def : proc_identifier LP arg_dec_list RP opt_eoln block",
	"procedure_def : EXTERNAL proc_identifier LP arg_dec_list RP opt_eoln STRING",
	"procedure_def : proc_identifier error",
	"assignment : identifier '=' expr",
	"identifier : vname",
	"identifier : vname FVAR",
	"identifier : vname FVAR MARKER",
	"identifier : vname FVAR LP subscript_list RP MARKER",
	"identifier : vname FVAR LP subscript_list RP",
	"identifier : vname FVAR DIM_MARKER primary",
	"identifier : vname FVAR ATTNAME",
	"identifier : vname FVAR ATTNAME LP subscript_list RP",
	"identifier : vname FVAR COORD",
	"identifier : vname FVAR COORD ATTNAME",
	"identifier : vname FVAR COORD LP subscript_list RP",
	"identifier : vname DIM_MARKER primary",
	"identifier : vname ATTNAME",
	"identifier : vname ATTNAME LP subscript_list RP",
	"identifier : vname MARKER",
	"identifier : vname LP subscript_list RP MARKER",
	"identifier : vname LP subscript_list RP",
	"identifier : vname COORD",
	"identifier : vname COORD LP subscript_list RP",
	"identifier : vname COORD ATTNAME",
	"vname : OBJVAR",
	"vname : OBJTYPE",
	"vname : VAR",
	"vname : UNDEF",
	"vname : DFILE",
	"subscript_list : subscript0",
	"subscript_list : LBC subscript1 RBC",
	"subscript_list : subscript_list ',' subscript0",
	"subscript_list : subscript_list ',' LBC subscript1 RBC",
	"subscript0 : subexpr",
	"subscript0 : DIM subexpr",
	"subscript1 : subexpr",
	"subscript1 : DIM subexpr",
	"subexpr : expr",
	"subexpr : COLON",
	"subexpr : expr COLON expr",
	"subexpr : COLON expr",
	"subexpr : expr COLON",
	"subexpr : expr COLON expr COLON",
	"subexpr : COLON expr COLON",
	"subexpr : expr COLON COLON",
	"subexpr : expr COLON expr COLON expr",
	"subexpr : expr COLON COLON expr",
	"subexpr : COLON expr COLON expr",
	"subexpr : COLON COLON",
	"subexpr : COLON COLON expr",
	"expr : primary",
	"expr : '-' expr",
	"expr : NOT expr",
	"expr : expr '%' expr",
	"expr : expr OR expr",
	"expr : expr AND expr",
	"expr : expr XOR expr",
	"expr : expr '<' expr",
	"expr : expr '>' expr",
	"expr : expr '+' expr",
	"expr : expr '-' expr",
	"expr : expr '*' expr",
	"expr : expr '#' expr",
	"expr : expr '/' expr",
	"expr : expr '^' expr",
	"expr : expr LE expr",
	"expr : expr GE expr",
	"expr : expr GT expr",
	"expr : expr LT expr",
	"expr : expr EQ expr",
	"expr : expr NE expr",
	"primary : REAL",
	"primary : INT",
	"primary : STRING",
	"primary : function",
	"primary : identifier",
	"primary : array",
	"primary : vcreate",
	"primary : LP expr RP",
	"primary : NEW LP expr ',' datatype ',' expr RP",
	"primary : NEW LP expr ',' datatype RP",
	"function : FUNC opt_arg_list",
	"function : IFUNC opt_arg_list",
	"function : EFUNC opt_arg_list",
	"function : NFUNC opt_arg_list",
	"function : IFUNC",
	"function : FUNC",
	"function : EFUNC",
	"function : NFUNC",
	"array : LPSLSH expr_list SLSHRP",
	"expr_list : expr",
	"expr_list : expr ',' expr_list",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 116 "ncl.y"
{	
								int strt;

								if((yypvt[-1].src_node != NULL)&&!(is_error)) {
									_NclPrintTree(yypvt[-1].src_node,thefptr);
									strt = _NclTranslate(yypvt[-1].src_node,thefptr);
									_NclTransTerminate();
									_NclPrintMachine(strt,-1,theoptr);
									_NclExecute(strt);
									_NclResetNewSymStack();
									_NclFreeTree();
#ifdef MAKEAPI
									return(0);
#endif
								} else {
									_NclDeleteNewSymStack();
									_NclFreeTree();
									is_error = 0;
#ifdef MAKEAPI
									return(1);
#endif
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
							} break;
case 2:
# line 142 "ncl.y"
{		
								int strt;

								if((yypvt[-1].src_node != NULL) && !(is_error)) {
									_NclPrintTree(yypvt[-1].src_node,thefptr);
									strt = _NclTranslate(yypvt[-1].src_node,thefptr);
									_NclTransTerminate();
									_NclPrintMachine(strt,-1,theoptr);
									_NclExecute(strt);
									_NclResetNewSymStack();
									_NclFreeTree();
#ifdef MAKEAPI
									return(0);
#endif 
								} else {
									_NclDeleteNewSymStack();
									_NclFreeTree();
									is_error = 0;
#ifdef MAKEAPI
									return(1);
#endif
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
							} break;
case 3:
# line 167 "ncl.y"
{ 
/*
* These record statments have to occur here so that the record command isn't written out
* by the scanner. The scanner writes each line when an EOLN is scanned.
*/
								recfp = fopen(_NhlResolvePath(yypvt[-1].str),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(NhlWARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
#ifdef MAKEAPI
									return(0);
#endif 
							} break;
case 4:
# line 185 "ncl.y"
{ 
								recfp = fopen(_NhlResolvePath(yypvt[-1].str),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(NhlWARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
#ifdef MAKEAPI
									return(0);
#endif 
							} break;
case 5:
# line 205 "ncl.y"
{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath(yypvt[-1].str),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number + 1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen(yypvt[-1].str)+1);
										strcpy(cur_load_file,yypvt[-1].str);
									} else {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not open %s",yypvt[-1].str);
										loading = 0;
									}
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
							} break;
case 6:
# line 231 "ncl.y"
{
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath(yypvt[-1].str),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number + 1;
										cur_line_number = 0;
										yyin = tmp_file;
										loading = 1;
										cur_load_file = (char*)NclMalloc(strlen((char*)yypvt[-1].str)+1);
										cmd_line = isatty(fileno(tmp_file));
										strcpy(cur_load_file,yypvt[-1].str);
									} else {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not open %s",yypvt[-1].str);
										loading = 0;
									}
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
#endif
							} break;
case 7:
# line 259 "ncl.y"
{ 	
								
								if(cmd_line) {
									if(is_error) {
										_NclDeleteNewSymStack();	
										is_error = 0;
									} else {
										_NclResetNewSymStack();
									}
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
								if(yypvt[-1].src_node != NULL) {
									yyval.list = _NclMakeNewListNode();
									yyval.list->next = NULL;
									yyval.list->node = yypvt[-1].src_node;
/*
									$$->next = _NclMakeNewListNode();
									$$->next->node = $2;
									$$->next->next = NULL;
*/
								} else {
									yyval.list = NULL;
								}
							} break;
case 8:
# line 283 "ncl.y"
{	
/*
* This looping is necessary because ordering needs to be maintained for statement_lists
*/
								NclSrcListNode *step;
								if(cmd_line){
									if(is_error) {
										_NclDeleteNewSymStack();	
										is_error = 0;
									} else {
										_NclResetNewSymStack();
									}
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
								if(yypvt[-2].list == NULL) {
									if(yypvt[-1].src_node != NULL) {
										yyval.list = _NclMakeNewListNode();
										yyval.list->next = NULL;
										yyval.list->node = yypvt[-1].src_node;
/*
										$$->next = _NclMakeNewListNode();
										$$->next->node = $3;
										$$->next->next = NULL;
*/
									} else if(yypvt[-1].src_node == NULL) {
										yyval.list = NULL;
									}
								} else if(yypvt[-1].src_node != NULL){
									step = yypvt[-2].list;
									while(step->next != NULL) {
										step = step->next;
									}
									step->next = _NclMakeNewListNode();
									step= step->next;
/*
									step->next = _NclMakeNewListNode();
									step->next->node = $3;
									step->next->next = NULL;
*/
									step->next = NULL;
									step->node = yypvt[-1].src_node;
									yyval.list = yypvt[-2].list;
								} else {
									yyval.list = yypvt[-2].list;
								}
							} break;
case 9:
# line 329 "ncl.y"
{ 
								recfp = fopen(_NhlResolvePath(yypvt[-1].str),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(NhlWARNING,errno,"Could not open record file");
									rec = 0;
								}
								yyval.list = yypvt[-3].list;
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
							} break;
case 10:
# line 341 "ncl.y"
{ 
								recfp = fopen(_NhlResolvePath(yypvt[-1].str),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(NhlWARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
								yyval.list = NULL;
							} break;
case 11:
# line 358 "ncl.y"
{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath(yypvt[-1].str),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number +1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen(yypvt[-1].str)+1);
										strcpy(cur_load_file,yypvt[-1].str);
									} else {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not open %s",yypvt[-1].str);
										loading = 0;
									}
								}
#endif
								yyval.list = NULL;
							} break;
case 12:
# line 383 "ncl.y"
{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath(yypvt[-1].str),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number +1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen((char*)yypvt[-1].str)+1);
										strcpy(cur_load_file,yypvt[-1].str);
									} else {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not open %s",yypvt[-1].str);
										loading = 0;
									}
								}
#endif
								yyval.list = yypvt[-3].list;
							} break;
case 13:
# line 410 "ncl.y"
{ /* do nothing */ } break;
case 14:
# line 411 "ncl.y"
{ 
				yyerrok; 
				if(cmd_line)
                                      fprintf(stdout,"ncl %d> ",cur_line_number);
			} break;
case 15:
# line 418 "ncl.y"
{ yyerrok; } break;
case 16:
# line 420 "ncl.y"
{ yyval.src_node = NULL; } break;
case 17:
# line 421 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node; 
							} break;
case 18:
# line 424 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 19:
# line 427 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 20:
# line 430 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 21:
# line 433 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 22:
# line 436 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 23:
# line 439 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 24:
# line 442 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 25:
# line 445 "ncl.y"
{
								yyval.src_node = yypvt[-0].src_node;
							} break;
case 26:
# line 448 "ncl.y"
{
								yyval.src_node = _NclMakeReturn(yypvt[-0].src_node); 
							} break;
case 27:
# line 451 "ncl.y"
{ 
								return(1);
							} break;
case 28:
# line 454 "ncl.y"
{ 
								yyval.src_node = NULL ; 
								ERROR("error in statement"); 
							} break;
case 29:
# line 458 "ncl.y"
{
/*
* this goes here so that rec gets set to one before eoln comes from scanner.
*/
								if(rec ==1 ) {
									fclose(recfp);
								} 
								yyval.src_node = NULL;
							} break;
case 30:
# line 469 "ncl.y"
{
				yyval.src_node = _NclMakeBreakCont(yypvt[-0].sym);
			} break;
case 31:
# line 472 "ncl.y"
{
				yyval.src_node = _NclMakeBreakCont(yypvt[-0].sym);
		} break;
case 32:
# line 476 "ncl.y"
{  yyval.src_node = _NclMakeIfThen(yypvt[-4].src_node,yypvt[-2].list);  } break;
case 33:
# line 477 "ncl.y"
{  yyval.src_node = _NclMakeIfThenElse(yypvt[-6].src_node,yypvt[-4].list,yypvt[-2].list);  } break;
case 34:
# line 478 "ncl.y"
{  
											NclSrcListNode *tmp = NULL;	
											if(yypvt[-2].src_node != NULL) {
												tmp = _NclMakeNewListNode();
												tmp->next = NULL;
												tmp->node = yypvt[-2].src_node;
											} 
											yyval.src_node = _NclMakeIfThen(yypvt[-4].src_node,tmp);  
										} break;
case 35:
# line 487 "ncl.y"
{  
											NclSrcListNode *tmp = NULL;
	
											if(yypvt[-4].src_node != NULL) {
                                                                                                tmp = _NclMakeNewListNode();
                                                                                                tmp->next = NULL;
                                                                                                tmp->node = yypvt[-4].src_node;
											} 
											yyval.src_node = _NclMakeIfThenElse(yypvt[-6].src_node,tmp,yypvt[-2].list);  
										} break;
case 36:
# line 499 "ncl.y"
{  
										NclSrcListNode *tmp = NULL ,*tmp1 = NULL ;
										if(yypvt[-4].src_node != NULL) {
                                                                                        tmp = _NclMakeNewListNode();
                                                                                        tmp->next = NULL;
                                                                                        tmp->node = yypvt[-4].src_node;
										}
										if(yypvt[-2].src_node != NULL) {
                                                                                        tmp1 = _NclMakeNewListNode();
                                                                                        tmp1->next = NULL;
                                                                                        tmp1->node = yypvt[-2].src_node;
										}		
										yyval.src_node = _NclMakeIfThenElse(yypvt[-6].src_node,tmp,tmp1);  
								} break;
case 37:
# line 513 "ncl.y"
{  
											NclSrcListNode *tmp = NULL ;
											if(yypvt[-2].src_node != NULL) {
                                                                                        	tmp = _NclMakeNewListNode();
                                                                                        	tmp->next = NULL;
                                                                                        	tmp->node = yypvt[-2].src_node;
	                                                                                } 
											yyval.src_node = _NclMakeIfThenElse(yypvt[-6].src_node,yypvt[-4].list,tmp);  

										} break;
case 40:
# line 530 "ncl.y"
{
				yyval.src_node = yypvt[-0].list;
			} break;
case 41:
# line 533 "ncl.y"
{
				yyval.src_node = yypvt[-0].list;
			} break;
case 42:
# line 538 "ncl.y"
{
			yyval.src_node = NULL;
		   } break;
case 43:
# line 541 "ncl.y"
{
			yyval.src_node = yypvt[-0].src_node;
		     } break;
case 44:
# line 547 "ncl.y"
{   
									yyval.src_node = _NclMakeVis(yypvt[-5].src_node,yypvt[-4].sym,yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKCREATE);
								} break;
case 45:
# line 550 "ncl.y"
{   
									yyval.src_node = _NclMakeVis(yypvt[-5].src_node,yypvt[-4].sym,yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKCREATE); 
								} break;
case 46:
# line 553 "ncl.y"
{
										yyval.src_node = NULL;
								} break;
case 47:
# line 558 "ncl.y"
{
									yyval.list = _NclMakeSGVis(yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKSET); 
								} break;
case 48:
# line 561 "ncl.y"
{
									yyval.list = _NclMakeSGVis(yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKSET);
								} break;
case 49:
# line 564 "ncl.y"
{
										yyval.list = NULL;
								} break;
case 50:
# line 568 "ncl.y"
{
									yyval.list = _NclMakeSGVis(yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKGET); 
								} break;
case 51:
# line 571 "ncl.y"
{
									yyval.list = _NclMakeSGVis(yypvt[-3].src_node,yypvt[-2].list,Ncl_VISBLKGET);
								} break;
case 52:
# line 574 "ncl.y"
{
										yyval.list = NULL;
								} break;
case 53:
# line 580 "ncl.y"
{
							if(yypvt[-1].list != NULL) {
						 		yyval.list = _NclMakeNewListNode();
								yyval.list->next = NULL;
								yyval.list->node = yypvt[-1].list;
							} else {
								yyval.list = NULL;
							}
							if(cmd_line) {
								fprintf(stdout,"ncl %d> ",cur_line_number);
							}
						} break;
case 54:
# line 592 "ncl.y"
{
							if(yypvt[-2].list == NULL) {
								if(yypvt[-1].list != NULL) {
									yyval.list = _NclMakeNewListNode();
									yyval.list->next = NULL;
								 	yyval.list->node = yypvt[-1].list;
								} else {
									yyval.list = NULL;
								}
							} else if(yypvt[-1].list != NULL) {
								yyval.list = _NclMakeNewListNode();
								yyval.list->next = yypvt[-2].list;
								yyval.list->node = yypvt[-1].list;
							} else {
								yyval.list = yypvt[-2].list;
							}
							if(cmd_line) {
								fprintf(stdout,"ncl %d> ",cur_line_number);
							}
						} break;
case 55:
# line 614 "ncl.y"
{
							yyval.list = NULL;
						} break;
case 56:
# line 617 "ncl.y"
{
						 	yyval.list = _NclMakeGetResource(yypvt[-2].str,yypvt[-0].sym);
/*
							if(cmd_line)
								if(!VerifyGetResExpr($3)) {
									$$ = NULL;
								}
*/
						} break;
case 57:
# line 631 "ncl.y"
{	
							yyval.list = NULL;
						} break;
case 58:
# line 637 "ncl.y"
{
							if(yypvt[-1].list != NULL) {
						 		yyval.list = _NclMakeNewListNode();
								yyval.list->next = NULL;
								yyval.list->node = yypvt[-1].list;
							} else {
								yyval.list = NULL;
							}
							if(cmd_line) {
								fprintf(stdout,"ncl %d> ",cur_line_number);
							}
						} break;
case 59:
# line 650 "ncl.y"
{
							if(yypvt[-2].list == NULL) {
								if(yypvt[-1].list != NULL) {
									yyval.list = _NclMakeNewListNode();
									yyval.list->next = NULL;
								 	yyval.list->node = yypvt[-1].list;
								} else {
									yyval.list = NULL;
								}
							} else if(yypvt[-1].list != NULL) {
								yyval.list = _NclMakeNewListNode();
								yyval.list->next = yypvt[-2].list;
								yyval.list->node = yypvt[-1].list;
							} else {
								yyval.list = yypvt[-2].list;
								if(is_error) {
/*
									_NclDeleteNewSymStack();		
*/
									is_error = 0;
								}
						
							}
							if(cmd_line) {
								fprintf(stdout,"ncl %d> ",cur_line_number);
							}
						} break;
case 60:
# line 677 "ncl.y"
{
							yyval.list = yypvt[-2].list;
							is_error -= 1;
/*
							_NclDeleteNewSymStack();	
*/
							if(cmd_line) {

								fprintf(stdout,"ncl %d> ",cur_line_number);
							}
						} break;
case 61:
# line 690 "ncl.y"
{
							yyval.list = NULL;
						} break;
case 62:
# line 693 "ncl.y"
{
						 	yyval.list = _NclMakeResource(yypvt[-2].str,yypvt[-0].src_node);
/*
							if(cmd_line)
								if(!VerifySetResExpr($3)) {
									$$ = NULL;
								}
*/
						} break;
case 63:
# line 724 "ncl.y"
{
										yyval.list = yypvt[-0].list;
									} break;
case 64:
# line 727 "ncl.y"
{
										NclSrcListNode * tmp = NULL;
										if(yypvt[-0].src_node != NULL ) {
											tmp = _NclMakeNewListNode();
											tmp->next = NULL;
											tmp->node = yypvt[-0].src_node;
										}
										yyval.list = tmp;
									} break;
case 65:
# line 738 "ncl.y"
{ 
										
										yyval.src_node = _NclMakeDoFromTo(yypvt[-7].src_node,yypvt[-5].src_node, yypvt[-3].src_node, yypvt[-2].list);
									} break;
case 66:
# line 742 "ncl.y"
{ 
										yyval.src_node = _NclMakeDoFromToStride(yypvt[-9].src_node,yypvt[-7].src_node,yypvt[-5].src_node,yypvt[-3].src_node,yypvt[-2].list);
									} break;
case 67:
# line 745 "ncl.y"
{   
								yyval.src_node = _NclMakeWhile(yypvt[-3].src_node,yypvt[-2].list);
							} break;
case 68:
# line 748 "ncl.y"
{   
								NclSrcListNode *tmp = NULL ;
								if(yypvt[-2].src_node != NULL) {
                                                               		tmp = _NclMakeNewListNode();
                                                                       	tmp->next = NULL;
                                                                       	tmp->node = yypvt[-2].src_node;
	                                                        } 
								yyval.src_node = _NclMakeWhile(yypvt[-3].src_node,tmp);
							} break;
case 69:
# line 759 "ncl.y"
{ yyval.src_node = _NclMakeBlock(yypvt[-1].list); } break;
case 70:
# line 760 "ncl.y"
{ 
					NclSrcListNode *tmp = NULL ;
					if(yypvt[-1].src_node != NULL) {
                                       		tmp = _NclMakeNewListNode();
                                        	tmp->next = NULL;
                                               	tmp->node = yypvt[-1].src_node;
	                                } 
					yyval.src_node = _NclMakeBlock(tmp); 
				} break;
case 71:
# line 771 "ncl.y"
{
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: procedure %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeProcCall(yypvt[-1].sym,yypvt[-0].list,Ncl_INTRINSICPROCCALL); 
						}
				} break;
case 72:
# line 788 "ncl.y"
{ 
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: procedure %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeProcCall(yypvt[-1].sym,yypvt[-0].list,Ncl_BUILTINPROCCALL); 
						}
				} break;
case 73:
# line 805 "ncl.y"
{ yyval.src_node = _NclMakeProcCall(yypvt[-1].sym,yypvt[-0].list,Ncl_EXTERNALPROCCALL); } break;
case 74:
# line 806 "ncl.y"
{ yyval.src_node = _NclMakeProcCall(yypvt[-1].sym,yypvt[-0].list,Ncl_PROCCALL); } break;
case 75:
# line 807 "ncl.y"
{ 
					yyval.src_node = _NclMakeProcCall(yypvt[-0].sym,NULL,Ncl_INTRINSICPROCCALL); 
				} break;
case 76:
# line 810 "ncl.y"
{ 
					yyval.src_node = _NclMakeProcCall(yypvt[-0].sym,NULL,Ncl_BUILTINPROCCALL); 
				} break;
case 77:
# line 813 "ncl.y"
{ yyval.src_node = _NclMakeProcCall(yypvt[-0].sym,NULL,Ncl_EXTERNALPROCCALL); } break;
case 78:
# line 814 "ncl.y"
{ 
						if(yypvt[-0].sym->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: procedure %s expects %d arguments, got %d",yypvt[-0].sym->name,yypvt[-0].sym->u.procfunc->nargs,0);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeProcCall(yypvt[-0].sym,NULL,Ncl_PROCCALL); 
						}
				} break;
case 79:
# line 827 "ncl.y"
{ ERROR("syntax error: <identifier> IS A FUNCTION NOT A PROCEDURE"); } break;
case 80:
# line 830 "ncl.y"
{ yyval.list = yypvt[-1].list;    } break;
case 81:
# line 831 "ncl.y"
{ yyval.list = NULL;    } break;
case 82:
# line 834 "ncl.y"
{ 
						/* Code to check type of expression, iff its and identifier then stamp it with
							the Ncl_PARAMIT tag so the translator can add extra code */
							if(((NclGenericNode*)yypvt[-0].src_node)->kind == Ncl_IDNEXPR) {
								((NclGenericRefNode*)((NclIdnExpr*)yypvt[-0].src_node)->idn_ref_node)->ref_type =
									Ncl_PARAMIT;
							}
							yyval.list = _NclMakeNewListNode();
							yyval.list->next = NULL;
							yyval.list->node = yypvt[-0].src_node;
						} break;
case 83:
# line 845 "ncl.y"
{
							NclSrcListNode * step;
						/* 
						* ordering is important because arguments eventually must be pushed on stack in
						* appropriate order 
						*/
							step = yypvt[-2].list;
							while(step->next != NULL) {
								step = step->next;
							}
						/* Code to check type of expression, iff its and identifier then stamp it with
							the Ncl_PARAMIT tag so the translator can add extra code */
							if(((NclGenericNode*)yypvt[-0].src_node)->kind == Ncl_IDNEXPR) {
								((NclGenericRefNode*)((NclIdnExpr*)yypvt[-0].src_node)->idn_ref_node)->ref_type =
									Ncl_PARAMIT;
							}
							step->next = _NclMakeNewListNode();
							step->next->next = NULL;
							step->next->node = yypvt[-0].src_node;
							yyval.list = yypvt[-2].list;
						} break;
case 84:
# line 867 "ncl.y"
{ _NclNewScope(); yyval.sym = yypvt[-0].sym; } break;
case 85:
# line 870 "ncl.y"
{
			/* have to make sure that items in the local list are not added twice !! */
			int lv = _NclGetCurrentScopeLevel();

			if(yypvt[-0].sym->level != lv) {
				_NclAddSym(yypvt[-0].sym->name,UNDEF);
			}
		} break;
case 86:
# line 878 "ncl.y"
{
			int lv = _NclGetCurrentScopeLevel();
			if(yypvt[-0].sym->level != lv) {
				_NclAddSym(yypvt[-0].sym->name,UNDEF);
			}
		} break;
case 87:
# line 884 "ncl.y"
{
			int lv = _NclGetCurrentScopeLevel();
			if(yypvt[-0].sym->level != lv) {
				_NclAddSym(yypvt[-0].sym->name,UNDEF);
			}
			} break;
case 88:
# line 890 "ncl.y"
{
			int lv = _NclGetCurrentScopeLevel();
			if(yypvt[-0].sym->level != lv) {
				_NclAddSym(yypvt[-0].sym->name,UNDEF);
			}
			} break;
case 89:
# line 897 "ncl.y"
{_NclChangeSymbolType(yypvt[-4].sym,NFUNC);_NclAddProcFuncInfoToSym(yypvt[-4].sym,yypvt[-2].list); } break;
case 90:
# line 898 "ncl.y"
{  
									NclSymTableListNode *tmp;

									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										yyval.src_node = NULL;
									}else {
										tmp = _NclPopScope();	
										yyval.src_node = _NclMakeNFunctionDef(yypvt[-6].sym,yypvt[-4].list,yypvt[-0].src_node,tmp);  
									}
								} break;
case 91:
# line 910 "ncl.y"
{_NclChangeSymbolType(yypvt[-7].sym,NFUNC); _NclAddProcFuncInfoToSym(yypvt[-7].sym,yypvt[-5].list); } break;
case 92:
# line 911 "ncl.y"
{  
									NclSymTableListNode *tmp;

									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										yyval.src_node = NULL;
									}else {
										tmp = _NclPopScope();	
										yyval.src_node = _NclMakeNFunctionDef(yypvt[-9].sym,yypvt[-7].list,yypvt[-0].src_node,tmp);  
									}
								} break;
case 93:
# line 924 "ncl.y"
{  
									NclSymTableListNode *tmp;
									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										yyval.src_node = NULL;
									} else {
										tmp = _NclPopScope();	
										yyval.src_node = _NclMakeEFunctionDef(_NclChangeSymbolType(yypvt[-5].sym,EFUNC),yypvt[-3].list,yypvt[-0].str,tmp);  
									}
								} break;
case 94:
# line 937 "ncl.y"
{
			is_error += 1;
/*
* Need to call this before new scope is poped so symbols can be found and freed
*/
			_NclDeleteNewSymStack();
/*
* Need to call function to free scope
*/
			(void)_NclPopScope();
	} break;
case 95:
# line 955 "ncl.y"
{ yyval.list = NULL; } break;
case 96:
# line 956 "ncl.y"
{ yyval.list = yypvt[-0].list; } break;
case 97:
# line 959 "ncl.y"
{	
							yyval.list = _NclMakeNewListNode();
							yyval.list->next = NULL;
							yyval.list->node = yypvt[-0].src_node;
						} break;
case 98:
# line 964 "ncl.y"
{ 
						/* once again ordering not important as long as it is consistent with function 
							and procedure ordering of argument lists */
							yyval.list = _NclMakeNewListNode();
							yyval.list->next = yypvt[-0].list;
							yyval.list->node = yypvt[-2].src_node;
							  
						} break;
case 99:
# line 974 "ncl.y"
{ 
					NclSymbol *s;
					int lv = _NclGetCurrentScopeLevel();

					if((yypvt[-0].sym->type != UNDEF)||(yypvt[-0].sym->level != lv)) {
						s = _NclAddSym(yypvt[-0].sym->name,UNDEF);
					} else {
						s = yypvt[-0].sym;
					}
					yyval.src_node = _NclMakeLocalVarDec(s,NULL,NULL); 
				} break;
case 100:
# line 985 "ncl.y"
{ 
					NclSymbol *s;
					int lv = _NclGetCurrentScopeLevel();

					if((yypvt[-2].sym->type != UNDEF)||(yypvt[-2].sym->level != lv)) {
						s = _NclAddSym(yypvt[-2].sym->name,UNDEF);
					} else {
						s = yypvt[-2].sym;
					}
					yyval.src_node = _NclMakeLocalVarDec(s,NULL,yypvt[-0].sym); 
				} break;
case 101:
# line 996 "ncl.y"
{ 
						NclSymbol *s;
						int lv = _NclGetCurrentScopeLevel();
						if((yypvt[-1].sym->type != UNDEF)||(yypvt[-1].sym->level != lv)) {
							s = _NclAddSym(yypvt[-1].sym->name,UNDEF);
						} else {
							s = yypvt[-1].sym;
						}

						yyval.src_node = _NclMakeLocalVarDec(s,yypvt[-0].list,NULL); 
					} break;
case 102:
# line 1007 "ncl.y"
{ 
						NclSymbol *s;
						int lv = _NclGetCurrentScopeLevel();
						if((yypvt[-3].sym->type != UNDEF)||(yypvt[-3].sym->level != lv)) {
							s = _NclAddSym(yypvt[-3].sym->name,UNDEF);
						} else {
							s = yypvt[-3].sym;
						}

						yyval.src_node = _NclMakeLocalVarDec(s,yypvt[-2].list,yypvt[-0].sym); 
					} break;
case 103:
# line 1018 "ncl.y"
{ 
				/* Need to intercept defined names and add them to current scope */
					NclSymbol *s;

					s = _NclAddSym(yypvt[-0].sym->name,UNDEF);
					yyval.src_node = _NclMakeLocalVarDec(s,NULL,NULL); 
				} break;
case 104:
# line 1025 "ncl.y"
{ 
					NclSymbol *s;

					s= _NclAddSym(yypvt[-2].sym->name,UNDEF);
					yyval.src_node = _NclMakeLocalVarDec(s,NULL,yypvt[-0].sym); 
				} break;
case 105:
# line 1031 "ncl.y"
{ 
					NclSymbol *s;

					s = _NclAddSym(yypvt[-1].sym->name,UNDEF);
					yyval.src_node = _NclMakeLocalVarDec(s,yypvt[-0].list,NULL); 
				} break;
case 106:
# line 1037 "ncl.y"
{ 
					NclSymbol *s;

					s = _NclAddSym(yypvt[-3].sym->name,UNDEF);
					yyval.src_node = _NclMakeLocalVarDec(s,yypvt[-2].list,yypvt[-0].sym); 
				} break;
case 107:
# line 1045 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 108:
# line 1048 "ncl.y"
{		
				yyval.sym = yypvt[-0].sym;
			} break;
case 109:
# line 1051 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 110:
# line 1054 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 111:
# line 1057 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 112:
# line 1060 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 113:
# line 1063 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 114:
# line 1068 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 115:
# line 1069 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 116:
# line 1070 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 117:
# line 1071 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 118:
# line 1072 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 119:
# line 1073 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 120:
# line 1074 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 121:
# line 1075 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 122:
# line 1076 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 123:
# line 1077 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 124:
# line 1078 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 125:
# line 1079 "ncl.y"
{ yyval.sym = yypvt[-0].sym; } break;
case 126:
# line 1082 "ncl.y"
{ 
					/* Dimension size list must be in order */
						yyval.list = _NclMakeNewListNode();
						yyval.list->next = NULL;
						yyval.list->node = _NclMakeDimSizeNode(yypvt[-1].integer);
						 
					} break;
case 127:
# line 1089 "ncl.y"
{
						yyval.list = _NclMakeNewListNode();
						yyval.list->next = NULL;
						yyval.list->node = _NclMakeDimSizeNode(-1);
						 
					} break;
case 128:
# line 1095 "ncl.y"
{   	
						NclSrcListNode *step;
						
						step = yypvt[-3].list;
						while(step->next != NULL) 
							step = step->next;
						step->next = _NclMakeNewListNode();
						step->next->next = NULL;
						step->next->node = _NclMakeDimSizeNode(yypvt[-1].integer);
						
					} break;
case 129:
# line 1106 "ncl.y"
{   
						NclSrcListNode *step;
                                                
                                                step = yypvt[-3].list;
                                                while(step->next != NULL) 
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = _NclMakeDimSizeNode(-1);
						
					} break;
case 130:
# line 1119 "ncl.y"
{ _NclNewScope(); yyval.sym = yypvt[-0].sym; } break;
case 131:
# line 1121 "ncl.y"
{_NclChangeSymbolType(yypvt[-7].sym,NPROC);_NclAddProcFuncInfoToSym(yypvt[-7].sym,yypvt[-5].list); } break;
case 132:
# line 1121 "ncl.y"
{
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
							
								yyval.src_node = _NclMakeProcDef(yypvt[-9].sym,yypvt[-7].list,yypvt[-0].src_node,tmp);
									
							} break;
case 133:
# line 1131 "ncl.y"
{_NclChangeSymbolType(yypvt[-4].sym,NPROC);_NclAddProcFuncInfoToSym(yypvt[-4].sym,yypvt[-2].list); } break;
case 134:
# line 1131 "ncl.y"
{
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
								yyval.src_node = _NclMakeProcDef(yypvt[-6].sym,yypvt[-4].list,yypvt[-0].src_node,tmp);
									
							} break;
case 135:
# line 1140 "ncl.y"
{
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
								yyval.src_node = _NclMakeExternalProcDef(_NclChangeSymbolType(yypvt[-5].sym,EPROC),yypvt[-3].list,yypvt[-0].str,tmp);
									
							} break;
case 136:
# line 1149 "ncl.y"
{
			is_error += 1;
/*
* Need to call this before new scope is poped so symbols can be found and freed
*/
			_NclDeleteNewSymStack();
/*
* Need to call function to free scope
*/
			(void)_NclPopScope();
	} break;
case 137:
# line 1162 "ncl.y"
{
						((NclGenericRefNode*)yypvt[-2].src_node)->ref_type = Ncl_WRITEIT;
						yyval.src_node = _NclMakeAssignment(yypvt[-2].src_node,yypvt[-0].src_node);
						  
					} break;
case 138:
# line 1174 "ncl.y"
{
			yyval.src_node = _NclMakeVarRef(yypvt[-0].sym,NULL);
		  } break;
case 139:
# line 1177 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarRef(yypvt[-1].sym,&((yypvt[-0].str)[2]),NULL,Ncl_FILEVAR);
					} break;
case 140:
# line 1180 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarRef(yypvt[-2].sym,&((yypvt[-1].str)[2]),NULL,Ncl_FILEVAR);
					} break;
case 141:
# line 1183 "ncl.y"
{
				
						yyval.src_node = _NclMakeFileVarRef(yypvt[-5].sym,&((yypvt[-4].str)[2]),yypvt[-2].list,Ncl_FILEVAR);
					} break;
case 142:
# line 1187 "ncl.y"
{	
				
						yyval.src_node = _NclMakeFileVarRef(yypvt[-4].sym,&((yypvt[-3].str)[2]),yypvt[-1].list,Ncl_FILEVAR);
					} break;
case 143:
# line 1191 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarDimRef(yypvt[-3].sym,&((yypvt[-2].str)[2]),yypvt[-0].src_node);		
					} break;
case 144:
# line 1194 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarAttRef(yypvt[-2].sym,&((yypvt[-1].str)[2]),yypvt[-0].str,NULL);
					} break;
case 145:
# line 1197 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarAttRef(yypvt[-5].sym,&((yypvt[-4].str)[2]),yypvt[-3].str,yypvt[-1].list);
					} break;
case 146:
# line 1200 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarCoordRef(yypvt[-2].sym,&((yypvt[-1].str)[2]),&((yypvt[-0].str)[1]),NULL);
					} break;
case 147:
# line 1203 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarCoordRef(yypvt[-3].sym,&((yypvt[-2].str)[2]),&((yypvt[-1].str)[1]),NULL);
					} break;
case 148:
# line 1206 "ncl.y"
{
						yyval.src_node = _NclMakeFileVarCoordRef(yypvt[-5].sym,&((yypvt[-4].str)[2]),&((yypvt[-3].str)[1]),yypvt[-1].list);
					} break;
case 149:
# line 1209 "ncl.y"
{
						yyval.src_node = _NclMakeVarDimRef(yypvt[-2].sym,yypvt[-0].src_node);		
					} break;
case 150:
# line 1212 "ncl.y"
{
						yyval.src_node = _NclMakeVarAttRef(yypvt[-1].sym,yypvt[-0].str,NULL);
					} break;
case 151:
# line 1215 "ncl.y"
{
						yyval.src_node = _NclMakeVarAttRef(yypvt[-4].sym,yypvt[-3].str,yypvt[-1].list);
					} break;
case 152:
# line 1218 "ncl.y"
{
						yyval.src_node = _NclMakeVarRef(yypvt[-1].sym,NULL);
					} break;
case 153:
# line 1221 "ncl.y"
{
						yyval.src_node = _NclMakeVarRef(yypvt[-4].sym,yypvt[-2].list);
					} break;
case 154:
# line 1224 "ncl.y"
{
						yyval.src_node = _NclMakeVarRef(yypvt[-3].sym,yypvt[-1].list);
					} break;
case 155:
# line 1227 "ncl.y"
{
						yyval.src_node = _NclMakeVarCoordRef(yypvt[-1].sym,&((yypvt[-0].str)[1]),NULL);
					} break;
case 156:
# line 1230 "ncl.y"
{
						yyval.src_node = _NclMakeVarCoordRef(yypvt[-4].sym,&((yypvt[-3].str)[1]),yypvt[-1].list);
					} break;
case 157:
# line 1233 "ncl.y"
{
						yyval.src_node = _NclMakeVarCoordRef(yypvt[-2].sym,&((yypvt[-1].str)[1]),NULL);
					} break;
case 158:
# line 1238 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 159:
# line 1241 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 160:
# line 1244 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 161:
# line 1247 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 162:
# line 1250 "ncl.y"
{
				yyval.sym = yypvt[-0].sym;
			} break;
case 163:
# line 1254 "ncl.y"
{
					/* ordering of subscripts must be preserved */
						yyval.list = _NclMakeNewListNode();
						yyval.list->next = NULL;
						yyval.list->node = yypvt[-0].src_node;
					} break;
case 164:
# line 1260 "ncl.y"
{
					/* ordering of subscripts must be preserved */
                                                yyval.list = _NclMakeNewListNode();
                                                yyval.list->next = NULL;
                                                yyval.list->node = yypvt[-1].src_node;
					} break;
case 165:
# line 1266 "ncl.y"
{
						NclSrcListNode *step;
                                                
                                                step = yypvt[-2].list;
                                                while(step->next != NULL) 
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = yypvt[-0].src_node;
						
					} break;
case 166:
# line 1277 "ncl.y"
{
						NclSrcListNode *step;
                                         
                                                step = yypvt[-4].list;
                                                while(step->next != NULL)
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = yypvt[-1].src_node;
                                                
					} break;
case 167:
# line 1290 "ncl.y"
{  
						yyval.src_node = _NclMakeIntSubscript(yypvt[-0].src_node,NULL);
						 
					} break;
case 168:
# line 1294 "ncl.y"
{ 
						yyval.src_node = _NclMakeIntSubscript(yypvt[-0].src_node,yypvt[-1].str);
						  
					} break;
case 169:
# line 1300 "ncl.y"
{  
						yyval.src_node = _NclMakeCoordSubscript(yypvt[-0].src_node,NULL);
						 
					} break;
case 170:
# line 1304 "ncl.y"
{ 
						yyval.src_node = _NclMakeCoordSubscript(yypvt[-0].src_node,yypvt[-1].str);
						  
					} break;
case 171:
# line 1311 "ncl.y"
{
						yyval.src_node = _NclMakeSingleIndex(yypvt[-0].src_node);
					} break;
case 172:
# line 1314 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(NULL,NULL,NULL);
					} break;
case 173:
# line 1317 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(yypvt[-2].src_node,yypvt[-0].src_node,NULL);
					} break;
case 174:
# line 1320 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(NULL,yypvt[-0].src_node,NULL);
					} break;
case 175:
# line 1323 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(yypvt[-1].src_node,NULL,NULL);
					} break;
case 176:
# line 1326 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(yypvt[-3].src_node,yypvt[-1].src_node,NULL);
					} break;
case 177:
# line 1329 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(NULL,yypvt[-1].src_node,NULL);
					} break;
case 178:
# line 1332 "ncl.y"
{
						yyval.src_node = _NclMakeRangeIndex(yypvt[-2].src_node,NULL,NULL);
					} break;
case 179:
# line 1335 "ncl.y"
{				
						yyval.src_node = _NclMakeRangeIndex(yypvt[-4].src_node,yypvt[-2].src_node,yypvt[-0].src_node);
					} break;
case 180:
# line 1338 "ncl.y"
{				
						yyval.src_node = _NclMakeRangeIndex(yypvt[-3].src_node,NULL,yypvt[-0].src_node);
					} break;
case 181:
# line 1341 "ncl.y"
{				
						yyval.src_node = _NclMakeRangeIndex(NULL,yypvt[-2].src_node,yypvt[-0].src_node);
					} break;
case 182:
# line 1344 "ncl.y"
{				
						yyval.src_node = _NclMakeRangeIndex(NULL,NULL,NULL);
					} break;
case 183:
# line 1347 "ncl.y"
{				
						yyval.src_node = _NclMakeRangeIndex(NULL,NULL,yypvt[-0].src_node);
					} break;
case 184:
# line 1351 "ncl.y"
{
						yyval.src_node = yypvt[-0].src_node;
					} break;
case 185:
# line 1354 "ncl.y"
{
						yyval.src_node = _NclMakeUnaryExpr(yypvt[-0].src_node,Ncl_NEGEXPR);
					} break;
case 186:
# line 1357 "ncl.y"
{
						yyval.src_node = _NclMakeUnaryExpr(yypvt[-0].src_node,Ncl_NOTEXPR);
					} break;
case 187:
# line 1360 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_MODEXPR);
					} break;
case 188:
# line 1363 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_OREXPR);
					} break;
case 189:
# line 1366 "ncl.y"
{			
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_ANDEXPR);
					} break;
case 190:
# line 1369 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_XOREXPR);
					} break;
case 191:
# line 1372 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_LTSELECTEXPR);
					} break;
case 192:
# line 1375 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_GTSELECTEXPR);
					} break;
case 193:
# line 1378 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_PLUSEXPR);
					} break;
case 194:
# line 1381 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_MINUSEXPR);
					} break;
case 195:
# line 1384 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_MULEXPR);
					} break;
case 196:
# line 1387 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_MATMULEXPR);
					} break;
case 197:
# line 1390 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_DIVEXPR);
					} break;
case 198:
# line 1393 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_EXPEXPR);
					} break;
case 199:
# line 1396 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_LEEXPR);
					} break;
case 200:
# line 1399 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_GEEXPR);
					} break;
case 201:
# line 1402 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_GTEXPR);
					} break;
case 202:
# line 1405 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_LTEXPR);
					} break;
case 203:
# line 1408 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_EQEXPR);
					} break;
case 204:
# line 1411 "ncl.y"
{
						yyval.src_node = _NclMakeExpr(yypvt[-2].src_node,yypvt[-0].src_node,Ncl_NEEXPR);
					} break;
case 205:
# line 1415 "ncl.y"
{
/*
* Note all of the structures created below the primary rule are special! They
* contain the ref_type field which is used to determine if the item
* is a parameter to a function or a procedure. The LP expr RP is an
* exception
*/
						yyval.src_node = _NclMakeIdnExpr(_NclMakeRealExpr(yypvt[-0].real));
					} break;
case 206:
# line 1424 "ncl.y"
{
						yyval.src_node = _NclMakeIdnExpr(_NclMakeIntExpr(yypvt[-0].integer));
					} break;
case 207:
# line 1427 "ncl.y"
{
						yyval.src_node = _NclMakeIdnExpr(_NclMakeStringExpr(yypvt[-0].str));
					} break;
case 208:
# line 1430 "ncl.y"
{	
						yyval.src_node = _NclMakeIdnExpr(yypvt[-0].src_node);
					} break;
case 209:
# line 1433 "ncl.y"
{
						yyval.src_node = _NclMakeIdnExpr(yypvt[-0].src_node);
					} break;
case 210:
# line 1436 "ncl.y"
{
						yyval.src_node = _NclMakeIdnExpr(yypvt[-0].src_node);
					} break;
case 211:
# line 1439 "ncl.y"
{
						yyval.src_node = yypvt[-0].src_node;
					} break;
case 212:
# line 1442 "ncl.y"
{ 
						yyval.src_node = yypvt[-1].src_node;
					} break;
case 213:
# line 1445 "ncl.y"
{
						yyval.src_node = _NclMakeNewOp(yypvt[-5].src_node,yypvt[-3].sym,yypvt[-1].src_node);
					} break;
case 214:
# line 1448 "ncl.y"
{
						yyval.src_node = _NclMakeNewOp(yypvt[-3].src_node,yypvt[-1].sym,NULL);
					} break;
case 215:
# line 1452 "ncl.y"
{	
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-1].sym,yypvt[-0].list,Ncl_BUILTINFUNCCALL);
						}
					} break;
case 216:
# line 1469 "ncl.y"
{
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-1].sym,yypvt[-0].list,Ncl_INTRINSICFUNCCALL);
						}
					} break;
case 217:
# line 1486 "ncl.y"
{
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-1].sym,yypvt[-0].list,Ncl_EXTERNFUNCCALL);
						}
					} break;
case 218:
# line 1503 "ncl.y"
{
						NclSrcListNode *step;
						int count = 0;
					
						step = yypvt[-0].list;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != yypvt[-1].sym->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-1].sym->name,yypvt[-1].sym->u.procfunc->nargs,count);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-1].sym,yypvt[-0].list,Ncl_FUNCCALL);
						}
					} break;
case 219:
# line 1520 "ncl.y"
{
						if(yypvt[-0].sym->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-0].sym->name,yypvt[-0].sym->u.procfunc->nargs,0);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-0].sym,NULL,Ncl_INTRINSICFUNCCALL);
						}
					} break;
case 220:
# line 1529 "ncl.y"
{
						if(yypvt[-0].sym->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-0].sym->name,yypvt[-0].sym->u.procfunc->nargs,0);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-0].sym,NULL,Ncl_BUILTINFUNCCALL);
						}
					} break;
case 221:
# line 1538 "ncl.y"
{
						if(yypvt[-0].sym->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-0].sym->name,yypvt[-0].sym->u.procfunc->nargs,0);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-0].sym,NULL,Ncl_EXTERNFUNCCALL);
						}
					} break;
case 222:
# line 1547 "ncl.y"
{
						if(yypvt[-0].sym->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"syntax error: function %s expects %d arguments, got %d",yypvt[-0].sym->name,yypvt[-0].sym->u.procfunc->nargs,0);
							yyval.src_node = NULL;
						} else {
							yyval.src_node = _NclMakeFuncCall(yypvt[-0].sym,NULL,Ncl_FUNCCALL);
						}
					} break;
case 223:
# line 1557 "ncl.y"
{ 
							yyval.src_node = _NclMakeArrayNode(yypvt[-1].array);
							 
					} break;
case 224:
# line 1563 "ncl.y"
{	
							yyval.array = _NclMakeRowList();
							yyval.array->list = _NclMakeNewListNode();
							yyval.array->list->next = NULL;
							yyval.array->list->node = yypvt[-0].src_node;
							yyval.array->currentitem= yyval.array->list;
							yyval.array->nelem = 1;
						} break;
case 225:
# line 1571 "ncl.y"
{ 
						/* pushed on backwards so they can be popped of in correct order*/
                                         
                                                	yypvt[-0].array->currentitem->next =  _NclMakeNewListNode();
							yypvt[-0].array->currentitem = yypvt[-0].array->currentitem->next;
                                                	yypvt[-0].array->currentitem->next = NULL;
                                                	yypvt[-0].array->currentitem->node = yypvt[-2].src_node;
							yypvt[-0].array->nelem++ ;
							yyval.array = yypvt[-0].array;
							 
						} break;
	}
	goto yystack;		/* reset registers in driver code */
}
