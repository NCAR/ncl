
typedef union  {
	int integer;
	double real;
	char  str[NCL_MAX_STRING];
	struct _NclSymbol *sym;
	void *src_node;
	struct src_node_list *list;
	struct ncl_rcl_list *array;
} YYSTYPE;
extern YYSTYPE ncllval;
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
