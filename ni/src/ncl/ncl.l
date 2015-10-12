%{
/*#include <stdio.h>*/
#include <string.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "parser.h"
#include "NclProf.h"
#ifdef DEBUGER
extern void printtoken(
#ifdef NhlNeedProto
	int /* type */,
	char* /* type */
#endif
);
#else
#define printtoken(n,m)
#endif
#define MAXTMPSPACE 512

extern short NCLecho;
extern short NCLoverrideEcho;

short   echo = 0;
short   load_stmt = 0,
        old_load_stmt = 0;
char    load_file[255];
char fbuf[256];

char *cur_line_text = NULL;
char *cur_line_text_pos = NULL;
int cur_line_length = 0;
int cur_line_number= -1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
int cur_line_maxsize = 0;
extern int loading;
extern int preloading;
#include <ctype.h>
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
extern int cmd_line;
extern int cmd_line_is_set;
extern char *cur_load_file;
extern char *ncl_cur_func;
int inc_line = 1;

#define PRINTLOCATION(errlev)	\
	if (loading > 0) \
		NhlPError(errlev,NhlEUNKNOWN,"error at line %d in file %s\n",cur_line_number,cur_load_file); \
	else\
		NhlPError(errlev,NhlEUNKNOWN,"error at line %d\n",cur_line_number); 

/*
 * Note concerning line numbers: in an interactive session the first line is
 * numbered 0, corresponding to the prompt "ncl 0>". Files executed in batch mode
 * or loaded with the "load" command have lines numbered from 1, corresponding to 
 * how an editor like emacs or vi numbers the lines.
 * Also unlike in batch mode, in interactive line numbers are not incremented for 
 * statements continued on the next line using the continuation char ('\').
 */

int _NclParseString
#if NhlNeedProto
(char *str,int reset)
#else
(str,reset)
char *str;
int reset;
#endif
{
        int tmp;
        yy_scan_string(str);
        tmp = yyparse(reset);
 
        yy_flush_buffer(YY_CURRENT_BUFFER );
        yy_delete_buffer( YY_CURRENT_BUFFER );
        return(tmp);

}

void ResetCurLine(void) {
	last_line_length = cur_line_length;
	cur_line_length = 0;
        cur_line_text_pos = &(cur_line_text[0]);
        *cur_line_text_pos = '\0';
}


int rec = 0;
FILE *recfp;
void SetLine
#if NhlNeedProto
(int line)
#else
(line)
int line;
#endif
{
        cur_line_number = line;
        return;
}

void IncLine
#if NhlNeedProto
(void)
#else
()
#endif
{
	cur_line_number++;
	return;
}
void CopyIt
#if NhlNeedProto
(char **cltp,int *cll,int ylg,char* ytxt)
#else
(cltp,cll,ylg,ytxt)
char **cltp;
int *cll;
int ylg;
char* ytxt;
#endif
{
	while (*cll + ylg > cur_line_maxsize -1) {
		cur_line_text = (char*)NclRealloc(cur_line_text,2*cur_line_maxsize);
		*cltp = &(cur_line_text[*cll]);
		cur_line_maxsize = 2 * cur_line_maxsize;
	}
	*cll += yyleng;
        strcpy(*cltp,ytxt);
        (*cltp) += ylg;
	return;
}
#define MAX_INCLUDE_DEPTH 20
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_numbers[MAX_INCLUDE_DEPTH];
char *file_names[1+MAX_INCLUDE_DEPTH];

void _NclResetScanner
#if NhlNeedProto
(void)
#else
()
#endif
{
	preloading = 0;
	if ( --loading < 0 ) {
		loading = 0;
		return;
	} else {
		yy_flush_buffer(YY_CURRENT_BUFFER );
		yy_delete_buffer( YY_CURRENT_BUFFER );
/*
		yy_set_bol(1);
*/
		if(include_stack[loading] != NULL)  {
			yy_switch_to_buffer(include_stack[loading] );
		}
		SetLine(line_numbers[loading]);

        if ((cur_load_file != NULL) && (load_file != NULL)) {
            if (!(strcmp(cur_load_file, load_file))) {
                /* done loading file */
                memset(&load_file, '\0', 255);
                load_stmt = 0;
                if (NCLoverrideEcho) {
                    NCLecho = 1;
                    load_stmt = 0;
                }
                else
                    NCLecho = echo;
            }
            else {
                if (load_stmt) {
                    load_stmt = 0;
                    if (NCLoverrideEcho) {
                        NCLecho = 1;
                        load_stmt = 0;
                    }
                    else
                        NCLecho = echo; 
                }
            }
        } else {
            memset(&load_file, '\0', 255);
            load_stmt = 0;
            if (NCLoverrideEcho) {
                NCLecho = 1;
                load_stmt = 0;
            }
        }
        }
	if(NULL != file_names[loading+1])
	{
		NclFree(file_names[loading+1]);
		file_names[loading+1] = NULL;
	}
	cur_load_file = file_names[loading];
	if(yyin != NULL) {
		if(loading) {
			cmd_line = (isatty(fileno(yyin))?1:0);
		} else if(cmd_line_is_set) {
			cmd_line = cmd_line_is_set;
		} else {
			cmd_line = (isatty(fileno(yyin))?1:0);
		}
	} else if(cmd_line_is_set) {
		cmd_line = cmd_line_is_set;
	} else {
		cmd_line = 0;
	}
}

NhlErrorTypes _NclPreLoadScript
#if	NhlNeedProto
(char *path,int status)
#else
(char,status)
char *path;
int status;
#endif
{
	static int first = 1;
	char *tmp = path;
	FILE *tmpfile = NULL;

	preloading = status;	

	if((first)&&!(YY_CURRENT_BUFFER)) {
		yyin = stdin;
		yy_switch_to_buffer(yy_create_buffer( yyin,  YY_BUF_SIZE ) );
		first = 0;	
	} else {
		first = 0;	
	}

	if ( loading >= MAX_INCLUDE_DEPTH )
	{
		fprintf( stderr, "Includes nested too deeply" );
		_NclExit( 1 );
	}
	line_numbers[loading] = cur_line_number;
	SetLine(0);
	file_names[loading] = cur_load_file;
	file_names[loading+1] = NULL;
	include_stack[loading++] = YY_CURRENT_BUFFER;
	tmpfile = fopen( _NGResolvePath(tmp), "r" );
	if(tmpfile != NULL) {
		NCL_PROF_REGISTER_MDATA(0, tmp);
		yyin = tmpfile;
		yy_switch_to_buffer(yy_create_buffer( yyin, YY_BUF_SIZE ) );
		cmd_line = isatty(fileno(yyin));
		cur_load_file = NclMalloc((unsigned)strlen(tmp)+1);
		strcpy(cur_load_file,tmp);
		return(NhlNOERROR);
	} else {
		loading--;
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",tmp);
		PRINTLOCATION(NhlFATAL);
		_NclResetScanner();
		SetLine(line_numbers[loading]);
		return(NhlFATAL);
	}
}
%}
%a 4000
%o 4000
let	[_a-zA-Z]
alnum   [_a-zA-Z0-9]
file_grp_var_alnum   [_a-zA-Z0-9/]
file_grp_var_compound_alnum   [_a-zA-Z0-9/.]
white	[\t ]
digit	[0-9]
inttype [bBChHiIlLqQ]

%x incl
%x IN_COMMENT
%%

	if(inc_line) {
		IncLine();
		if(cmd_line) {
			_NclCallPromptFunc(cur_line_number);
		} 
		inc_line = 0;
	} 
        if (cur_line_length == 0) {
                cur_line_text_pos = &(cur_line_text[0]);
                *cur_line_text_pos = '\0';
        }
"load" 	{ 
    BEGIN incl;
    load_stmt = 1;
    if (cur_load_file != NULL)
        (void) strcpy(load_file, cur_load_file);
}

<incl>[ \t]*      {
	/*CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);*/
}
<incl>["][^"\n]*[\n] {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Syntax Error : \\n not allowed in strings");
	PRINTLOCATION(NhlFATAL);
	IncLine();
	if(cmd_line) {
        	_NclCallPromptFunc(cur_line_number);
        }
	BEGIN 0;
}

<incl>["][^"\n]*["] {
	char *tmp;
	FILE *tmpfile = NULL;

	/*CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);*/
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;

	NCL_PROF_REGISTER_MDATA(0, tmp);
    if (NCLecho)
        nclfprintf(stdout, "Loading file \"%s\"\n", tmp);
    if (load_stmt) {
            echo = NCLecho;
            if (NCLoverrideEcho) {
                NCLecho = 1;
                load_stmt = 0;
            }
            else
                NCLecho = 0;
    }

	if ( loading >= MAX_INCLUDE_DEPTH )
	{
		fprintf( stderr, "Includes nested too deeply" );
		_NclExit( 1 );
	}
	line_numbers[loading] = cur_line_number;
	file_names[loading] = cur_load_file;
	include_stack[loading++] = YY_CURRENT_BUFFER;
	tmpfile = fopen( _NGResolvePath(tmp), "r" );
	if(tmpfile != NULL) {
                /* Note that this is slightly different behavior than _NclPreLoadScript. 
                 * Here line number is set to one and the inc_line flag set to 0. That is 
                 * because the lines at the beginning of the lex code following the '%%'
                 * do not get executed until after the first piece of text is parsed when
                 * load is matched. I'm not sure why this is at this point, but if the 
                 * first piece of text is a comment that consumes a whole line then without
                 * this change, subsequent line numbers are one less than what they should be. 
                 * -dib
                 */
                SetLine(1);
		inc_line = 0;
		yyin = tmpfile;
		yy_switch_to_buffer(yy_create_buffer( yyin, YY_BUF_SIZE ) );
		cmd_line = isatty(fileno(yyin));
		cur_load_file = NclMalloc((unsigned)strlen(tmp)+1);
		strcpy(cur_load_file,tmp);
	} else {
		loading--;
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",tmp);
		PRINTLOCATION(NhlFATAL);
		_NclResetScanner();

		SetLine(line_numbers[loading]);
	}

    BEGIN 0;
}

<INITIAL>{
     "/;"      BEGIN(IN_COMMENT); 
}

<IN_COMMENT>{
     ";/"      BEGIN(INITIAL);
     [^;\n]+   /* eat comment in chunks */
     ";"       /* eat the lone semicolon */
     \n        { IncLine(); if(cmd_line) _NclCallPromptFunc(cur_line_number); }
}

<<EOF>> {
	inc_line = 1;
	yyterminate();
/*	if ( --loading < 0 ) {
		loading = 0;
		fprintf(stdout,"yyterminate....\n");
		yy_flush_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer(yy_create_buffer( stdin , YY_BUF_SIZE) );
		yy_set_interactive(1);
		yyterminate();
	} else {
		yy_flush_buffer( YY_CURRENT_BUFFER );
		yy_delete_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer(include_stack[loading] );
		SetLine(line_numbers[loading]);

		cur_load_file = file_names[loading];
	}
*/
}
\;[^\n\\]*[\\][ \t]*[\n] {
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);

    if (NCLoverrideEcho) {
        old_load_stmt = load_stmt;
        load_stmt = 0;
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
        load_stmt = old_load_stmt;
    } else {
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
    }

	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	ResetCurLine();
        if(cmd_line) {
               _NclCallPromptFunc(cur_line_number);
        } else {
		IncLine();
	}
}
[\\][^\n]*[\n] {
    if (NCLoverrideEcho) {
        old_load_stmt = load_stmt;
        load_stmt = 0;\
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
        load_stmt = old_load_stmt;
    } else {
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
    }
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	ResetCurLine();
        if(cmd_line) {
               _NclCallPromptFunc(cur_line_number);
        } else { 
		IncLine();
	}

}
\;[^\n]*[\n] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
    if (NCLoverrideEcho) {
        old_load_stmt = load_stmt;
        load_stmt = 0;
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
        load_stmt = old_load_stmt;
    } else {
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s", cur_line_text);
            (void) fflush(stdout);
        }
    }

	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	inc_line = 1;
	return EOLN;
}
\r?\n {
	printtoken(EOLN,(char*)NULL);

    if (NCLoverrideEcho) {
        old_load_stmt = load_stmt;
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
        load_stmt = old_load_stmt;
    } else {
        if (NCLecho != 0 && !load_stmt) {
            (void) fprintf(stdout, "+ %s\n", cur_line_text);
            (void) fflush(stdout);
        }
    }

	inc_line = 1;
	cur_line_length += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s\n",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	return EOLN;
}
[ \t][ \t]* { 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	
}

{digit}+\.|{digit}*\.{digit}+ {
		sscanf(yytext,"%lf",&(yylval.real));
		printtoken(REAL,yytext);
		CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
		return REAL;
}
{digit}+\.?{digit}*[eE][+-]?{digit}+|{digit}?\.{digit}+[eE][+-]?{digit}+ {
	sscanf(yytext,"%le",&(yylval.real));
	printtoken(REAL,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return REAL;
}
{digit}+\.?{digit}*[dD][+-]?{digit}+|{digit}?\.{digit}+[dD][+-]?{digit}+|{digit}+\.?{digit}*[dD]|{digit}?\.{digit}+[dD] {
        char *tmp = strpbrk(yytext,"dD");
        *tmp = (yytext + yyleng - 1 == tmp) ? '\0' : 'e';
	sscanf(yytext,"%le",&(yylval.real));
        *tmp = 'd';
	printtoken(REAL,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return REAL;
}
{digit}+{inttype}? {
	sscanf(yytext,"%llu",&(yylval.int_val));
	printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return INT;
}
{digit}+{inttype}?(".gt."|".GT.") {
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".lt."|".LT.") {
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".le."|".LE.") {
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".eq."|".EQ.") {
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".ge."|".GE.") {
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".ne."|".NE.") {
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".and."|".AND.") {
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".or."|".OR.") {
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+{inttype}?(".not."|".NOT.") {
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%llu",&(yylval.int_val));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
".gt."|".GT." {
	printtoken(GT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GT;
}
".lt."|".LT." {
	printtoken(LT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return LT;
} 
".le."|".LE." { 
	printtoken(LE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return LE;
}
".eq."|".EQ." { 
	printtoken(EQ,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return EQ;
}
".ge."|".GE." {
	printtoken(GE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GE;
}
".not."|".NOT." {
	printtoken(NOT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return NOT;
}
".ne."|".NE." {
	printtoken(NE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return NE;
}
".and."|".AND." {
	printtoken(AND,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return AND;
}
".or."|".OR." {
	printtoken(OR,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return OR;
}
".xor."|".XOR." {
	printtoken(XOR,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return XOR;
}
"->/"{file_grp_var_compound_alnum}+?("/)") {
	char *tmp;

	yyless(yyleng - strlen("/)"));

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"->/"{file_grp_var_compound_alnum}+?(".eq."|".EQ."|".ne."|".NE."|".le."|".LE."|".lt."|".LT."|".ge."|".GE."|".gt."|".GT."|".or."|".OR.") {
	char *tmp;

	yyless(yyleng - strlen(".eq."));

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"->/"{file_grp_var_compound_alnum}+?(".and."|".AND."|".xor."|".XOR."|".not."|".NOT.") {
        char *tmp;

        yyless(yyleng - strlen(".and."));

        tmp = yytext + 2;

        strcpy(yylval.str,tmp);
        printtoken(FVAR,yylval.str);
        CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return FVAR;
}
"->/"{file_grp_var_compound_alnum}+ {
	char *tmp;

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"->"{file_grp_var_compound_alnum}+?(".eq."|".EQ."|".ne."|".NE."|".le."|".LE."|".lt."|".LT."|".ge."|".GE."|".gt."|".GT."|".or."|".OR.") {
	char *tmp;

	yyless(yyleng - strlen(".eq."));

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"->"{file_grp_var_compound_alnum}+?(".and."|".AND."|".xor."|".XOR."|".not."|".NOT.") {
        char *tmp;

        yyless(yyleng - strlen(".and."));

        tmp = yytext + 2;

        strcpy(yylval.str,tmp);
        printtoken(FVAR,yylval.str);
        CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return FVAR;
}
"->"{alnum}+ {
	char *tmp;

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"$" {
	strcpy(yylval.str,yytext);
	printtoken(EFSTRING,yylval.str);

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return EFSTRING;
}
"->$" {
	strcpy(yylval.str,yytext);
	printtoken(FSTRING,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FSTRING;
}
"=>"{file_grp_var_alnum}+ {
	char *tmp;

	tmp = yytext + 2;

	strcpy(yylval.str,tmp);

	printtoken(GVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GVAR;
}
"=>$" {
	strcpy(yylval.str,yytext);
	printtoken(GSTRING,yylval.str);

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GSTRING;
}

{let}{alnum}* {
        NclSymbol *s;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        if(( s = _NclLookUp(yytext)) == NULL) 
		s = _NclAddSym(yytext,UNDEF);
	if (s->type == IFUNC || s->type == NFUNC) {
	    ncl_cur_func = s->name;
	}
       	yylval.sym = s;
       	printtoken(s->type,yytext);
	if((rec == 1)&&(s->type == QUIT)) {
		fclose(recfp);
	}
       	return s->type;
}
[!] {
	strcpy(yylval.str,&(yytext[1]));
	printtoken(DIM_MARKER,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return DIM_MARKER;
}
[@]{let}{alnum}* {
	char *tmp;

	tmp = (char*)yytext + 1;
	strcpy(yylval.str,tmp);
	printtoken(ATTNAME,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return ATTNAME;

}
"@$" {
	strcpy(yylval.str,yytext);
	printtoken(ASTRING,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return ASTRING;
}
{let}{alnum}*[ \t]*[\|] {
	char *tmp;

	tmp = strchr(yytext,' ');
	if (tmp != NULL) {
		*tmp = '\0';
	} else {
	   tmp = strchr(yytext,'\t');
	   if (tmp != NULL) {
		*tmp = '\0';
	   } else {
	   	tmp = strchr(yytext,'|');;
	   	if (tmp != NULL) {
			*tmp = '\0';
	   	} 
	   }
	}
	strcpy(yylval.str,yytext);
	printtoken(DIM,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return DIM;
}
[&]{let}{alnum}* {
	char *tmp;
	tmp = (char*)yytext + 1;
	strcpy(yylval.str,tmp);
	printtoken(COORDV,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return COORDV;
/* NOTREACHED */
}
"&$" {
	strcpy(yylval.str,yytext);
	printtoken(CSTRING,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return CSTRING;
}
["][^"\n]*["] {
	char *tmp;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	yylval.sstr = NclMalloc(strlen(tmp) + 1);
	strncpy(yylval.sstr,tmp,strlen(tmp)+1);
	printtoken(STRING,yylval.sstr);
	return STRING;
}
\: { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(COLON); 
}
\{ { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	printtoken(yytext[0],(char*)NULL); 
	return(LBC); 
}
\} { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RBC); 
}
\( { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LP); 
}
"(\/" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LPSLSH); 
}
\) { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RP); 
}
"\/)" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(SLSHRP); 
}
\[ { 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LBK); 
}
\] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RBK); 
}
"[\/" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LBKSLSH); 
}
"\/]" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(SLSHRBK); 
}
\' {
	printtoken(yytext[0],(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(MARKER);
}
[\177] {
	yytext[0] = (char)'\177';
	return(EOFF);
}
. {  
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	printtoken(yytext[0],(char*)NULL);
	return(yytext[0]);
}
":=" {
	printtoken(REASSIGN,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return REASSIGN;
}
