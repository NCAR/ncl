%{
/*#include <stdio.h>*/
#include <string.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "parser.h"
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
char *cur_line_text = NULL;
char *cur_line_text_pos = NULL;
int cur_line_length = 0;
int cur_line_number= -1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
int cur_line_maxsize = 0;
extern int loading;
#include <ctype.h>
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
extern int cmd_line;
extern int cmd_line_is_set;
extern char *cur_load_file;
int inc_line = 1;
int _NclParseString
#if NhlNeedProto
(char *str,int reset)
#else
(str,reset)
char *str;
int reset;
#endif
{
	yy_scan_string(str);
	return(yyparse(reset));
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
	if(*cll + ylg > cur_line_maxsize -1) {
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
char *file_names[MAX_INCLUDE_DEPTH];

void _NclResetScanner
#if NhlNeedProto
(void)
#else
()
#endif
{
	if ( --loading < 0 ) {
		loading = 0;
		return;
	} else {
/*
		yy_delete_buffer( YY_CURRENT_BUFFER );
*/
		yy_switch_to_buffer(include_stack[loading] );
/*
		yy_flush_buffer(include_stack[loading]);
*/
		SetLine(line_numbers[loading]);
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
}
%}
%a 4000
%o 4000
let	[_a-zA-Z]
alnum   [_a-zA-Z0-9]
white	[\t ]
digit	[0-9]
%x incl
%%
	if(inc_line) {
		IncLine();
		if(cmd_line) {
			_NclCallPromptFunc(cur_line_number);
		} 
		inc_line = 0;
	} 
"load" 	{ 
		BEGIN incl;
	}
<incl>[ \t]*      {
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
}
<incl>["][^"\n]*[\n] {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Syntax Error : \\n not allowed in strings");
	IncLine();
	if(cmd_line) {
        	_NclCallPromptFunc(cur_line_number);
        }
	BEGIN 0;
}
<incl>["][^"\n]*["] {
	char *tmp;
	FILE *tmpfile = NULL;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	if ( loading >= MAX_INCLUDE_DEPTH )
	{
		fprintf( stderr, "Includes nested too deeply" );
		exit( 1 );
	}
	line_numbers[loading] = cur_line_number;
	SetLine(0);
	file_names[loading] = cur_load_file;
	include_stack[loading++] = YY_CURRENT_BUFFER;
	tmpfile = fopen( _NGResolvePath(tmp), "r" );
	if(tmpfile != NULL) {
		yyin = tmpfile;
		yy_switch_to_buffer(yy_create_buffer( yyin, YY_BUF_SIZE ) );
		cmd_line = isatty(fileno(yyin));
		cur_load_file = NclMalloc((unsigned)strlen(tmp)+1);
		strcpy(cur_load_file,tmp);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)\n",tmp);
		_NclResetScanner();


		SetLine(line_numbers[loading]);
	}

	BEGIN 0;
}
<<EOF>> {
	if ( --loading < 0 ) {
		loading = 0;
		yyterminate();
	} else {
		yy_delete_buffer( YY_CURRENT_BUFFER );
		yy_switch_to_buffer(include_stack[loading] );
		SetLine(line_numbers[loading]);

		cur_load_file = file_names[loading];
	}
}
\;[^\n\\]*[\\][ \t]*[\n] {
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	IncLine();
}
[\\][^\n]*[\n] {
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	IncLine();
}
\;[^\n]*[\n] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	inc_line = 1;
	return EOLN;
}
[\n] {
	printtoken(EOLN,(char*)NULL);
/*
	if(cur_line_length == 0) {
		IncLine();
	} else {
		BEGIN NEWLINE;
	}
*/
	inc_line = 1;
	cur_line_length += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s\n",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	return EOLN;
}
[ \t] { 
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
{digit}+ {
	sscanf(yytext,"%d",&(yylval.integer));
	printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return INT;
}
{digit}+(".gt."|".GT.") {
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".lt."|".LT.") {
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".le."|".LE.") {
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".eq."|".EQ.") {
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".ge."|".GE.") {
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".ne."|".NE.") {
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".and."|".AND.") {
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".or."|".OR.") {
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".not."|".NOT.") {
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%d",&(yylval.integer));
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
".not."|".not." {
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
{let}{alnum}* {
        NclSymbol *s;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        if(( s = _NclLookUp(yytext)) == NULL) 
		s = _NclAddSym(yytext,UNDEF);
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
	strncpy(yylval.str,tmp,strlen(tmp)+1);
	printtoken(STRING,yylval.str);
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
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LBK); 
}
\] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RBK); 
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
	printtoken(yytext[0],(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(yytext[0]);
}
