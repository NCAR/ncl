# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN _yybgin = _yysvec + 1 +
# define INITIAL 0
# define YYLERR _yysvec
# define YYSTATE (_yyestate-_yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,_yyout)
# define input() (((_yytchar=_yysptr>_yysbuf?U(*--_yysptr):getc(_yyin))==10?(_yylineno++,_yytchar):_yytchar)==EOF?0:_yytchar)
# define unput(c) {_yytchar= (c);if(_yytchar=='\n')_yylineno--;*_yysptr++=_yytchar;}
# define _yymore() (_yymorfg=1)
# define ECHO fprintf(_yyout, "%s",yytext)
# define REJECT { nstr = _yyreject(); goto _yyfussy;}
int yyleng; extern char yytext[];
int _yymorfg;
extern char *_yysptr, _yysbuf[];
int _yytchar;
FILE *_yyin = {stdin}, *_yyout = {stdout};
extern int _yylineno;
struct _yysvf { 
	struct _yywork *_yystoff;
	struct _yysvf *_yyother;
	int *_yystops;};
struct _yysvf *_yyestate;
extern struct _yysvf _yysvec[], *_yybgin;
/*#include <stdio.h>*/
#include <string.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "y.tab.h"
#ifdef DEBUGER
extern void printtoken(
#ifdef NhlNeedProto
	int /* type */
#endif
);
#else
#define printtoken(n,m)
#endif

#define MAXTMPSPACE 512
char cur_line_text[MAXTMPSPACE];
char *cur_line_text_pos = &(cur_line_text[0]);
int cur_line_length = 0;
int cur_line_number= 1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
extern int loading;
#include <ctype.h>
#ifdef MAKEAPI
#undef input
#undef unput
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
#define input() (( (int)the_input_buffer_ptr >=the_input_buffer_size + (int)the_input_buffer)? 0 : (((int)*the_input_buffer_ptr == 10) ? _yylineno++,*the_input_buffer_ptr++:*the_input_buffer_ptr++))

#define unput(c) { _yytchar = (c); if(_yytchar=='\n')_yylineno--;*(--the_input_buffer_ptr) = _yytchar; }
#endif /* MAKEAPI */

int rec = 0;
FILE *recfp;
# define AA 2
# define YYNEWLINE 10
_yylex(){
int nstr; extern int yyprevious;
while((nstr = _yylook()) >= 0)
_yyfussy: switch(nstr){
case 0:
if(_yywrap()) return(0); break;
case 1:
{
	cur_line_number++;
	BEGIN 0;
	yyless(yyleng-1);
}
break;
case 2:
{
	cur_line_number++;
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	return EOLN;
}
break;
case 3:
{
	BEGIN AA;
}
break;
case 4:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	cur_line_number++;
	return EOLN;
}
break;
case 5:
{
	printtoken(EOLN,(char*)NULL);
	cur_line_length += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s\n",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	BEGIN AA;
	return EOLN;
}
break;
case 6:
{ 
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	cur_line_length += yyleng;
	
}
break;
case 7:
{
		sscanf(yytext,"%lf",&(_yylval.real));
		printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
		return REAL;
}
break;
case 8:
{
	sscanf(yytext,"%le",&(_yylval.real));
	printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return REAL;
}
break;
case 9:
{
	sscanf(yytext,"%d",&(_yylval.integer));
	printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return INT;
}
break;
case 10:
{
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 11:
{
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 12:
{
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 13:
{
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 14:
{
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 15:
{
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 16:
{
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 17:
{
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 18:
{
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 19:
{
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 20:
{
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(_yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 21:
{
	printtoken(GT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GT;
}
break;
case 22:
{
	printtoken(LT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LT;
}
break;
case 23:
{ 
	printtoken(LE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LE;
}
break;
case 24:
{ 
	printtoken(EQ,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return EQ;
}
break;
case 25:
{
	printtoken(GE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GE;
}
break;
case 26:
{
	printtoken(NOT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NOT;
}
break;
case 27:
{
	printtoken(NE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NE;
}
break;
case 28:
{
	printtoken(AND,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return AND;
}
break;
case 29:
{
	printtoken(OR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return OR;
}
break;
case 30:
{
	printtoken(XOR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return XOR;
}
break;
case 31:
{

	strcpy(_yylval.str,yytext);
	printtoken(FVAR,_yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return FVAR;
}
break;
case 32:
{
        NclSymbol *s;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        if(( s = _NclLookUp(yytext)) == NULL) 
		s = _NclAddSym(yytext,UNDEF);
       	_yylval.sym = s;
       	printtoken(s->type,yytext);
	if((rec == 1)&&(s->type == QUIT)) {
		fclose(recfp);
	}
       	return s->type;
}
break;
case 33:
{
	strcpy(_yylval.str,&(yytext[1]));
	printtoken(DIM_MARKER,_yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM_MARKER;
}
break;
case 34:
{
	char *tmp;

	tmp = (char*)yytext + 1;
	strcpy(_yylval.str,tmp);
	printtoken(ATTNAME,_yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return ATTNAME;

}
break;
case 35:
{
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
	strcpy(_yylval.str,yytext);
	printtoken(DIM,_yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM;
}
break;
case 36:
{

	strcpy(_yylval.str,yytext);
	printtoken(COORD,_yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return COORD;
/* NOTREACHED */
}
break;
case 37:
{
	char *tmp;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	strncpy(_yylval.str,tmp,strlen(tmp)+1);
	printtoken(STRING,_yylval.str);
	return STRING;
}
break;
case 38:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(COLON); 
}
break;
case 39:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	printtoken(yytext[0],(char*)NULL); 
	return(LBC); 
}
break;
case 40:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBC); 
}
break;
case 41:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LP); 
}
break;
case 42:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LPSLSH); 
}
break;
case 43:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RP); 
}
break;
case 44:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(SLSHRP); 
}
break;
case 45:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LBK); 
}
break;
case 46:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBK); 
}
break;
case 47:
{
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(MARKER);
}
break;
case 48:
{  
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(yytext[0]);
}
break;
case -1:
break;
default:
fprintf(_yyout,"bad switch _yylook %d",nstr);
} return(0); }
/* end of _yylex */
int _yyvstop[] = {
0,

48,
0,

6,
48,
0,

5,
0,

33,
48,
0,

48,
0,

48,
0,

47,
48,
0,

41,
48,
0,

43,
48,
0,

48,
0,

48,
0,

48,
0,

9,
48,
0,

38,
48,
0,

48,
0,

48,
0,

32,
48,
0,

45,
48,
0,

48,
0,

46,
48,
0,

39,
48,
0,

40,
48,
0,

1,
48,
0,

1,
6,
48,
0,

2,
5,
0,

1,
33,
48,
0,

1,
48,
0,

1,
48,
0,

1,
47,
48,
0,

1,
41,
48,
0,

1,
43,
48,
0,

1,
48,
0,

1,
48,
0,

1,
48,
0,

1,
9,
48,
0,

1,
38,
48,
0,

1,
48,
0,

1,
48,
0,

1,
32,
48,
0,

1,
45,
48,
0,

1,
48,
0,

1,
46,
48,
0,

1,
39,
48,
0,

1,
40,
48,
0,

37,
0,

36,
0,

42,
0,

7,
0,

44,
0,

7,
0,

9,
0,

4,
0,

34,
0,

32,
0,

35,
0,

3,
0,

31,
0,

7,
0,

7,
0,

8,
0,

8,
0,

24,
0,

25,
0,

21,
0,

23,
0,

22,
0,

27,
0,

29,
0,

7,
0,

28,
0,

30,
0,

26,
0,

8,
0,

13,
0,

14,
0,

10,
0,

12,
0,

11,
0,

15,
0,

17,
0,

16,
0,

19,
0,

18,
20,
0,
0};
# define YYTYPE int
struct _yywork { YYTYPE verify, advance; } _yycrank[] = {
0,0,	0,0,	1,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
0,0,	0,0,	23,79,	31,0,	
49,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,8,	1,9,	
0,0,	0,0,	0,0,	1,10,	
1,11,	1,12,	1,13,	0,0,	
1,5,	16,69,	1,14,	1,15,	
1,16,	1,17,	0,0,	12,52,	
0,0,	76,76,	0,0,	2,11,	
2,12,	2,13,	0,0,	1,18,	
1,19,	2,14,	2,15,	2,16,	
0,0,	1,20,	1,21,	14,53,	
0,0,	0,0,	1,21,	0,0,	
0,0,	59,88,	2,18,	2,19,	
76,76,	0,0,	17,70,	0,0,	
17,71,	17,71,	17,71,	17,71,	
17,71,	17,71,	17,71,	17,71,	
17,71,	17,71,	3,27,	55,82,	
1,22,	1,23,	1,24,	56,83,	
60,89,	61,90,	3,28,	3,29,	
71,116,	17,72,	83,122,	84,123,	
85,124,	57,84,	58,86,	2,22,	
82,121,	2,24,	51,51,	51,51,	
51,51,	51,51,	51,51,	51,51,	
51,51,	51,51,	51,51,	51,51,	
57,85,	58,87,	3,30,	3,31,	
1,25,	1,5,	1,26,	3,32,	
3,33,	3,34,	3,35,	62,91,	
3,27,	17,72,	3,36,	3,37,	
3,38,	3,39,	63,92,	2,25,	
64,93,	2,26,	65,95,	4,33,	
4,34,	4,35,	66,97,	3,40,	
3,41,	4,36,	4,37,	4,38,	
9,49,	3,42,	3,43,	64,94,	
66,98,	65,96,	3,43,	67,99,	
9,49,	9,0,	4,40,	4,41,	
68,100,	86,125,	87,126,	88,127,	
76,78,	89,128,	15,54,	15,54,	
15,54,	15,54,	15,54,	15,54,	
15,54,	15,54,	15,54,	15,54,	
3,44,	3,45,	3,46,	90,129,	
9,49,	9,50,	91,130,	15,55,	
98,131,	9,49,	100,132,	15,56,	
102,134,	15,57,	9,49,	4,44,	
103,135,	4,46,	15,58,	9,49,	
15,59,	15,60,	104,136,	105,138,	
107,142,	108,143,	109,144,	110,145,	
114,152,	111,146,	15,61,	106,140,	
3,47,	3,27,	3,48,	9,49,	
9,49,	104,137,	105,139,	15,62,	
9,49,	106,141,	112,148,	15,63,	
111,147,	15,64,	113,150,	4,47,	
115,153,	4,48,	15,65,	121,157,	
15,66,	15,67,	129,158,	131,159,	
113,151,	112,149,	134,162,	135,163,	
136,164,	137,165,	15,68,	9,49,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	138,166,	139,167,	
140,168,	141,169,	10,51,	9,49,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	19,73,	142,170,	
143,171,	144,172,	151,173,	153,174,	
155,135,	156,145,	19,73,	19,74,	
162,175,	169,176,	171,177,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	19,73,	19,73,	
54,81,	0,0,	0,0,	19,73,	
0,0,	0,0,	0,0,	0,0,	
19,73,	72,117,	0,0,	72,117,	
0,0,	19,73,	72,118,	72,118,	
72,118,	72,118,	72,118,	72,118,	
72,118,	72,118,	72,118,	72,118,	
0,0,	0,0,	0,0,	0,0,	
0,0,	19,73,	19,73,	0,0,	
54,81,	0,0,	19,73,	75,75,	
75,75,	75,75,	75,75,	75,75,	
75,75,	75,75,	75,75,	75,75,	
75,75,	117,118,	117,118,	117,118,	
117,118,	117,118,	117,118,	117,118,	
117,118,	117,118,	117,118,	0,0,	
0,0,	19,73,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
0,0,	0,0,	0,0,	0,0,	
20,75,	19,73,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
21,76,	81,119,	0,0,	81,119,	
0,0,	0,0,	81,120,	81,120,	
81,120,	81,120,	81,120,	81,120,	
81,120,	81,120,	81,120,	81,120,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	21,76,	
0,0,	0,0,	0,0,	0,0,	
0,0,	101,101,	101,101,	101,101,	
101,101,	101,101,	101,101,	101,101,	
101,101,	101,101,	101,101,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	0,0,	101,133,	0,0,	
0,0,	0,0,	0,0,	0,0,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	101,133,	0,0,	
0,0,	0,0,	21,77,	0,0,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	0,0,	21,78,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	0,0,	
0,0,	0,0,	0,0,	53,80,	
0,0,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	70,101,	
70,101,	70,101,	70,101,	70,101,	
70,101,	70,101,	70,101,	70,101,	
70,101,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
70,102,	0,0,	0,0,	0,0,	
70,103,	0,0,	70,104,	0,0,	
0,0,	0,0,	0,0,	70,105,	
0,0,	70,106,	70,107,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	70,108,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
70,109,	0,0,	0,0,	0,0,	
70,110,	0,0,	70,111,	0,0,	
0,0,	0,0,	0,0,	70,112,	
0,0,	70,113,	70,114,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	70,115,	
116,154,	116,154,	116,154,	116,154,	
116,154,	116,154,	116,154,	116,154,	
116,154,	116,154,	119,120,	119,120,	
119,120,	119,120,	119,120,	119,120,	
119,120,	119,120,	119,120,	119,120,	
133,160,	116,155,	133,160,	0,0,	
0,0,	133,161,	133,161,	133,161,	
133,161,	133,161,	133,161,	133,161,	
133,161,	133,161,	133,161,	154,154,	
154,154,	154,154,	154,154,	154,154,	
154,154,	154,154,	154,154,	154,154,	
154,154,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	116,156,	160,161,	160,161,	
160,161,	160,161,	160,161,	160,161,	
160,161,	160,161,	160,161,	160,161,	
0,0};
struct _yysvf _yysvec[] = {
0,	0,	0,
_yycrank+-1,	0,		0,	
_yycrank+-16,	_yysvec+1,	0,	
_yycrank+-89,	0,		0,	
_yycrank+-104,	_yysvec+3,	0,	
_yycrank+0,	0,		_yyvstop+1,
_yycrank+0,	0,		_yyvstop+3,
_yycrank+0,	0,		_yyvstop+6,
_yycrank+0,	0,		_yyvstop+8,
_yycrank+-151,	0,		_yyvstop+11,
_yycrank+179,	0,		_yyvstop+13,
_yycrank+0,	0,		_yyvstop+15,
_yycrank+4,	0,		_yyvstop+18,
_yycrank+0,	0,		_yyvstop+21,
_yycrank+5,	0,		_yyvstop+24,
_yycrank+122,	0,		_yyvstop+26,
_yycrank+4,	0,		_yyvstop+28,
_yycrank+32,	0,		_yyvstop+30,
_yycrank+0,	0,		_yyvstop+33,
_yycrank+-301,	0,		_yyvstop+36,
_yycrank+329,	0,		_yyvstop+38,
_yycrank+443,	0,		_yyvstop+40,
_yycrank+0,	0,		_yyvstop+43,
_yycrank+4,	0,		_yyvstop+46,
_yycrank+0,	0,		_yyvstop+48,
_yycrank+0,	0,		_yyvstop+51,
_yycrank+0,	0,		_yyvstop+54,
_yycrank+0,	0,		_yyvstop+57,
_yycrank+0,	0,		_yyvstop+60,
_yycrank+0,	0,		_yyvstop+64,
_yycrank+0,	0,		_yyvstop+67,
_yycrank+-5,	_yysvec+9,	_yyvstop+71,
_yycrank+0,	_yysvec+10,	_yyvstop+74,
_yycrank+0,	0,		_yyvstop+77,
_yycrank+0,	_yysvec+12,	_yyvstop+81,
_yycrank+0,	0,		_yyvstop+85,
_yycrank+0,	_yysvec+14,	_yyvstop+89,
_yycrank+0,	_yysvec+15,	_yyvstop+92,
_yycrank+0,	_yysvec+16,	_yyvstop+95,
_yycrank+0,	_yysvec+17,	_yyvstop+98,
_yycrank+0,	0,		_yyvstop+102,
_yycrank+0,	_yysvec+19,	_yyvstop+106,
_yycrank+0,	_yysvec+20,	_yyvstop+109,
_yycrank+0,	_yysvec+21,	_yyvstop+112,
_yycrank+0,	0,		_yyvstop+116,
_yycrank+0,	_yysvec+23,	_yyvstop+120,
_yycrank+0,	0,		_yyvstop+123,
_yycrank+0,	0,		_yyvstop+127,
_yycrank+0,	0,		_yyvstop+131,
_yycrank+-6,	_yysvec+9,	0,	
_yycrank+0,	0,		_yyvstop+135,
_yycrank+62,	_yysvec+10,	_yyvstop+137,
_yycrank+0,	0,		_yyvstop+139,
_yycrank+520,	0,		0,	
_yycrank+267,	0,		_yyvstop+141,
_yycrank+13,	0,		0,	
_yycrank+14,	0,		0,	
_yycrank+36,	0,		0,	
_yycrank+37,	0,		0,	
_yycrank+4,	0,		0,	
_yycrank+14,	0,		0,	
_yycrank+18,	0,		0,	
_yycrank+21,	0,		0,	
_yycrank+25,	0,		0,	
_yycrank+39,	0,		0,	
_yycrank+41,	0,		0,	
_yycrank+45,	0,		0,	
_yycrank+45,	0,		0,	
_yycrank+53,	0,		0,	
_yycrank+0,	0,		_yyvstop+143,
_yycrank+595,	0,		_yyvstop+145,
_yycrank+54,	_yysvec+17,	_yyvstop+147,
_yycrank+302,	0,		0,	
_yycrank+0,	_yysvec+19,	0,	
_yycrank+0,	0,		_yyvstop+149,
_yycrank+323,	_yysvec+20,	_yyvstop+151,
_yycrank+44,	0,		0,	
_yycrank+0,	_yysvec+21,	_yyvstop+153,
_yycrank+0,	0,		_yyvstop+155,
_yycrank+0,	0,		_yyvstop+157,
_yycrank+0,	_yysvec+53,	_yyvstop+159,
_yycrank+410,	0,		0,	
_yycrank+40,	0,		0,	
_yycrank+56,	0,		0,	
_yycrank+57,	0,		0,	
_yycrank+58,	0,		0,	
_yycrank+119,	0,		0,	
_yycrank+120,	0,		0,	
_yycrank+121,	0,		0,	
_yycrank+123,	0,		0,	
_yycrank+101,	0,		0,	
_yycrank+86,	0,		0,	
_yycrank+0,	_yysvec+83,	0,	
_yycrank+0,	_yysvec+84,	0,	
_yycrank+0,	_yysvec+85,	0,	
_yycrank+0,	_yysvec+86,	0,	
_yycrank+0,	_yysvec+87,	0,	
_yycrank+0,	_yysvec+88,	0,	
_yycrank+72,	0,		0,	
_yycrank+0,	_yysvec+89,	0,	
_yycrank+76,	0,		0,	
_yycrank+433,	0,		_yyvstop+161,
_yycrank+114,	0,		0,	
_yycrank+115,	_yysvec+72,	0,	
_yycrank+133,	0,		0,	
_yycrank+134,	0,		0,	
_yycrank+142,	0,		0,	
_yycrank+122,	0,		0,	
_yycrank+126,	0,		0,	
_yycrank+96,	0,		0,	
_yycrank+94,	_yysvec+72,	0,	
_yycrank+108,	0,		0,	
_yycrank+121,	0,		0,	
_yycrank+125,	0,		0,	
_yycrank+94,	0,		0,	
_yycrank+117,	0,		0,	
_yycrank+668,	_yysvec+70,	_yyvstop+163,
_yycrank+333,	0,		0,	
_yycrank+0,	_yysvec+117,	_yyvstop+165,
_yycrank+678,	0,		0,	
_yycrank+0,	_yysvec+119,	_yyvstop+167,
_yycrank+185,	0,		0,	
_yycrank+0,	0,		_yyvstop+169,
_yycrank+0,	0,		_yyvstop+171,
_yycrank+0,	0,		_yyvstop+173,
_yycrank+0,	0,		_yyvstop+175,
_yycrank+0,	0,		_yyvstop+177,
_yycrank+0,	0,		_yyvstop+179,
_yycrank+0,	0,		_yyvstop+181,
_yycrank+188,	0,		0,	
_yycrank+0,	_yysvec+121,	0,	
_yycrank+189,	0,		0,	
_yycrank+0,	_yysvec+129,	0,	
_yycrank+693,	0,		0,	
_yycrank+170,	0,		0,	
_yycrank+193,	0,		0,	
_yycrank+194,	0,		0,	
_yycrank+195,	0,		0,	
_yycrank+224,	0,		0,	
_yycrank+225,	0,		0,	
_yycrank+226,	0,		0,	
_yycrank+189,	0,		0,	
_yycrank+257,	0,		0,	
_yycrank+222,	0,		0,	
_yycrank+205,	0,		0,	
_yycrank+0,	_yysvec+135,	0,	
_yycrank+0,	_yysvec+136,	0,	
_yycrank+0,	_yysvec+137,	0,	
_yycrank+0,	_yysvec+138,	0,	
_yycrank+0,	_yysvec+139,	0,	
_yycrank+0,	_yysvec+140,	0,	
_yycrank+190,	0,		0,	
_yycrank+0,	_yysvec+142,	0,	
_yycrank+193,	0,		0,	
_yycrank+703,	0,		_yyvstop+183,
_yycrank+227,	0,		0,	
_yycrank+196,	0,		0,	
_yycrank+0,	0,		_yyvstop+185,
_yycrank+0,	0,		_yyvstop+187,
_yycrank+0,	0,		_yyvstop+189,
_yycrank+722,	0,		0,	
_yycrank+0,	_yysvec+160,	_yyvstop+191,
_yycrank+266,	0,		0,	
_yycrank+0,	0,		_yyvstop+193,
_yycrank+0,	0,		_yyvstop+195,
_yycrank+0,	0,		_yyvstop+197,
_yycrank+0,	0,		_yyvstop+199,
_yycrank+0,	0,		_yyvstop+201,
_yycrank+0,	0,		_yyvstop+203,
_yycrank+267,	0,		0,	
_yycrank+0,	0,		_yyvstop+205,
_yycrank+268,	0,		0,	
_yycrank+0,	_yysvec+162,	0,	
_yycrank+0,	_yysvec+169,	0,	
_yycrank+0,	_yysvec+171,	0,	
_yycrank+0,	0,		_yyvstop+207,
_yycrank+0,	0,		_yyvstop+209,
_yycrank+0,	0,		_yyvstop+211,
0,	0,	0};
struct _yywork *_yytop = _yycrank+779;
struct _yysvf *_yybgin = _yysvec+1;
char _yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,'!' ,'"' ,01  ,01  ,01  ,'&' ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
'@' ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,0134,01  ,01  ,'A' ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,'|' ,01  ,01  ,01  ,
0};
char _yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int _yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct _yysvf *_yylstate [YYLMAX], **_yylsp, **_yyolsp;
char _yysbuf[YYLMAX];
char *_yysptr = _yysbuf;
int *_yyfnd;
extern struct _yysvf *_yyestate;
int yyprevious = YYNEWLINE;
_yylook(){
	register struct _yysvf *_yystate, **lsp;
	register struct _yywork *_yyt;
	struct _yysvf *_yyz;
	int _yych, _yyfirst;
	struct _yywork *_yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *_yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	_yyfirst=1;
	if (!_yymorfg)
		_yylastch = yytext;
	else {
		_yymorfg=0;
		_yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = _yylstate;
		_yyestate = _yystate = _yybgin;
		if (yyprevious==YYNEWLINE) _yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(_yyout,"state %d\n",_yystate-_yysvec-1);
# endif
			_yyt = _yystate->_yystoff;
			if(_yyt == _yycrank && !_yyfirst){  /* may not be any transitions */
				_yyz = _yystate->_yyother;
				if(_yyz == 0)break;
				if(_yyz->_yystoff == _yycrank)break;
				}
			*_yylastch++ = _yych = input();
			_yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(_yyout,"char ");
				allprint(_yych);
				putchar('\n');
				}
# endif
			_yyr = _yyt;
			if ( (int)_yyt > (int)_yycrank){
				_yyt = _yyr + _yych;
				if (_yyt <= _yytop && _yyt->verify+_yysvec == _yystate){
					if(_yyt->advance+_yysvec == YYLERR)	/* error transitions */
						{unput(*--_yylastch);break;}
					*lsp++ = _yystate = _yyt->advance+_yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)_yyt < (int)_yycrank) {		/* r < _yycrank */
				_yyt = _yyr = _yycrank+(_yycrank-_yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(_yyout,"compressed state\n");
# endif
				_yyt = _yyt + _yych;
				if(_yyt <= _yytop && _yyt->verify+_yysvec == _yystate){
					if(_yyt->advance+_yysvec == YYLERR)	/* error transitions */
						{unput(*--_yylastch);break;}
					*lsp++ = _yystate = _yyt->advance+_yysvec;
					goto contin;
					}
				_yyt = _yyr + YYU(_yymatch[_yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(_yyout,"try fall back character ");
					allprint(YYU(_yymatch[_yych]));
					putchar('\n');
					}
# endif
				if(_yyt <= _yytop && _yyt->verify+_yysvec == _yystate){
					if(_yyt->advance+_yysvec == YYLERR)	/* error transition */
						{unput(*--_yylastch);break;}
					*lsp++ = _yystate = _yyt->advance+_yysvec;
					goto contin;
					}
				}
			if ((_yystate = _yystate->_yyother) && (_yyt= _yystate->_yystoff) != _yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(_yyout,"fall back to state %d\n",_yystate-_yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--_yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(_yyout,"state %d char ",_yystate-_yysvec-1);
				allprint(_yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(_yyout,"stopped at %d with ",*(lsp-1)-_yysvec-1);
			allprint(_yych);
			putchar('\n');
			}
# endif
		while (lsp-- > _yylstate){
			*_yylastch-- = 0;
			if (*lsp != 0 && (_yyfnd= (*lsp)->_yystops) && *_yyfnd > 0){
				_yyolsp = lsp;
				if(_yyextra[*_yyfnd]){		/* must backup */
					while(_yyback((*lsp)->_yystops,-*_yyfnd) != 1 && lsp > _yylstate){
						lsp--;
						unput(*_yylastch--);
						}
					}
				yyprevious = YYU(*_yylastch);
				_yylsp = lsp;
				yyleng = _yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(_yyout,"\nmatch ");
					sprint(yytext);
					fprintf(_yyout," action %d\n",*_yyfnd);
					}
# endif
				return(*_yyfnd++);
				}
			unput(*_yylastch);
			}
		if (yytext[0] == 0  /* && feof(_yyin) */)
			{
			_yysptr=_yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		_yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
_yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
_yyinput(){
	return(input());
	}
_yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
