/************************************************************************
File Name :		scanner.c
Compiler :		MS Visual Studio 2013
Author :		Sanket Patel (040-809-189) & Yasser Noor (040-815-665)
Course :		CST 8152 – Compilers, Lab Section: 011
Assignment :	3
Date :			24 November 2016
Professor :		Sv. Ranev
Purpose :		Modified VID functions
Function List : scanner_init(), mlwpar_next_token), get_next_state(),
char_class(), aa_func02(), aa_func03(), aa_func05(),
aa_func08(), aa_func10(), aa_func11(), atool()
**************************************************************************/


/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */
#include <math.h> /*included by me*/
/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

extern STD sym_table;

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/***********************************************************************************

Purpose :			Intializes the scanner input buffer and some other
scanner components
Author :			Svillen Ranev
History/Version :	1.0
Called functions :	b_isempty(), b_setmark(), b_reset(), b_retract_to_mark()
parameters :		sc_buf:Buffer*, takes the input buffer as parameter which reads the file
Return value :		int, returns 0 on success
Algorithm :

***********************************************************************************/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/***********************************************************************************

Purpose :			performs token recognizations and catches the tokens along with
errors if encountered
Author :			Sanket Patel & Yasser Noor
History/Version :	1.0
Called functions :	b_getc(), b_retract(),b_retract_to_mark(),b_setmark(),b_mark(),
b_getcoffset(), isalpha(), isdigit(), get_next_state(), b_create(),
b_addc(), b_cbhead(), b_free(),
parameters :		sc_buf: Buffer*, pointer to the buffer containg the read program
Return value :		Token, returns a token with the possible information of a valid token or
an error
Algorithm :			- loops until SEOF is encountered in the buffer
- gets the character one by	one
- compares character with various possiblities, to produce a valid token
- also checks for error, if found produces error token

***********************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
	short i; /* loop counter for various purpose */
	short j;

	/* endless loop broken by token returns it will generate a warning */
	while (1){
		//printf("Line Number : %d\n", line);
		/* Get the next symbol from the input buffer */
		c = b_getc(sc_buf);

		/* check if its end of file, if yes break the loop to stop reading characters*/
		if (c == SEOF || c == '\0') {
			t.code = SEOF_T;
			break;
		}

		/* check if its End of Line, then increase the line count and continue*/
		if (c == EOL){
			line++;
			continue;
		}

		/* if white space or tab continue to get the next character in the loop*/
		if (isspace(c))
			continue;

		/*check for Right brace*/
		if (c == '{'){
			t.code = LBR_T;
			return t;
		}
		/*check for Left brace*/
		if (c == '}'){
			t.code = RBR_T;
			return t;
		}

		/************************************************************
		Check for arithmetic operators
		*************************************************************/
		/* check for plus sign*/
		if (c == '+'){
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}

		/* check for Minus sign*/
		if (c == '-'){
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}

		/* check for multiply sign*/
		if (c == '*'){
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}

		/* check for division sign*/
		if (c == '/'){
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}


		/************************************************************
		Process comma
		************************************************************/
		if (c == ','){
			t.code = COM_T;
			return t;
		}

		/************************************************************
		Process semi colon
		************************************************************/
		if (c == ';'){
			t.code = EOS_T;
			return t;
		}

		/************************************************************
		Process right parenthesis
		************************************************************/
		if (c == '('){
			t.code = LPR_T;
			return t;
		}

		/************************************************************
		Process left parenthesis
		************************************************************/
		if (c == ')'){
			t.code = RPR_T;
			return t;
		}

		/************************************************************
		Process assignment operator and equal to relational operator
		check for first equal sign, then check for second equal sign, if
		found its an relational equal to operator other wise assignment operator
		************************************************************/
		if (c == '='){
			if (b_getc(sc_buf) == '='){
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			/*
			* Retract the next character that was read
			* if it was not equal to sign, because it was not
			* the equal to operator
			*/
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		}

		/************************************************************
		Process relational operator token : ">: greater than
		************************************************************/
		if (c == '>'){
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}

		/************************************************************
		Process relational operator token : ">" : less than and "<>" : not equal to
		************************************************************/
		if (c == '<'){
			if (b_getc(sc_buf) == '>'){
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			/*
			* Retract the next character that was read
			* if it was not greater than, because it was not
			* the not equal to realtional operator
			*/
			b_retract(sc_buf);
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			return t;
		}

		/************************************************************
		Process string concatenation operator : "#"
		************************************************************/
		if (c == '#'){
			t.code = SCC_OP_T;
			return t;
		}


		/***********************************************************************
		Chek for .AND. and .OR.
		WHAT TO RETURN - STILL A DOUBT
		**************************************************************************/
		if (c == '.'){
			i = 0;
			t.attribute.err_lex[0] = c;
			/*
			* check for the rest of characters in ".AND.",
			*
			* check till all characters match, if they match generate a
			* relational operator token and return it
			*
			* While scanning for ".AND." and ".OR.", store the character that was
			* scanned as attribute or token in err_lex, that could be used if
			* error was found while scanning
			*
			* else if one of the character is missing then generate an
			* error token and return it
			*/
			c = b_getc(sc_buf);
			//t.attribute.err_lex[i++] = c;
			/* Check for ".AND." */
			i++;
			if (c == 'A'){
				i++;
				if (b_getc(sc_buf) == 'N'){
					i++;
					if (b_getc(sc_buf) == 'D'){
						i++;
						if (b_getc(sc_buf) == '.'){
							/* if .AND. matches then generate relatopnal operator token*/
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
				/*
				* Retract all the characters we read, as "." was
				* considered an error token so, to start reading after the
				* dot
				*/
				while (i > 0){
					b_retract(sc_buf);
					i--;
				}

				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;
				return t;
			}
			/* Check for ".OR." */
			else if (c == 'O') {
				i++;
				if (b_getc(sc_buf) == 'R'){
					i++;
					if (b_getc(sc_buf) == '.'){
						/* if .OR. matches then generate relatopnal operator token*/
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
				/*
				* Retract all the characters we read, as "." was
				* considered an error token so, to start reading after the
				* dot
				*/
				while (i > 0){
					b_retract(sc_buf);
					i--;
				}

				/* if the above conditions dosent match, then return error token */
				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.attribute.err_lex[1] = '\0';
				t.code = ERR_T;	/*SEOF BEFORE ERR_T, if error comes*/
				return t;
			}
		}

		/*****************************************************************
		check for comments
		******************************************************************/
		/*
		* if the comment is found starting with "!<" then just skip the whole line
		* else store the two char "!" and the other one in err_lex, and skip the line
		*/
		if (c == '!'){
			if ((c = b_getc(sc_buf)) == '<'){
				while ((c = b_getc(sc_buf)) != EOL && c != SEOF){
					continue;
				}
				line++;
				continue;
			}
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';
				while ((c = b_getc(sc_buf)) != EOL){
					if (c == SEOF)
						return t;
					continue;
				}
				line++;
				return t;
			}

		} // end comments main if

		/*******************************************************
		* check for string literal
		*********************************************************/
		if (c == '"'){

			lexstart = b_getcoffset(sc_buf);
			b_setmark(sc_buf, lexstart);

			while (1){
				c = b_getc(sc_buf);

				if (c == EOL)
					line++;

				/*
				* If SEOF is found, produce an error token,
				* if the string is greater than 20 char, then grab
				* first 17 char and put 3 dots afterwards,other wise store the
				* 20 char, and dont forget to put '\0' in both cases
				*/
				else if (c == SEOF){

					lexend = b_getcoffset(sc_buf);
					b_retract_to_mark(sc_buf);
					b_retract(sc_buf);
					t.code = ERR_T;
					if ((lexend - lexstart) > 20){

						/*
						* copy the first 17 char in error lexeme
						* and then add three dots and '\0' at end
						*/
						for (i = 0; i < 17; i++)
							t.attribute.err_lex[i] = b_getc(sc_buf);
						t.attribute.err_lex[ERR_LEN - 3] = '.';
						t.attribute.err_lex[ERR_LEN - 2] = '.';
						t.attribute.err_lex[ERR_LEN - 1] = '.';
						t.attribute.err_lex[ERR_LEN] = '\0';
						/*
						* This would consume the characters
						* that are after 17, becuase they were more than
						* 20 chars
						*/
						while (i < lexend){
							//printf("consume\n");
							b_getc(sc_buf);
							i++;
						}

					}
					else {
						//printf("lexend - lexstart = %d\n", lexend - lexstart);
						j = 0;
						for (i = 0; i < (lexend - lexstart); i++){
							//	if ((c = b_getc(sc_buf)) == EOL)
							//	continue;
							t.attribute.err_lex[i] = b_getc(sc_buf);
						}
						t.attribute.err_lex[i] = '\0';
					}
					//b_retract(sc_buf);
					return t;
				}

				else if (c == '"'){
					/*
					* if the second double quote is found,
					* mark the end of the string and then
					* copy the whole string to string literal table
					* along with '\0' at the end, then generate
					* string token and return it
					*/
					/*
					* retract the last double quote, so as to mark the lexend
					* before the quote
					*/
					b_retract(sc_buf);
					lexend = b_getcoffset(sc_buf);
					b_retract_to_mark(sc_buf);
					t.code = STR_T;

					t.attribute.str_offset = b_size(str_LTBL);
					/*
					* copy the string literal to String literal table
					*/
					for (i = lexstart; i < lexend; i++)
						b_addc(str_LTBL, b_getc(sc_buf));

					b_addc(str_LTBL, '\0');
					/*
					* Get the double quote that was retracted above
					* to consume it
					*/
					b_getc(sc_buf);
					return t;
				}
			}
		}

		if (isalpha(c) || isdigit(c)){
			b_retract(sc_buf);
			lexstart = b_getcoffset(sc_buf);
			b_setmark(sc_buf, lexstart);
			c = b_getc(sc_buf);
			state = 0;
			/* get next state */
		FSM1:state = get_next_state(state, c, &accept);

			/*
			* Get the character only if its not accepting state and
			* Goto FSM1
			*/
			if (accept == NOAS){
				c = b_getc(sc_buf);
				/* if state is not accepting go to FSM1*/
				goto FSM1;
			}

			if (accept == ASWR)	// if the final state is retraccting final state
				b_retract(sc_buf);

			lexstart = b_mark(sc_buf);
			lexend = b_getcoffset(sc_buf);
			/*
			* size of buffer would be (lexstart - lexend) + 1
			* for adding end of file
			*/
			/*
			*	reports a run time error if any encountered during creation
			* of buffer
			*/
			if ((lex_buf = b_create((lexend - lexstart) + 1, 0, 'f')) == NULL){
				scerrnum = 1;
				aa_func11("Runtime error");
			}
			b_retract_to_mark(sc_buf);	// retract to mark

			for (i = lexstart; i < lexend; i++)
				b_addc(lex_buf, b_getc(sc_buf));

			b_addc(lex_buf, '\0');

			t = aa_table[state](b_cbhead(lex_buf));
			/*
			* check if the token attribute st_offset is -1
			* , if it is then exit with ST_FAIL, after the buffer
			* memory is freed, else continue the program
			*/
			b_free(lex_buf);
			//if (t.attribute.str_offset == ST_FAIL)
			//exit(ST_FAIL);
			return t;
		}

		t.code = ERR_T;
		t.attribute.err_lex[0] = c;
		t.attribute.err_lex[1] = '\0';
		return t;


	}

	return t;
}

/***********************************************************************************

Purpose :			gets the next state from the transition table and returns it to
the calling function
Author :			Svillen Ranev
History/Version :	1.0
Called functions :	char_class(),assert(), printf(), exit()
parameters :		state:int, takes the current state of FSM
c:char, takes the current character read from buffer
accept: int*, its the type of state
Return value :		int, returns the next state
Algorithm :

***********************************************************************************/

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	//printf("The returned from char classs : %d\n", col);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/***********************************************************************************

Purpose :			returns the column number of the transition table for a particular
character
Author :			Sanket Patel
History/Version :	1.0
Called functions :	isalpha(), isdigit()
parameters :		c: char, takes character from calling function
Return value :		int, returns the column number
Algorithm :


***********************************************************************************/
int char_class(char c)
{
	int val = 6;

	if (isalpha(c))
		return 0;

	if (c == '0')
		return 1;

	if (isdigit(c)){
		if (c == '8' || c == '9')
			return 3;
		return 2;
	}

	if (c == '.')
		return 4;

	if (c == '%')
		return 5;

	return val;
}

/* ASWR : VID : AVID/KW */
/* ASWR : VID : AVID/KW */
/***********************************************************************************

Purpose :			checks for a valid keyword and AVID
Author :			Sanket Patel
History/Version :	1.1
Called functions :	strcmp(),strlen(),
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- checks for the keywork in keywork table
- checks for AVID
- if nothing is valid, returns error token


***********************************************************************************/
Token aa_func02(char lexeme[]){
	Token t;	/* stores the returning token */
	int i;		/* loop counter variabe*/
	int lengthOfLexeme = strlen(lexeme);
	char temp_lexeme[VID_LEN + 1];
	/**************************
	Check for keyword first
	**************************/
	for (i = 0; i < KWT_SIZE; i++){
		if (strcmp(lexeme, kw_table[i]) == 0){
			t.code = KW_T;
			t.attribute.kwt_idx = i;
			return t;
		}
	}

	/*******************************
	Check for AVID
	******************************/
	/*
	* If AVID length is greater than 8 just copy
	* lexeme in to vid_lex attribute of only VID_LENGTH-1, adding '\0' at end
	*/
	if (lengthOfLexeme > VID_LEN){
		for (i = 0; i < VID_LEN; i++)
			temp_lexeme[i] = lexeme[i];
		temp_lexeme[VID_LEN] = '\0';

	}
	else{

		/*
		* If AVID length is less than 8 just copy
		* lexeme in to vid_lex attribute, adding '\0' at end
		*/
		for (i = 0; i < (signed)lengthOfLexeme; i++)
			temp_lexeme[i] = lexeme[i];
		temp_lexeme[i] = '\0';
	}
	/*
	* depending on the first char of lexeme decide the default type for the data type for it,
	* and call the st install function to install new entry
	*
	* if the st_install function returns -1, that means there is some problem in symbol table
	* so print a message and store the symbol table in a file calling st_store function,
	*
	* then exit the program, but that was the flaw in the program design, that is if we exit here
	* the lex_buffer allocated in the fuction where this function was called, wont be freed,
	*
	* To overcome this problem, we return the token to  the previous function, allowing it to
	* first free the lex buffer, and then exit there checking the token attribut vid_offset, which
	* would be -1 if there is problem in vid_offset
	*/
	if (temp_lexeme[0] == 'i' || temp_lexeme[0] == 'o' || temp_lexeme[0] == 'd' || temp_lexeme[0] == 'w')
		t.attribute.vid_offset = st_install(sym_table, &temp_lexeme[0], INTEGER_CONST, line);
	else
		t.attribute.vid_offset = st_install(sym_table, &temp_lexeme[0], FLOAT_CONST, line);

	if (t.attribute.vid_offset == ST_FAIL){
		printf("\nError: The Symbol Table is full - install failed.\n\n");
		st_store(sym_table);
		b_free(lex_buf);
		exit(ST_FAIL);
	}

	t.code = AVID_T;
	return t;
}

/* ASNR : VID : SVID */
/***********************************************************************************

Purpose :			checks for a valid SVID
Author :			Sanket Patel
History/Version :	1.1
Called functions :	strlen()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- copys the SVID to the returning token attribute
- considers only the first VID_LEN char of lexeme


***********************************************************************************/
Token aa_func03(char *lexeme){

	Token t;	/* stores the returning token */
	int i;		/* loop counter variable */
	int lengthOfLexeme = strlen(lexeme);
	char temp_lexeme[VID_LEN + 1];

	t.code = SVID_T;
	/*
	* If SVID length is greater than 8 just copy
	* first 7 in to the vid_lex attribute adding '%' and '\0' at end
	*/
	if (lengthOfLexeme > VID_LEN){
		for (i = 0; i < VID_LEN - 1; i++)
			temp_lexeme[i] = lexeme[i];
		temp_lexeme[VID_LEN - 1] = '%';
		temp_lexeme[VID_LEN] = '\0';
	}
	else {

		/*
		* If AVID length is les than 8 just copy
		* lexeme in to vid_lex attribute, adding '\0' at end
		*/
		for (i = 0; i < (signed)lengthOfLexeme; i++)
			temp_lexeme[i] = lexeme[i];
		temp_lexeme[i] = '\0';
	}

	t.attribute.vid_offset = st_install(sym_table, &temp_lexeme[0], STRING_CONST, line);

	/*
	* depending on the first char of lexeme decide the default type for the data type for it,
	* and call the st install function to install new entry
	*
	* if the st_install function returns -1, that means there is some problem in symbol table
	* so print a message and store the symbol table in a file calling st_store function,
	*
	* then exit the program, but that was the flaw in the program design, that is if we exit here
	* the lex_buffer allocated in the fuction where this function was called, wont be freed,
	*
	* To overcome this problem, we return the token to  the previous function, allowing it to
	* first free the lex buffer, and then exit there checking the token attribut vid_offset, which
	* would be -1 if there is problem in vid_offset
	*/
	if (t.attribute.vid_offset == ST_FAIL){
		printf("\nError: The Symbol Table is full - install failed.\n\n");
		st_store(sym_table);
		b_free(lex_buf);
		exit(ST_FAIL);
	}

	t.code = SVID_T;
	return t;
}

/* ASWR : DIL */
/**********************************************************************************

Purpose :			checks for DIL
Author :			Yasser Noor
History/Version :	1.0
Called functions :	atol(), aa_func11()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- checks if integer literal is in range
- if out of range produces error token, otherwise creates INL_T token

***********************************************************************************/
Token aa_func05(char *lexeme){
	long number;	/* stores the converted number from string */
	Token t;		/* returns the produced token */
	number = atol(lexeme);
	/*
	* check if number is in range then create a token and return it
	*/
	if (number >= INTEGER_LITERAL_MIN && number <= INTERGER_LITERAL_MAX){
		t.code = INL_T;
		t.attribute.int_value = number;
		return t;
	}
	/*
	* If its not in range or for some reason produces error token
	* call aa_func_11, to produce error token and copy the error lexeme
	*/
	return aa_func11(lexeme);


}

/* ASWR : FPL */
/***********************************************************************************

Purpose :			checks for FPL
Author :			Sanket Patel
History/Version :	1.0
Called functions :	atof(), aa_func11()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- checks if Floting literal is in range
- if out of range produces error token, otherwise creates FPL_T token

***********************************************************************************/
Token aa_func08(char *lexeme){
	Token t;		/* returns the produced token */
	double number;	/* stores the converted number from string */

	/* convert lexeme to double */
	number = atof(lexeme);

	if (number == 0 || (number >= FLT_MIN && number <= FLT_MAX)){  /* Check which constants to use for max and min*/
		t.code = FPL_T;
		t.attribute.flt_value = (float)number;
		return t;
	}
	/*
	* If its not in range or for some reason produces error token
	* call aa_func_11, to produce error token and copy the error lexeme
	*/
	return aa_func11(lexeme);
}

/* ASWR : OIL */
/***********************************************************************************

Purpose :			checks for OIL
Author :			Sanket Patel
History/Version :	1.0
Called functions :	atool(), aa_func11()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- checks if Octal Integer literal is in range
- if out of range produces error token, otherwise creates INL_T token

***********************************************************************************/
Token aa_func10(char *lexeme) {
	Token t;		/* returns the produced token */
	long number;	/* stores the converted number from string */

	number = atool(lexeme);

	if (number >= INTEGER_LITERAL_MIN && number <= INTERGER_LITERAL_MAX){
		t.code = INL_T;
		t.attribute.int_value = number;
		return t;
	}

	/*
	* If its not in range or for some reason produces error token
	* call aa_func_11, to produce error token and copy the error lexeme
	*/
	return aa_func11(lexeme);

}
/* ASNR : ES */
/***********************************************************************************

Purpose :			produces error state token
Author :			Yasser Noor
History/Version :	1.0
Called functions :	strlen()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		Token, returns a valid or error token
Algorithm :			- produces an error token
- copies only 20 characters if the lexeme is larger than it

***********************************************************************************/
Token aa_func11(char *lexeme){
	Token t;
	int lexeme_length, i;
	t.code = ERR_T;

	/*
	* When number not in range, generate a error token
	* if the lexeme greate the ERR_LEN, then copy first 20 char
	* else, if its less than ERR_LEN, then copy till the characters are there,
	* then add SEOF at the end in both cases
	*/
	lexeme_length = strlen(lexeme);
	t.code = ERR_T;
	if (lexeme_length > ERR_LEN){
		for (i = 0; i < ERR_LEN; i++)
			t.attribute.err_lex[i] = lexeme[i];
		t.attribute.err_lex[ERR_LEN] = '\0';
		return t;
	}
	for (i = 0; i < lexeme_length; i++)
		t.attribute.err_lex[i] = lexeme[i];
	t.attribute.err_lex[i] = '\0';
	return t;

}

/***********************************************************************************

Purpose :			converts string to Octal Integer literal
Author :			Yasser Noor
History/Version :	1.0
Called functions :	atol(), strlen(), pow()
parameters :		lexeme: char[], takes the stream of character to produce token
Return value :		long, returns converted number
Algorithm :

***********************************************************************************/
long atool(char * lexeme){

	int i;	/*loop counter*/
	/*
	* decimal_number : stores the decimal form from lexeme
	* octal_number : stores the converted octal number
	*/
	long decimal_number, octal_number, temp;
	decimal_number = octal_number = temp = 0;

	octal_number = atol(lexeme);

	for (i = 0; i < (signed)strlen(lexeme) - 1; i++){
		temp = octal_number % 10;
		octal_number = octal_number / 10;
		decimal_number += temp * (long)pow(8, i);
	}

	return decimal_number;

}
