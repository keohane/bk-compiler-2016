/*****************************************************************************
File Name: scanner.c
Compiler: Visual Studio 2013 Compiler
Author: Brandon Keohane & Gabriel Bourget
Course: Computer Engineering Technology
Assignment: Assignment 2 - The Scanner
Date: October 27th, 2016
Professor: Svillen Ranev
Purpose: Implements a lexical scanner in a compiler
Function List: char_class(), get_next_state(), iskeyword(), atool(),
			   isValidInteger(), isLetter(), isDigit()
*****************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS
#define _CRTDBG_MAP_ALLOC

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef TABLE_H_
#include "table.h"
#endif

#ifndef STABLE_H_
#include "stable.h"
#endif

#define DEBUG  /* for conditional processing */
#undef DEBUG

/* Global objects - variables */
extern Buffer * str_LTBL;						/* String literal table */
int line;										/* Current line number of the source code */
extern int scerrnum;							/* Defined in platy_st.c - run-time error number */
extern STD sym_table;

/* Local(file) global objects - variables */
static Buffer *lex_buf;							/* Pointer to temporary lexeme buffer*/

/* scanner.c static(local) function  prototypes */
static int char_class(char c);					/* Character class function */
static int get_next_state(int, char, int *);	/* State machine function */
static int iskeyword(char * kw_lexeme);			/* Keywords lookup functuion */
static long atool(char * lexeme);				/* Converts octal string to decimal value */
static int isValidInteger(char * lexeme);		/* Checks if lexeme is a valid int */
static int isLetter(unsigned char);				/* Checks if the character passed is a letter */
static int isDigit(unsigned char);				/* Checks if the character passed is a digit */

/*****************************************************************************
Purpose: Initializes this scanner
Author: Svillen Ranev
History/Version: v1.0
Called Functions: b_isempty, b_setmark, b_retract_to_mark, b_reset.
Parameters: Buffer * sc_buf - pointer to buffer
Return Value: Returns if initialization was a success
Algorithm:
		-> If buffer is empty dont bother scanning return error
		-> Initialize the mark, reset all parameters, and initialize line numbers
*****************************************************************************/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;
}


/***********************************mlwpar_next_token()**************************************
Purpose: Processes the input stream of characters, separating them into distinct lexemes
		 when the rules set out in the function recognize it as a pre-defined pattern.
		 Part of the processing is token driven, where patterns are recognized one at a
		 time, and the other part is transition table driven. Once a pattern is found,
		 key attributes and parameters about the pattern are recorded in a Token structure,
		 and the Token is returned.
Authors: Gabriel Bourget and Brandon Keohane
History/Version: v1.0
Called Functions: b_getc, b_retract, b_setmark, b_getcoffset, b_retract_to_mark,
				  b_size, b_addc, get_next_state, b_mark, b_create, b_free
Parameters: -> sc_buf (input buffer)
			   type: BufferDescriptor
Return Value: Token (representing the corresponding token that was recognized)
Algorithm:
			--------------------------------TOKEN DRIVEN -------------------------------------

			-> Character a newspace character? Increase line number, and continue.
			-> Character a whitespace or separator character?
				-> Yes: Assign token parameters and return.
			-> Character a concatenation, assignment, arithmetic or conditional operator?
				-> Yes: Assign token parameters and return.
			-> Character a '.' ?
				-> Yes: Go into token processing for logical operators to determine if the
						lexeme is a .AND. or .OR. operator.
			-> Character a ' " ' ?
				-> Yes: Go into token processing for strings
					-> If illegal string, process in appropriate way and return error token.
					-> If legal string, add it to string literal table buffer.

			---------------------------- TRANSITION TABLE DRIVEN -----------------------------

			-> Character is [a-zA-Z] or [0-9]?
				|------------------------|
				|- FINITE STATE MACHINE -|
				|------------------------|
				|-----------------------------------------------------------------------------|
				|-> Set state to 0.                                                           |
				|-> 1. Get the next state by calling get_next_state().
				|-> 2. Check if the state is NOAS
				|	-> If yes skip loop and go to accepting state
				|	-> Else enter loop and continue to #3
				|-> 3. Get the next character using b_getc().                                 |
				|-> 4. If the stateType is NOAS, go back up to step 1. If it's anything else, |
				|      leave the FSM and call an accepting state function based on what state |
				|      you're in.                                                             |
				|-----------------------------------------------------------------------------|
				-> If state is accepting with retract, retract the buffer using b_retract().
				-> Create temp buffer that is 1 more character in size than recorded lexeme.
					-> If there's a run time error in creating the buffer, handle it here.
				-> Retract getc_offset to the mark that was previously set at beginning of
				   lexeme.
				-> Copy lexeme from lexstart to lexend from input buffer into temp buffer using
				   b_addc() and add '\0' to make buffer a c type string.
				-> Call acceptor state function from callback table depending on what state
				   you're in.
			-> For any other character, generate an error token consisting of the erroneous
			   character, followed by a '\0' to make it a c-style string and return the token.
*********************************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t;			/* token to return after recognition */
	unsigned char c;	/* input symbol */
	int state = 0;		/* initial state of the FSM */
	short lexstart;		/* start offset of a lexeme in the input buffer */
	short lexend;		/* end   offset of a lexeme in the input buffer */
	int accept = NOAS;	/* type of state - initially not accepting */
	int index = 0;		/* Index of loop */
	char* lexbuf;		/* Lexeme to pass to the accepting functions */

	if (sc_buf == NULL){
		t.code = ERR_T;
		strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
		scerrnum = 400; /* Random error value to check in main */
		return t;
	}

	/* Will have warning for being constant */
	while (1){ /* endless loop broken by token returns it will generate a warning */

		/* Get next symbol */
		c = b_getc(sc_buf);

		/* Special case switch */
		switch (c){
			/* EOF cases */
			case 255: case '\0': case '0xFF': case EOF: t.code = SEOF_T; return t;
			/* Space case - ignore and continue */
			case ' ': continue;
			/* Horizontal tab case - ignore and continue */
			case '\t': continue;
			/* Vertical tab case - ignore and continue */
			case '\v': continue;
			/* New line increment - ignore and continue */
			case '\n': line++; continue;
			/* Right parenthesis token */
			case ')': t.code = RPR_T; return t;
			/* Left parenthesis token */
			case '(': t.code = LPR_T; return t;
			/* Left brace token */
			case '{': t.code = LBR_T; return t;
			/* Right brace token */
			case '}': t.code = RBR_T; return t;
			/* Comma token */
			case ',': t.code = COM_T; return t;
			/* Semi colon token */
			case ';': t.code = EOS_T; return t;
			/* String concatination token */
			case '#': t.code = SCC_OP_T; return t;
			/* Other case */
			default: break;
		}

		/****************************************************************
		*						Arithmetic Token					    *
		****************************************************************/
		/* Addition operator token */
		if (c == '+') { t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; }
		/* Subtraction operator token */
		if (c == '-') { t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; }
		/* Multiplication operator token */
		if (c == '*') { t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; }
		/* Division operator token */
		if (c == '/') { t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; }

		/****************************************************************
		*						Relational Token					    *
		****************************************************************/
		/* Greater than operator token */
		if (c == '>') { t.code = REL_OP_T; t.attribute.rel_op = GT; return t; }
		if (c == '<') {
			c = b_getc(sc_buf);
			/* Not equal operator token */
			if (c == '>'){
				t.code = REL_OP_T; t.attribute.rel_op = NE; return t;
			}
			/* Less than operator token */
			else {
				b_retract(sc_buf);
				t.code = REL_OP_T; t.attribute.rel_op = LT; return t;
			}
		}
		if (c == '=') {
			c = b_getc(sc_buf);
			/* Equals relational token */
			if (c == '=') {
				t.code = REL_OP_T; t.attribute.rel_op = EQ; return t;
			}
			/* Assignment operator token */
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T; return t;
			}
		} /* End relational tokens if */

		/****************************************************************
		*						  Logical Token					        *
		****************************************************************/
		/* And or OR operator token */
		if (c == '.'){
			/* Set mark to current get c offset position */
			b_setmark(sc_buf, b_getcoffset(sc_buf));
			/* Check for AND operator */
			if ((c = b_getc(sc_buf)) == 'A'){
				if ((c = b_getc(sc_buf)) == 'N' && (c = b_getc(sc_buf)) == 'D' && (c = b_getc(sc_buf)) == '.'){
					t.code = LOG_OP_T; t.attribute.log_op = AND; return t;
				}
				else {
					/* Retract to mark at beginning '.' */
					b_retract_to_mark(sc_buf);
					/* Get '.' again */
					b_retract(sc_buf);
					/* Create error token */
					c = b_getc(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					return t;
				}
			}
			/* Check for OR operator */
			else if (c == 'O'){
				if ((c = b_getc(sc_buf)) == 'R' && (c = b_getc(sc_buf)) == '.'){
					t.code = LOG_OP_T; t.attribute.log_op = OR; return t;
				}
				else {
					/* Retract to mark at beginning '.' */
					b_retract_to_mark(sc_buf);
					/* Get '.' again */
					b_retract(sc_buf);
					/* Create error token */
					c = b_getc(sc_buf);
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					return t;
				}
			}
			/* No 'A' or 'O' */
			else {
				/* Retract to mark at beginning '.' */
				b_retract_to_mark(sc_buf);
				/* Get '.' again */
				b_retract(sc_buf);
				/* Create error token */
				c = b_getc(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				return t;
			}
		} /* End logical tokens if */

		/****************************************************************
		*						  Comment Token					        *
		****************************************************************/
		if (c == '!'){
			/* Set mark to current get c offset position */
			b_setmark(sc_buf, b_getcoffset(sc_buf));
			/* Get next character */
			c = b_getc(sc_buf);
			if (c == '<'){
				/* Loop till new line character is found */
				while (c != '\n'){
					/* Get next character */
					c = b_getc(sc_buf);
					/* Check for end of file */
					if (c == 255 || c == '\0'){
						/* Retract to mark at beginning */
						b_retract_to_mark(sc_buf);
						/* Create error token */
						t.code = ERR_T;
						t.attribute.err_lex[0] = c;
						t.attribute.err_lex[1] = '\0';
						return t;
					}
				}
				line++;
				/* Ignore comments and continue */
				continue;
			}
			else {
				/* Create error token */
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = '\0';

				/* Loop till new line character is found to skip all comment characters */
				while (c != '\n'){
					/* Get next character */
					c = b_getc(sc_buf);
					/* Check for end of file */
					if (c == 255){
						/* Retract to mark at beginning */
						b_retract_to_mark(sc_buf);
						/* Create error token */
						t.code = ERR_T;
						t.attribute.err_lex[0] = c;
						t.attribute.err_lex[1] = '\0';
						return t;
					}
				}
				line++;

				/* At new content for compiler to read, return error token created */
				return t;
			}
		} /* End comment tokens if */

		/****************************************************************
		*						   String Token						    *
		****************************************************************/
		if (c == '"'){
			/* Set mark to '"' for error purposes */
			b_setmark(sc_buf, b_getcoffset(sc_buf));
			/* Loop and check for next '"' */
			/* If error aka 255 was found create error token and return */
			
			do {
				/* Get next character and check if its valid */
				c = b_getc(sc_buf);

				if (c == '\n') line++;

				/* If end of file token */
				if (c == 255 || c == '\0'){
					/* Set error limit values */
					lexstart = b_mark(sc_buf);
					lexend = b_getcoffset(sc_buf) - 1;
					/* Retract to mark */
					b_retract_to_mark(sc_buf);
					/* Retract back one to quote */
					b_retract(sc_buf);
					/* Create error token */
					t.code = ERR_T;

					/* Fill error token with size of error length */
					if ((lexend - lexstart) > ERR_LEN){
						for (index = 0; index < ERR_LEN; ++index){
							c = b_getc(sc_buf);
							t.attribute.err_lex[index] = c;
						}
						/* Fill last three characters with ... */
						t.attribute.err_lex[ERR_LEN - 3] = '.';
						t.attribute.err_lex[ERR_LEN - 2] = '.';
						t.attribute.err_lex[ERR_LEN - 1] = '.';
						t.attribute.err_lex[ERR_LEN] = '\0';

						/* Loop till the error symbol */
						while ((c == 255 || c == '\0' || c == '0xFF' || c == EOF) == 0){
							c = b_getc(sc_buf);
						}

						/* Return error token */
						return t;
					}
					/* Not at limit for error length so fill what we have */
					for (index = 0; index < lexend - lexstart; ++index){
						t.attribute.err_lex[index] = b_getc(sc_buf);
					}
					/* Make c type string */
					t.attribute.err_lex[lexend - lexstart] = '\0';
					/* Return error token */
					return t;
				}

			} while (c != 34);

			/* If broken from while that means that an ending '"' was found */

			/* Set calculation variables */
			/* lexstart = '"' */
			/* lexend   = '"' */
			lexstart = b_mark(sc_buf);
			lexend = b_getcoffset(sc_buf) - 1;

			/* Set token code and attribute */
			t.code = STR_T;
			t.attribute.str_offset = b_size(str_LTBL);

			/* Empty string */
			if ((lexend - lexstart) <= 1){
				/* Make the string in the string literal table a c type string */
				b_addc(str_LTBL, '\0');
				return t;
			}
			/* Retract to mark and copy string into str_table */
			b_retract_to_mark(sc_buf);
			for (index = lexstart; index < lexend; ++index){
				c = b_getc(sc_buf);
				b_addc(str_LTBL, c);
			}

			/* Loop brings the buffer to the end of the string now get end '"' aswell */
			c = b_getc(sc_buf);

			/* Make the string in the string literal table a c type string */
			b_addc(str_LTBL, '\0');

			return t;
		} /* End string token if */

		/****************************************************************
		*					   Finite State Machine						*
		****************************************************************/
		if (isDigit(c) || isLetter(c)){
			/* Set mark to current lexeme position */
			b_setmark(sc_buf, b_getcoffset(sc_buf) - 1);
			/* Start state at 0 */
			state = get_next_state(state, c, &accept);
			/* Loop while transitioning states */
			while (accept == NOAS){
				/* Get next character */
				c = b_getc(sc_buf);
				/* Get next state from transition table */
				state = get_next_state(state, c, &accept);
				/* Check if state is non accepting state */
			}
			/* If accepting state with retract */
			/* Ask prof if this is the correct SOLUTION */
			if (accept == ASWR){
				b_retract(sc_buf);
			}
			/* Set the beggining and end points of lexeme */
			lexend = b_getcoffset(sc_buf);
			lexstart = b_mark(sc_buf);
			/* Create temp lexeme buffer */
			if ((lex_buf = b_create(lexend - lexstart + 1, 0, 'f')) == NULL){
				t.code = ERR_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				scerrnum = 500; /* Random error value to check in main */
				return t;
			}
			/* 
			Allocate the lexeme to be passed to the accepting functions...
			This is used because when the symbol table is full it prints and exits,
			this will allow the buffer to be destroyed here in the finite state machine 
			before you pass the lexeme you clean up the buffer memory before you send 
			the lexeme inside the accepting function whenever that case happens.
			*/
			if ((lexbuf = (char*)malloc((lexend - lexstart + 1) * sizeof(char))) == NULL){
				t.code = ERR_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				scerrnum = 400; /* Random error value to check in main */
				return t;
			}
			/* Retract mark to begining and copy lexeme into temp lex */
			b_retract_to_mark(sc_buf);
			/* Loop and copy */
			for (index = lexstart; index < lexend; ++index){
				c = b_getc(sc_buf);
				b_addc(lex_buf, c);
			}
			/* Make a c type string to know when the buffer stops when passed to state */
			b_addc(lex_buf, '\0');

			/* Copy lexeme into lex to pass to accepting functions */
			strncpy(lexbuf, b_cbhead(lex_buf), lexend - lexstart + 1);
			/* Delete buffer data */
			b_free(lex_buf);
			/* Place new lexeme in corresponding state */
			t = (*aa_table[state])(lexbuf);

			/* Free up lexeme */
			free(lexbuf);
			
			return t;
		}

		/* If it gets this far set error token */
		t.code = ERR_T;
		t.attribute.err_lex[0] = c;
		t.attribute.err_lex[1] = '\0';
		return t;
	}/* end while(1) */
}

/*****************************************************************************
Purpose: Calculates next state the scanner should move to in the transition table
Author: Svillen Ranev
History/Version: v1.0
Called Functions: char_class, assert
Parameters: state - is the current state of the transition table
			c - symbol to process
			*accept - holds the next function to call
Return Value: the index of the next function to use
Algorithm: -> Calculates the column from transition table
		   -> Retrieves state in which to move next
		   -> Stores the value in accept of what the state class is
		   -> Returns the index of next function to use
*****************************************************************************/
int get_next_state(int state, char c, int *accept){
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

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

/*****************************************************************************
Purpose: Checks if character is part of a corresponding column
Author: Brandon Keohane
History/Version: v1.0
Called Functions: isLetter
Parameters: char c -> Character to check to see the corresponding column
Return Value: Column number
Algorithm: -> Checks if letter, 0, 1-7, 8-9, '.', '%' or other and returns the
			  column that corresponds to the table.
*****************************************************************************/
int char_class(char c){
	/* Casts to unsigned character of character parameter */
	unsigned char sym = c;
	if (isLetter(sym)) return 0;
	if (sym == '0')	return 1;
	if (sym >= '1' && sym <= '7') return 2;
	if (sym == '8' || sym == '9') return 3;
	if (sym == '.') return 4;
	if (sym == '%') return 5;
	return 6;
}

/******************************aa_func02()************************************
Purpose: Accepting function for state 02 of the transition table. Processes the
		 lexeme into either a keyword or AVID token.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: iskeyword(), strlen(), strcpy(), strncpy()
Parameters: -> lexeme (input string)
type: character array
Return Value: Token (representing processed keyword or AVID token)
Algorithm: -> If lexeme comes back as one of the keywords, record the token's code
			  and proper attribute, and then return it.
		   -> Not a keyword...
			   -> If lexeme is longer than VID_LEN...
				   -> Copy its contents into a temp character array up to VID_LEN characters.
				   -> Add a terminating character '\0' to the end of the array.
				   -> Install the lexeme into the symbol table
						-> If could not be installed store the symbol table in a file and exit program
						-> Else record the token code and proper offset attribute, and return the token.
			   -> If lexeme is shorter or equal to VID_LEN
				   -> Copy the lexeme into the token attribute's vid_lex array.
				   -> Add a terminating character '\0' to the end of the array.
				   -> Install the lexeme into the symbol table
						-> If could not be installed store the symbol table in a file and exit program
						-> Else record the token code and proper offset attribute, and return the token.
*****************************************************************************/
Token aa_func02(char lexeme[]) {
	/* Final token to be returned */
	Token t; 
	/* Queries to see if lexeme is a keyword */
	int keywordNum = iskeyword(lexeme);
	/* Offset returned from install */
	int offset;
	/* VID lexeme copied as c type string */
	char vid_lex[VID_LEN + 1] = { '\0' };
	/* Character to compare for integer */
	char c;

	/* Lexeme ends up being a keyword */
	if (keywordNum >= 0) {
		t.code = KW_T;
		t.attribute.kwt_idx = keywordNum;
	}
	/* Lexeme is not a keyword */
	else {
		/* Get first character to compare if it is integer VID */
		c = lexeme[0];
		/* Lexeme longer than VID_LEN */
		if (strlen(lexeme) > VID_LEN) {
			strncpy(vid_lex, lexeme, VID_LEN);
			vid_lex[VID_LEN] = '\0';
			t.code = AVID_T;
		}
		/* Lexeme shorter or equal to VID_LEN */
		else {
			strcpy(vid_lex, lexeme);
			t.code = AVID_T;
		}

		/* Add Integer VID to symbol table */
		if (c == 'i' || c == 'o' || c == 'd' || c == 'w'){
			/* Install VID in symbol table */
			offset = st_install(sym_table, vid_lex, 'I', line);
		}
		/* Add float VID to symbol table */
		else {
			/* Install VID in symbol table */
			offset = st_install(sym_table, vid_lex, 'F', line);
		}

		/* If installing to symbol table was a failure */
		if (offset == -1){
			/* Print error statement */
			fprintf(stdout, "\nError: The Symbol Table is full - install failed.\n");
			/* Store the symbol table in a file */
			st_store(sym_table);
			/* Free the lexeme passed in parameter */
			free(lexeme);
			/* Returned exit failure */
			exit(EXIT_FAILURE);
		}
		t.attribute.vid_offset = offset;
	}
	return t;
}

/*****************************************************************************
Purpose: Accepting function for the string variable id
Author: Brandon Keohane
History/Version: v1.0
Called Functions: strlen, strncpy
Parameters: char[] lexeme - is character array of token
Return Value: Token returned by state representing a string vid
Algorithm: -> Checks if lexeme is greater than VID_LEN
		   -> If greater than VID_LEN...
				-> it sets token with corresponding attributes and code
				   with truncated string to VID_LEN and adds '%' and '\0' to
				   the end.
				-> Install the lexeme into the symbol table
				   -> If could not be installed store the symbol table in a file and exit program
				   -> Else record the token code and proper offset attribute, and return the token.
		   -> If less than or equal to VID_LEN...
				-> it copys lexeme into token and adds '\0' to the end
				-> Install the lexeme into the symbol table
				   -> If could not be installed store the symbol table in a file and exit program
				   -> Else record the token code and proper offset attribute, and return the token.
*****************************************************************************/
Token aa_func03(char lexeme[]){
	/* Token created, set, and returned */
	Token tken;
	/* Holds the lexeme length */
	int lexlen = strlen(lexeme);
	/* Offset returned from the install to symbol table */
	int offset;
	/* */
	char vid_lex[VID_LEN + 1] = { '\0' };

	/* Set SVID token attributes */
	tken.code = SVID_T;

	/* If lexeme's length is greater than max value */
	if (lexlen > VID_LEN){
		/* Copy lexeme to length of VID_LEN - 1 */
		strncpy(vid_lex, lexeme, (VID_LEN - 1));
		/* Add character to end to make it a c type string */
		vid_lex[VID_LEN - 1] = '%';
		vid_lex[VID_LEN] = '\0';
		/* Add string to symbol table */
		offset = st_install(sym_table, vid_lex, 'S', line);
	}
	else {
		/* Copy lexeme to length of lexeme */
		strncpy(vid_lex, lexeme, lexlen);
		/* Add character to make it a c type string */
		vid_lex[lexlen] = '\0';
		/* Add string to symbol table */
		offset = st_install(sym_table, vid_lex, 'S', line);
	}

	/* If installing to symbol table was a failure */
	if (offset == -1){
		/* Print error statement */
		fprintf(stdout, "\nError: The Symbol Table is full - install failed.\n");
		/* Store the symbol table in a file */
		st_store(sym_table);
		/* Free the lexeme passed in parameter */
		free(lexeme);
		/* Returned exit failure */
		exit(EXIT_FAILURE);
	}
	tken.attribute.vid_offset = offset;
	return tken;
}

/*****************************************************************************
Purpose: Accepting function for the integer literal, decimal constant, and ZERO
Author: Brandon Keohane
History/Version: v1.0
Called Functions: isValidInteger, atoi, strlen, strncpy
Parameters: char[] lexeme - is character array of token
Return Value: Token returned by state representing a string vid
Algorithm: -> Checks if lexeme is a valid integer
		   -> If it is valid integer...
				-> it sets token with corresponding integer attributes and code
		   -> If it is not a valid integer...
				-> it creates a error token with the truncated ERR_LEN length of
				   characters with '\0' at the end
		   -> Returns token
*****************************************************************************/
Token aa_func05(char lexeme[]){
	/* Token created, set, and returned to main */
	Token tken;
	/* Holds lexeme length */
	int lexlen;

	/* Valid integer */
	if (isValidInteger(lexeme)){
		/* Token code is integer */
		tken.code = INL_T;
		/* Set integer value to lexeme integer value */
		tken.attribute.int_value = atoi(lexeme);
	}
	/* Not valid integer */
	else {
		/* Get length of lexeme */
		lexlen = strlen(lexeme);
		/* Set error token value */
		tken.code = ERR_T;

		if (lexlen > ERR_LEN){
			/* Copy size of ERR_LEN from lexeme into error lexeme */
			strncpy(tken.attribute.err_lex, lexeme, ERR_LEN);
			/* Add character to make it a c type string */
			tken.attribute.err_lex[ERR_LEN] = '\0';
		}
		else {
			/* Copy lexeme into error lexeme */
			strncpy(tken.attribute.err_lex, lexeme, lexlen);
			/* Add character to make it a c type string */
			tken.attribute.err_lex[lexlen] = '\0';
		}
	}
	return tken;
}

/******************************aa_func08()************************************
Purpose: Accepting function for state 08 of the transition table. Processes the
		 lexeme into a float literal token.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: atof(), strlen(), strcpy(), strncpy()
Parameters: -> lexeme (input string)
type: character array
Return Value: Token (representing processed SVID token)
Algorithm: -> Float number out of float data type range...
				-> If lexeme is longer than ERR_LEN...
					-> Copy its contents into a temp character array up to ERR_LEN characters.
					-> Add a terminating character '\0' to the end of the array.
					-> Copy the overall AVID into the token attribute's vid_lex array.
					-> Record the token code and proper attribute, and return the token.
				-> If lexeme is shorter or equal to ERR_LEN...
					-> Copy the lexeme into the token attribute's vid_lex array.
					-> Add a terminating character '\0' to the end of the array.
					-> Add a '%' character to the second last character of the array.
					-> Record the token code and return the token.
		   -> Float number in range of float data type range...
				-> Record the token's code and proper attribute, and then return it.
*****************************************************************************/
Token aa_func08(char lexeme[]) {

	Token t; /* Final token to be returned */
	/* Hold converted value in double, in case of over/underflow */
	double fl_conversion = atof(lexeme);
	/* Holds the length of the lexeme */
	int lexLength = strlen(lexeme);

	/* Float value is out of float data type range */
	if ((fl_conversion < FLT_MIN && fl_conversion > 0.0) || fl_conversion > FLT_MAX) {
		/* Error lexeme longer than ERR_LEN */
		if (lexLength > ERR_LEN) {
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
			t.attribute.err_lex[ERR_LEN] = '\0';
			t.code = ERR_T;
		}
		/* Error lexeme shorter or equal to ERR_LEN */
		else {
			strcpy(t.attribute.err_lex, lexeme);
			t.attribute.err_lex[ERR_LEN] = '\0';
			t.code = ERR_T;
		}
	}
	/* Float value within float data type range */
	else {
		t.attribute.flt_value = (float)fl_conversion;
		t.code = FPL_T;
	}
	return t;
}

/******************************aa_func11()************************************
Purpose: Accepting function for state 11 of the transition table. Processes the
		 lexeme into an octal literal token.
Author: Gabriel Bourget
History/Version: v1.0
Called Functions: atool(), strlen(), strcpy(), strncpy()
Parameters: -> lexeme (input string)
type: character array
Return Value: Token (representing processed SVID token)
Algorithm: -> Octal number out of short data type range...
				-> If lexeme is longer than ERR_LEN...
					-> Copy its contents into a temp character array up to ERR_LEN characters.
					-> Add a terminating character '\0' to the end of the array.
					-> Copy the overall AVID into the token attribute's vid_lex array.
					-> Record the token code and proper attribute, and return the token.
				-> If lexeme is shorter or equal to ERR_LEN...
					-> Copy the lexeme into the token attribute's vid_lex array.
					-> Add a terminating character '\0' to the end of the array.
					-> Add a '%' character to the second last character of the array.
					-> Record the token code and return the token.
		   -> Octal number in range of short data type range...
				-> Record the token's code and proper attribute, and then return it.
*****************************************************************************/
Token aa_func11(char lexeme[]){

	Token t; /* Final token to be returned */
	/* Hold converted value in long int, in case of over/underflow */
	long oct_conversion = atool(lexeme); 

	/* Octal value is out of short data type range */
	if (oct_conversion < SHRT_MIN || oct_conversion > SHRT_MAX) {
		/* Error lexeme longer than ERR_LEN */
		if (strlen(lexeme) > ERR_LEN) {
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
			t.attribute.err_lex[ERR_LEN] = '\0';
			t.code = ERR_T;
		}
		/* Error lexeme shorter or equal to ERR_LEN */
		else {
			strcpy(t.attribute.err_lex, lexeme);
			t.attribute.err_lex[ERR_LEN] = '\0';
			t.code = ERR_T;
		}
	}
	/* Octal value within short data type range */
	else {
		t.attribute.int_value = (short)oct_conversion;
		t.code = INL_T; 
	}
	return t;
}

/*****************************************************************************
Purpose: Makes a error token from lexeme passed
Author: Brandon Keohane
History/Version: v1.0
Called Functions: strcpy
Parameters: char lexeme[] holds lexeme with error text
Return Value: Returns error token
Algorithm: -> Sets error token code to error code...
		   -> If lexeme length is greater than ERR_LEN...
				-> Copy ERR_LEN of characters into err_lex of union
				-> Make err lex a valid c string
		   -> Else lexeme length is less than ERR_LEN...
				-> Copy length of lexeme into err lex
				-> Make err lex a valid c string
		   -> Return token created
*****************************************************************************/
Token aa_func12(char lexeme[]){
	/* Token created and set to return to main */
	Token tken;
	/* Get length of lexeme */
	int lexlen = strlen(lexeme);
	/* Set error token value */
	tken.code = ERR_T;

	if (lexlen > ERR_LEN){
		/* Copy size of ERR_LEN from lexeme into error lexeme */
		strncpy(tken.attribute.err_lex, lexeme, ERR_LEN);
		/* Add character to make it a c type string */
		tken.attribute.err_lex[ERR_LEN] = '\0';
	}
	else {
		/* Copy lexeme into error lexeme */
		strncpy(tken.attribute.err_lex, lexeme, lexlen);
		/* Add character to make it a c type string */
		tken.attribute.err_lex[lexlen] = '\0';
	}
	return tken;
}

/*****************************************************************************
Purpose: Converts a string to octal to decimal
Author: Brandon Keohane
History/Version: v1.0
Called Functions: strlen, atoi, pow
Parameters: string containing octal number
Return Value: Long converted from octal string value
Algorithm: -> Converts lexeme from octal to integer
*****************************************************************************/
long atool(char * lexeme){
	return strtol(lexeme, NULL, 8);
}

/*****************************************************************************
Purpose: Checks lexeme against keyword table and notifies if it was found
Author: Brandon Keohane
History/Version: v1.0
Called Functions: strcmp
Parameters: char * kw_lexeme contains a string to check against keyword table
Return Value: Returns either the index of keyword, or R_FAIL1.
Algorithm: -> Loops and checks lexeme against keyword table and returns index 
			  if found and R_FAIL1 if not found.
*****************************************************************************/
int iskeyword(char * kw_lexeme){
	/* Index of keyword */
	int i;
	/* Loop and check if keyword at position equals lexeme */
	for (i = 0; i < KWT_SIZE; ++i){
		if (strcmp(kw_lexeme, kw_table[i]) == 0) return i;
	}
	return R_FAIL1;
}

/*****************************************************************************
Purpose: Checks if character is a valid character
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: char c -> Character to check if valid
Return Value: true 1, false 0
Algorithm: -> Checks if values are from 65 - 90 & 97 - 122
		   -> 65 = a to 90 = z
		   -> 97 = A to 122 = Z
*****************************************************************************/
int isLetter(unsigned char c){
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

/*****************************************************************************
Purpose: Checks if character is a valid digit
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: char c -> Character to check if valid
Return Value: true -> 1 false -> 0
Algorithm: -> Checks if values are from 0 - 9
*****************************************************************************/
int isDigit(unsigned char c){
	return c >= '0' && c <= '9';
}

/*****************************************************************************
Purpose: Checks if lexeme contains a valid integer number
Author: Brandon Keohane
History/Version: v1.0
Called Functions: atol
Parameters: char * lexeme contains integer number to check
Return Value: Returns 1 if valid number and 0 if not valid
Algorithm: -> Converts lexeme to long and checks if long number is greater than
			  short max or less than short min.
*****************************************************************************/
int isValidInteger(char * lexeme){
	/* Converted value of integer string */
	long conv = atol(lexeme);
	return conv <= SHRT_MAX && conv >= SHRT_MIN;
}
