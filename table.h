/*****************************************************************************
File Name: table.h
Compiler: Visual Studio 2013 Compiler
Author: Brandon Keohane & Gabriel Bourget
Course: Compilers - CST8152
Assignment: The Scanner - Assignment02
Date: October 27th, 2016
Professor: Svillen Ranev
Purpose: Initializes the transition table for the finite state machine
Function List: aa_func02(char *lexeme), aa_func03(char *lexeme),
			   aa_func05(char *lexeme), aa_func08(char *lexeme),
			   aa_func11(char *lexeme), Token aa_func12(char *lexeme)
*****************************************************************************/

#ifndef TABLE_H_
#define TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define ES  12   /* Error state */
#define IS  13   /* Invalid state */

/* State transition table definition */

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* |--------------------------------------------------------------------------------------| */
	/* |----------||----------------------INPUT SYMBOLS-----------------------||--------------| */
	/* |Currstate ||    [a-zA-Z]   0     [1-7]  [8-9]    .     %     other    || State Type   | */
	/* |--------------------------------------------------------------------------------------| */
	/* |   0      */{		1,     6,	   4,	  4,	ES,	   ES,	  ES   }, /*    NOAS      | */
	/* |   1      */{		1,	   1,	   1,	  1,	 2,		3,	   2   }, /*    NOAS      | */
	/* |   2      */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /* AVID/KW/ASWR | */
	/* |   3      */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*  SVID/ASNR   | */
	/* |   4      */{	   ES,	   4,	   4,	  4,	 7,		5,	   5   }, /*    NOAS      | */
	/* |   5      */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*   DIL/ASWR   | */
	/* |   6      */{	   ES,	  ES,	  10,	 ES,	 7,	   ES,	   5   }, /*    NOAS      | */
	/* |   7      */{		8,	   7,	   7,	  7,	 8,		8,	   8   }, /*    NOAS      | */
	/* |   8      */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*   FPL/ASWR   | */
	/* |   9      */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*    NOAS      | */
	/* |   10     */{	   11,	  10,	  10,	 ES,	ES,	   11,	  11   }, /*    NOAS      | */
	/* |   11     */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*   OIL/ASWR   | */
	/* |   12     */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   }, /*   ES/ASNR    | */
	/* |   13     */{	   IS,	  IS,	  IS,	 IS,	IS,	   IS,	  IS   } /*   ES/ASWR    | */
	/* ---------------------------------------------------------------------------------------| */

};

/* Accepting state table definition */
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     3  /* not accepting state */

int as_table[] = {
	NOAS, /* State 0  */
	NOAS, /* State 1  */
	ASWR, /* State 2  */
	ASNR, /* State 3  */
	NOAS, /* State 4  */
	ASWR, /* State 5  */
	NOAS, /* State 6  */
	NOAS, /* State 7  */
	ASWR, /* State 8  */
	NOAS, /* State 9  */
	NOAS, /* State 10 */
	ASWR, /* State 11 */
	ASNR, /* State 12 */
	ASWR /* State 13 */
};

/* Accepting action function declarations */

Token aa_func02(char *lexeme); /* AVID / KW */
Token aa_func03(char *lexeme); /* SVID */
Token aa_func05(char *lexeme); /* DIL  */
Token aa_func08(char *lexeme); /* FPL */
Token aa_func11(char *lexeme); /* OIL */
Token aa_func12(char *lexeme); /* ERR_TOK ASNR/ASWR */

typedef Token(*PTR_AAF)(char *lexeme);

PTR_AAF aa_table[] = {
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	NULL,
	aa_func11,
	aa_func12
};

#define KWT_SIZE  8

char * kw_table[] = {
	"ELSE", /* 0 */
	"IF", /* 1 */
	"INPUT", /* 2 */
	"OUTPUT", /* 3 */
	"PLATYPUS", /* 4 */
	"REPEAT", /* 5 */
	"THEN", /* 6 */
	"USING" /* 7 */
};

#endif
