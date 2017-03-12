/*****************************************************************************
File Name:		stable.h
Compiler:		Visual Studio 2013
Author:			Brandon Keohane
Course:			Compilers - CST8152
Assignment:		The Symbol Table - Assignment 3
Date:			24 November, 2016
Professor:		Svillen Ranev
Purpose:		Defines all datatypes and functions used by the symbol table
Function List:  st_create, st_install, st_lookup, st_update_type, 
				st_update_value, st_get_type, st_destroy, st_print,
				st_setsize, st_incoffset, st_store, st_sort
*****************************************************************************/
#ifndef STABLE_H_
#define STABLE_H_
/******************************* DEFINES ************************************/
#define ZERO			0x0000			/* 0000 0000 0000 0000 */
#define DEFAULT			0xFFF8			/* 1111 1111 1111 1000 */
#define SET_FLT			0x0002			/* 0000 0000 0000 0010 */
#define FLT_MASK		0x0002			/* 0000 0000 0000 0010 */
#define SET_INT			0x0004			/* 0000 0000 0000 0100 */
#define INT_MASK		0x0004			/* 0000 0000 0000 0100 */
#define SET_STRING		0x0006			/* 0000 0000 0000 0110 */
#define STRING_MASK		0x0006			/* 0000 0000 0000 0110 */
#define CHK_UPDATEF		0x0001			/* 0000 0000 0000 0001 */
#define SET_UPDATEF		0x0001			/* 0000 0000 0000 0001 */
#define RESET_UPDATEF	0xFFFE			/* 1111 1111 1111 1110 */
#define FILE_NAME		"$stable.ste"	/* File name to output to with store function */
#define DEFAULT_I		0				/* Default Integer value */
#define DEFAULT_F		0				/* Default Float value */
#define DEFAULT_S		-1				/* Default String value */
#define CHAR_INTEGER	'I'				/* Default character label integer value */
#define CHAR_FLOAT		'F'				/* Default character label float value */
#define CHAR_STRING		'S'				/* Default character label string value */
#define CHAR_ASCEND		'A'				/* Default character label ascending sort */
#define CHAR_DESCEND	'D'				/* Default character label descending sort */
#define R_STFAIL_1		-1				/* Symbol table fail constant */
/*****************************************************************************/

#ifndef BUFFER_H_
#include "buffer.h"
#endif

/******************************* TYPEDEFS ************************************/
typedef union InitialValue {
	int int_val;					/* Integer variable initial value */
	float fpl_val;					/* Floating point variable initial value */
	int str_offset;					/* String variable initial value offset */
} InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field;	/* Variable record status field */
	char *plex;						/* Pointer to lexeme (VID name) in Character buffer */
	int o_line;						/* Line number of first occurence */
	InitialValue i_value;			/* Variable initial value */
	size_t reserved;				/* Reserved for future use */
} STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;					/* Pointer to array of STVR */
	int st_size;					/* Size in number of STVR elements */
	int st_offset;					/* Offset in number of STVR elements */
	Buffer *plsBD;					/* Pointer to the lexeme storage buffer descriptor */
} STD;
/*****************************************************************************/

/************************** FUNCTION DEFINITIONS *****************************/
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);
/*****************************************************************************/

#endif
