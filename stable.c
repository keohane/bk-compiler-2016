/*****************************************************************************
File Name:		stable.c
Compiler:		Visual Studio 2013
Author:			Brandon Keohane
Course:			Compilers - CST8152
Assignment:		The Symbol Table - Assignment 3
Date:			24 November, 2016
Professor:		Svillen Ranev
Purpose:		Allows you to create a symbol table and provides functions 
				to make it function with the rest of the compiler
Function List:  st_create, st_install, st_lookup, st_update_type,
				st_update_value, st_get_type, st_destroy, st_print,
				st_setsize, st_incoffset, st_store, st_sort, str_compare_A,
				str_compare_B
*****************************************************************************/
#define _CRT_SECURE_NO_WARNINGS
#include "stable.h"
#include "token.h" /* Just to have access to the VID_LEN value */
#include <stdlib.h>
#include <string.h>

#define DEBUG
#undef DEBUG

/* Function Def */
static void st_setsize(void);
static void st_incoffset(void);
static int str_compare_A(const void* a, const void* b);
static int str_compare_D(const void* a, const void* b);
/* Variables */
extern STD sym_table;

/*****************************************************************************
Purpose:			Creates a new symbol table
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	malloc, b_create
Parameters:			st_size - Defualt size of symbol table in number of elements
Return Value:		Initialized STD structure
***************************** Algorithm	**************************************		
-> Declares local variable of symbol table descriptor
-> Allocates dynamic memory for the number of st_size in elements
-> Creates self incrementing buffer in the pointer plsBD
-> Initializes st_offset to zero
-> If successful
	-> Sets st_size to st_size
-> If not successful
	-> Sets st_size to 0
*****************************************************************************/
STD st_create(int st_size){
	/* Declare local symbol table descriptor */
	STD std;
	/* Default error value to cut down on if size and if it passes all tests */
	/* it will be overrided to the st_size passed as parameter */
	std.st_size = 0;
	/* Size cannot be 0 or less */
	if (st_size <= 0) return std;
	/* Allocates array of dynamic memory for the size of st_size in elements */
	if ((std.pstvr = (STVR*)malloc(st_size * sizeof(STVR))) == NULL) return std;

	/*
	Creates buffer with size of the number of symbol table spots multiplied
	by the size of each VID
	*/
	if ((std.plsBD = b_create(
		(short)st_size * ((VID_LEN + 1) * sizeof(char)), 15, 'm')) == NULL) {
		return std;
	}
	/* Initialize st_offset to zero */
	std.st_offset = 0;
	/* Initialization passed: st_size to st_size */
	std.st_size = st_size;
	/* Return initialized structure */
	return std;
}

/*****************************************************************************
Purpose:			Installs a new entry (VID record) in the symbol table
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	st_lookup, strlen, b_addc, b_rflag, b_cbhead, b_size, st_incoffset
Parameters:			sym_table - symbol table to add record to
					*lexeme - variable name to add
					type - variable type (I for integer, F for float, S for string)
					line - line number
Return Value:		offset of the vid added, if full it returns -1
***************************** Algorithm	**************************************
-> Call st_lookup to search for lexeme
	-> If type is not I, F, and S return -1
	-> If symbol table is full return -1
		-> If not in symbol table
			-> Installs the new entry at current offset
			-> Set the pointer to the lexeme (plex) to the position at which the 
			   record was placed
			-> O_line is set to the line number passed as parameter
			-> Status field set to default value
			-> Set data type indicator and update flag (If nessesary)
				-> Set indicator using correct bitwise operation and mask
			-> Set i_value for initial value corresponding to the type
				-> 0 for integer and float, string is -1
			-> Lexeme (value) is added as a c type string
			-> After added increment 'global' st offset
		-> Else if found it returns the corresponding offset
	
*****************************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){
	/* 
	offset - Offset found by st_lookup 
	size - size of buffer holding VIDS
	lSize - size of lexeme passed through parameter
	index - index of character in buffer pointer 
	*/
	int offset, size, lSize, index = 0;
	/* Temporary pointer to STVR */
	STVR* tempSTVR;
	/* R flag thrown indicator */
	char rflag = 0;
	/* Temporary buffer pointer */
	char* tBuffer;
	/* Character to check for end of string */
	char c;
	/***************** Error checking before adding *****************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/* Call st_lookup to see if lexeme exists */
	if ((offset = st_lookup(sym_table, lexeme)) != R_STFAIL_1) return offset;
	/* If not the correct type */
	if (type != CHAR_INTEGER && type != CHAR_FLOAT && type != CHAR_STRING) return R_STFAIL_1;
	/* If symbol table is full */
	if (sym_table.st_offset == sym_table.st_size) return R_STFAIL_1;
	/****************************************************************/

	/* Loop through and add to the buffer */
	for (unsigned int i = 0; i <= strlen(lexeme); ++i){
		b_addc(sym_table.plsBD, lexeme[i]);
		/* If r_flag was thrown during the loop ??? */
		if (b_rflag(sym_table.plsBD)){
			rflag = 1;
		}
	}

	/* Reallocation was done when installing new VID in buffer */
	if (rflag){
		/* Get temporary buffer head */
		tBuffer = b_cbhead(sym_table.plsBD);
		/* Loop and get new pointer locations for plex pointers */
		for (int i = 0; i <= sym_table.st_offset; ++i){
			/* Point plex at new buffer pointer */
			sym_table.pstvr[i].plex = tBuffer;
			do {
				c = tBuffer[index];
				index++;
			} while (c != '\0');
			/* Shift pointer to next VID */
			tBuffer = &(tBuffer[index++]);
			/* Reset index counter */
			index = 0;
		}
		/* No danglers */
		tBuffer = NULL;
	}

	/* Get temp pointer to array position */
	tempSTVR = &(sym_table.pstvr[sym_table.st_offset]);

	/************** Find pointer to newly placed lexeme *************/
	/* Size of total buffer inputed */
	size = b_size(sym_table.plsBD);
	/* Size of lexeme length */
	lSize = strlen(lexeme);
	/* Initialize the plex value of the offset */
	tempSTVR->plex = &(b_cbhead(sym_table.plsBD)[size - lSize - 1]);
	/****************************************************************/

	/* Set line number */
	tempSTVR->o_line = line;
	/* Set status field to default value aka initialize */
	tempSTVR->status_field &= ZERO;
	tempSTVR->status_field |= DEFAULT;
	/* Set type indicator */
	if (type == CHAR_INTEGER) {
		/* Integer type indicator */
		tempSTVR->status_field |= SET_INT;
		/* Set initial value */
		tempSTVR->i_value.int_val = DEFAULT_I;
	}
	else if (type == CHAR_FLOAT){
		/* Floating point type indicator */
		tempSTVR->status_field |= SET_FLT;
		/* Set initial value */
		tempSTVR->i_value.fpl_val = DEFAULT_F;
	}
	else {
		/* String type indicator */
		tempSTVR->status_field |= SET_STRING | SET_UPDATEF;
		/* Set initial value */
		tempSTVR->i_value.str_offset = DEFAULT_S;
	}
	/* No dangling pointers */
	tempSTVR = NULL;
	/* Increment offset */
	st_incoffset();
	/* Return previous offset where new VID was placed */
	return sym_table.st_offset;
}

/*****************************************************************************
Purpose:			Looks up a VID in the symbol table
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	strcmp
Parameters:			sym_table - symbol table to check record from
					*lexeme - variable name to look up
Return Value:		offset from the beggining of array if found, else -1
***************************** Algorithm	**************************************
-> If sym table is NULL return -1
-> If lexeme is NULL return -1
-> Search from back of symbol table to find the lexeme
	-> If found return offset position
	-> Else return -1
*****************************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	/***************** Error checking before adding *****************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/****************************************************************/

	/* Starting at back loop and see if lexeme is in table */
	for (int i = sym_table.st_offset - 1; i >= 0; --i){
		/* If pointer in the buffer contains lexeme VID */
		if (!strcmp(sym_table.pstvr[i].plex, lexeme)) return i;
	}
	/* Wasnt found return -1 */
	return R_STFAIL_1;
}

/*****************************************************************************
Purpose:			Updates type for VID at offset 
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:
Parameters:			sym_table - symbol table to update type of record
					vid_offset - variable identifier offset from start of table
					v_type - variable type
Return Value:		If update flag is 1 (already updated) returns -1
					If string v_type already return -1 (cannot be changed)
					Else returns variable identifier offset
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> If vid offset is out of bounds return -1
-> If v_type is incorrect type return -1
-> If variable at offset is a string return -1
-> If variable at VID offset is already updated return -1
-> Else function updates fields...
	-> Reset status field
	-> Set data type indicator to corresponding type
	-> Set update flag
-> Returns VID offset
*****************************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	unsigned char vtype = v_type;
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/* If vid offset is out of range */
	if (vid_offset >= sym_table.st_offset || vid_offset < 0) return R_STFAIL_1;
	/* If type is incorrect */
	if (vtype != CHAR_INTEGER && vtype != CHAR_FLOAT && vtype != CHAR_STRING) return R_STFAIL_1;
	/* If already updated before */
	if (sym_table.pstvr[vid_offset].status_field % 2 == 1) return R_STFAIL_1;
	/* If variable at offset is string */
	if (sym_table.pstvr[vid_offset].status_field == SET_STRING) return R_STFAIL_1;
	/***********************************************************************/

	/* If already the type being passed as v type */
	if (vtype == CHAR_INTEGER && sym_table.pstvr[vid_offset].status_field == SET_INT) return R_STFAIL_1;
	if (vtype == CHAR_FLOAT && sym_table.pstvr[vid_offset].status_field == SET_FLT) return R_STFAIL_1;
	if (vtype == CHAR_STRING && sym_table.pstvr[vid_offset].status_field == SET_STRING) return R_STFAIL_1;

	/***************************** Update type *****************************/
	/* Reset status field */
	sym_table.pstvr[vid_offset].status_field &= DEFAULT;
	/* Set all flags corresponding to the variable type */
	if (vtype == CHAR_INTEGER) {
		/* Integer type indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_INT;
		/* LSB update indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_UPDATEF;
	}
	else if (vtype == CHAR_FLOAT){
		/* Floating point type indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_FLT;
		/* LSB update indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_UPDATEF;
	}
	else {
		/* String type indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_STRING;
		/* LSB update indicator */
		sym_table.pstvr[vid_offset].status_field |= SET_UPDATEF;
	}
	/***********************************************************************/
	
	/* Update successful return the offset */
	return vid_offset;
}

/*****************************************************************************
Purpose:			Updates value for VID at offset 
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:
Parameters:			sym_table - symbol table to update value of record
					vid_offset - variable identifier offset from start of table
					i_value - initial value of variable
Return Value:		If successful returns vid_offset
					Else returns -1
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> If vid offset is out of bounds or not a appropriate offset return -1
-> Update initial value of the variable
*****************************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/* If vid offset is out of range */
	if (vid_offset >= sym_table.st_offset || vid_offset < 0) return R_STFAIL_1;
	/************************************************************************/

	/* Update type */
	sym_table.pstvr[vid_offset].i_value = i_value;

	/* Update successful return the offset */
	return vid_offset;
}

/*****************************************************************************
Purpose:			Returns type for VID at offset 
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:
Parameters:			sym_table - symbol table to retrieve type from
					vid_offset - variable identifier offset from start of table
Return Value:		Type of variable [I integer, F float, S string] 
					If failure returns -1
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> If vid offset is out of bounds or not a appropriate offset return -1
-> Use appropriate bitwise and masks to get the type of the variable
*****************************************************************************/
char st_get_type(STD sym_table, int vid_offset){
	/* Temporary status field used for bitwise calculations */
	unsigned short status;
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/* If vid offset is out of range */
	if (vid_offset >= sym_table.st_offset || vid_offset < 0) return R_STFAIL_1;
	/************************************************************************/

	/* Remove the update bit from temp status */
	status = sym_table.pstvr[vid_offset].status_field & RESET_UPDATEF;
	/* Return corresponding type letter */
	if (!(status ^ (DEFAULT | INT_MASK)))		return CHAR_INTEGER; /* Integer */
	if (!(status ^ (DEFAULT | FLT_MASK)))		return CHAR_FLOAT;	/* Float   */
	if (!(status ^ (DEFAULT | STRING_MASK)))	return CHAR_STRING; /* String  */

	/* Should not be reached */
	return R_STFAIL_1;
}

/*****************************************************************************
Purpose:			Destroys symbol table passed 
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	b_free, free, st_setsize
Parameters:			sym_table - symbol table to destroy
Return Value:		void
***************************** Algorithm	**************************************
-> Free dynamic memory
-> Set st size to 0
*****************************************************************************/
void st_destroy(STD sym_table){
	/* Remove dynamic buffer structure */
	if (sym_table.plsBD != NULL){
		b_free(sym_table.plsBD);
	}
	/* Set to NULL */
	sym_table.plsBD = NULL;
	/* Remove dynamic symbol table VID record */
	if (sym_table.pstvr != NULL){
		free(sym_table.pstvr);
	}
	/* Set to NULL */
	sym_table.pstvr = NULL;
	/* Reset the size to default */
	st_setsize();
}

/*****************************************************************************
Purpose:			Prints symbol table elements
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	printf
Parameters:			sym_table - symbol table holding elements to print
Return Value:		Returns number of entries printed
					If failure occurs returns -1
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> Print all entries in symbol table while incrementing count of how many 
   were printed
-> Return count of lines printed
*****************************************************************************/
int st_print(STD sym_table){
	/* Total VID's printed */
	int count = 0;
	/* Temp STVR */
	STVR stvr;
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/************************************************************************/

	/* Print all VID's */
	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("%-12s%-13s\n", "Line Number", "Variable Identifier");
	for (int i = 0; i < sym_table.st_offset; ++i){
		stvr = sym_table.pstvr[i];
		printf("%2d%10s%-s\n", stvr.o_line, "", stvr.plex);
		count++;
	}
	return count;
}

/*****************************************************************************
Purpose:			Sets the st_size of this structure to 0
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:
Parameters:			void
Return Value:		void
***************************** Algorithm	**************************************
-> Set st size to 0
*****************************************************************************/
static void st_setsize(void){
	sym_table.st_size = 0;
}

/*****************************************************************************
Purpose:			Increments st_offset by 1
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:
Parameters:			void
Return Value:		void
***************************** Algorithm	**************************************
-> Increment the st offset by 1
*****************************************************************************/
static void st_incoffset(void){
	++sym_table.st_offset;
}

/*****************************************************************************
Purpose:			Stores entire symbol table into a file named $stable.ste
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	fopen, fprintf, st_get_type, strlen, fclose
Parameters:			sym_table - symbol table to copy into file $stable.ste
Return Value:		Number of records stored in file
					If failure occurs returns -1
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> Create file pointer and open file
	-> If not successful return -1
	-> Else start outputing to file + new line
		-> Write st size + space
		-> Loop through entire symbol table...
			-> Write status field in hex format + space
			-> Write length of the lexeme + space
			-> Write lexeme + space
			-> Write line number + space
			-> Write initial value + new line
				-> To get proper initial value you must use st_get_type and 
				   then print the proper value according to what is returned
	-> Return number of records stored
*****************************************************************************/
int st_store(STD sym_table){
	/* Temp STVR */
	STVR stvr;
	/* File pointer */
	FILE* file;
	/* Number of records */
	int nRecords = 0;
	/* Character returned by get type */
	unsigned char c;
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/************************************************************************/

	/* Open file to write to */
	if ((file = fopen(FILE_NAME, "w")) == NULL){
		return R_STFAIL_1;
	}

	/***************************** Write to file ****************************/
	/* Header of size */
	fprintf(file, "%d", sym_table.st_offset);
	/* Loop and write to file */
	for (int i = 0; i < sym_table.st_offset; ++i){
		stvr = sym_table.pstvr[i];
		/* HEX status field */
		fprintf(file, " %X", stvr.status_field);
		/* Length of lexeme */
		fprintf(file, " %d", strlen(stvr.plex));
		/* Vid lexeme */
		fprintf(file, " %s", stvr.plex);
		/* Line number */
		fprintf(file, " %d", stvr.o_line);
		/* Initial value to corresponding type */
		c = st_get_type(sym_table, i);
		switch (c){
			case CHAR_INTEGER:	fprintf(file, " %d", stvr.i_value.int_val);		break;
			case CHAR_FLOAT:	fprintf(file, " %.2f", stvr.i_value.fpl_val);	break;
			case CHAR_STRING:	fprintf(file, " %d", stvr.i_value.str_offset);	break;
		}
		/* Increment records total */
		++nRecords;
	}
	/************************************************************************/

	printf("\nSymbol Table stored.\n");

	/* Close file */
	fclose(file);
	/* Return how many records were written to file */
	return nRecords;
}

/*****************************************************************************
Purpose:			Calls the sorting function, either sorts the array in 
					ascending or descending order
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	qsort, str_compare_A, str_compare_D, b_size, malloc, strlen,
					b_reset, b_addc, free, b_cbhead
Parameters:			sym_table - symbol table to sort
					s_order - Either A ascending or D decending
Return Value:		Returns 0
***************************** Algorithm	**************************************
-> If sym_table is NULL return -1
-> If s_order is not a 'A' or 'D' return -1
-> Call sort function
-> Return 0
*****************************************************************************/
int st_sort(STD sym_table, char s_order){
	/* Total length of lexemes */
	short length;
	/* New character array of lexemes */
	char *newBuf;
	/* Temporary character */
	unsigned char c;
	
	/********************* Error checking before adding *********************/
	/* If sym table pointer are null */
	if (sym_table.plsBD == NULL || sym_table.st_size == 0) return R_STFAIL_1;
	/* If sort order is not a 'A' or a 'D' */
	if (s_order != CHAR_ASCEND && s_order != CHAR_DESCEND) return R_STFAIL_1;
	/************************************************************************/
	if (s_order == CHAR_ASCEND)
	{
		/* Throws a warning for the parameter not being const values into function */
		qsort((void*)sym_table.pstvr, (size_t)sym_table.st_offset, sizeof(STVR), str_compare_A);
	}
	else
	{
		/* Throws a warning for the parameter not being const values into function */
		qsort((void*)sym_table.pstvr, (size_t)sym_table.st_offset, sizeof(STVR), str_compare_D);
	}
	/* Total length of the buffer */
	length = b_size(sym_table.plsBD);
	/* Create a new character array to hold sorted buffer */
	if ((newBuf = malloc(length * sizeof(char))) == NULL) return R_STFAIL_1;
	
	/* Fill temporary buffer */
	for (int i = 0, count = 0; i < sym_table.st_offset; ++i){
		for (unsigned int y = 0; y < strlen(sym_table.pstvr[i].plex) + 1; ++y){
			newBuf[count] = *(sym_table.pstvr[i].plex + y);
			count++;
		}
	}

	/* Reset buffer to override when adding */
	b_reset(sym_table.plsBD);
	/* Loop and add correct order to buffer */
	for (int i = 0; i < length; ++i){
		/* Add character */
		b_addc(sym_table.plsBD, newBuf[i]);
	}

	/* Free memory */
	free(newBuf);
	/* Reuse pointer for buffer head */
	newBuf = b_cbhead(sym_table.plsBD);

	/* Loop and get new pointer locations for plex pointers */
	for (int i = 0, index = 0; i < sym_table.st_offset; ++i){
		/* Point plex at new buffer pointer */
		sym_table.pstvr[i].plex = newBuf;
		do {
			c = newBuf[index];
			index++;
		} while (c != '\0');
		/* Shift pointer to next VID */
		newBuf = &(newBuf[index++]);
		/* Reset index counter */
		index = 0;
	}

	/* No dangling pointers */
	newBuf = NULL;

	return 0;
}

/*****************************************************************************
Purpose:			Compares strings for ascending order
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	strcmp
Parameters:			a - plex to compare
					b - plex to compare
Return Value:		Returns 1 to indicate higher, 0 the same, -1 lower
***************************** Algorithm	**************************************
*****************************************************************************/
int str_compare_A(const void* a, const void* b){
	return strcmp(((STVR*)a)->plex, ((STVR*)b)->plex);
}

/*****************************************************************************
Purpose:			Compares strings for descending order
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	strcmp
Parameters:			a - plex to compare
					b - plex to compare
Return Value:		Returns 1 to indicate higher, 0 the same, -1 lower
***************************** Algorithm	**************************************
*****************************************************************************/
int str_compare_D(const void* a, const void* b){
	return strcmp(((STVR*)b)->plex, ((STVR*)a)->plex);
}
