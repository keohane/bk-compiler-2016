/*****************************************************************************
File Name: parser.h
Compiler: Visual Studio 2013 Compiler
Author: Brandon Keohane
Course: Computer Engineering Technology Lab: 012
Assignment: Assignment 01 - The Parser
Date: 8th December, 2016
Professor: Svillen Ranev
Purpose: Header for parser
Function List:
-> mlwpar_next_token() parser(Buffer*), match(int, int), syn_eh(int), 
   syn_printe(), gen_incode(char*), program(), opt_statements(),
   statements(), statements_p(), statement(), assignment_statement(),
   assignment_expression(), selection_statement(), iteration_statement(),
   input_statement(), variable_list(), variable_list_p(), opt_variable_list(),
   variable_identifier(), output_statement(), output_list(),
   arithmetic_expression(), unary_arithmetic_expression(),
   additive_arithmetic_expression(), additive_arithmetic_expression_p(),
   multiplicative_arithmetic_expression(), multiplicative_arithmetic_expression_p(),
   primary_arithmetic_expression(), string_expression(), string_expression_p(),
   primary_string_expression(), conditional_expression(), logical_or_expression(),
   logical_or_expression_p(), logical_and_expression(), logical_and_expression_p(),
   relational_expression(), primary_a_relational_expression(),
   primary_s_relational_expression(), primary_a_relational_expression_p(),
   primary_s_relational_expression_p()
*****************************************************************************/

#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef STABLE_H_
#include "stable.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

/******************************** Variables *********************************/
#define NO_ATTR -1
#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7
extern int line;
extern STD sym_table;
extern Buffer* str_LTBL;
extern char* kw_table[];
/****************************************************************************/

/*************************** Function definitions ***************************/
extern Token mlwpar_next_token(Buffer* sc_buf);
void parser(Buffer*);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void opt_variable_list(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void primary_a_relational_expression_p(void);
void primary_s_relational_expression_p(void);
/****************************************************************************/
#endif
