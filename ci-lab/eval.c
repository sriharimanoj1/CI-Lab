/**************************************************************************
 * C S 429 EEL interpreter
 * 
 * eval.c - This file contains the skeleton of functions to be implemented by
 * you. When completed, it will contain the code used to evaluate an expression
 * based on its AST.
 * 
 * Copyright (c) 2021. S. Chatterjee, X. Shen, T. Byrd. All rights reserved.
 * May not be used, modified, or copied without permission.
 **************************************************************************/ 

#include <string.h>
#include "ci.h"

extern bool is_binop(token_t);
extern bool is_unop(token_t);
char *strrev(char *str);

/* infer_type() - set the type of a non-root node based on the types of children
 * Parameter: A node pointer, possibly NULL.
 * Return value: None.
 * Side effect: The type field of the node is updated.
 * (STUDENT TODO)
 */

static void infer_type(node_t *nptr) {
    if (nptr == NULL) 
        return;
    if(nptr->node_type==NT_LEAF&&nptr->type==ID_TYPE){
        entry_t *var = calloc(1, sizeof(entry_t));
        var = get(nptr->val.sval);
        if(var == NULL)
            return;
        nptr->type = var->type;
        return;
    }
    if(nptr->node_type==NT_LEAF)
        return;
    for(int i = 0; i < 3; i++) {
        if((*nptr).children[i] == NULL)
            break;
        infer_type((*nptr).children[i]);
    }
    if(nptr->tok == TOK_EQ || nptr->tok == TOK_LT || nptr->tok == TOK_GT) {
        nptr->type = BOOL_TYPE;
    }
    else if(nptr->tok == TOK_DIV){
        nptr->type = INT_TYPE;
    }
    else {
        nptr-> type = (*nptr).children[0]-> type;
    }
}

/* infer_root() - set the type of the root node based on the types of children
 * Parameter: A pointer to a root node, possibly NULL.
 * Return value: None.
 * Side effect: The type field of the node is updated. 
 */

static void infer_root(node_t *nptr) {
    if (nptr == NULL) return;
    // check running status
    if (terminate || ignore_input) return;

    // check for assignment
    if (nptr->type == ID_TYPE) {
        infer_type(nptr->children[1]);
    } else {
        for (int i = 0; i < 3; ++i) {
            infer_type(nptr->children[i]);
        }
        if (nptr->children[0] == NULL) {
            logging(LOG_ERROR, "failed to find child node");
            return;
        }
        nptr->type = nptr->children[0]->type;
    }
    return;
}

/* eval_node() - set the value of a non-root node based on the values of children
 * Parameter: A node pointer, possibly NULL.
 * Return value: None.
 * Side effect: The val field of the node is updated.
 * (STUDENT TODO) 
 */

static void eval_node(node_t *nptr) {
    if(nptr == NULL) {
        return;
    }
    if (terminate || ignore_input) return;
    if(nptr-> tok == TOK_PLUS) {
        eval_node(nptr->children[0]);
        if(nptr-> children[0]-> type == BOOL_TYPE) {
            handle_error(ERR_TYPE);
            return;
        }
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            int sum = childone-> val.ival + childtwo-> val.ival;
            nptr-> tok = TOK_NUM;
            nptr-> type = INT_TYPE;
            nptr-> val.ival = sum;
            return;
        }
        if(nptr-> children[0]-> type == STRING_TYPE && nptr-> children[1]->type  == STRING_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            char *str = malloc(sizeof(childone->val.sval));
            strcpy(str, childone->val.sval);
            char *sum = strcat(str, childtwo-> val.sval);
            nptr-> tok = TOK_STR;
            nptr-> type = STRING_TYPE;
            nptr-> val.sval = sum;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_BMINUS) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE) {  
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            int diff = childone-> val.ival - childtwo-> val.ival;
            nptr-> tok = TOK_NUM;
            nptr-> type = INT_TYPE;
            nptr-> val.ival = diff;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_TIMES) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            int product = childone-> val.ival * childtwo-> val.ival;
            nptr-> tok = TOK_NUM;
            nptr-> type = INT_TYPE;
            nptr-> val.ival = product;
            return;
        }
        if(nptr-> children[0]-> type == STRING_TYPE && nptr-> children[1]->type  == INT_TYPE){
            if(nptr->children[1]-> val.ival < 0) {
                handle_error(ERR_EVAL);
                return;
            }
            char *str = nptr->children[0]->val.sval;
            int amt = nptr->children[1]->val.ival;
            int len = strlen(str);
            char *combined = malloc(len*amt + 1);
            char *temp;
            int i;
            for(i = 0, temp = combined; i < amt; ++i, temp += len) {
                memcpy(temp, str, len);
            }
            *temp = '\0';
            nptr-> tok = TOK_STR;
            nptr-> type = STRING_TYPE;
            nptr-> val.sval = combined;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_DIV) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE) {
            if(nptr-> children[1]->val.ival == 0){
            handle_error(ERR_EVAL);
            return;
            }
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            int quotient = childone-> val.ival / childtwo-> val.ival;
            nptr-> tok = TOK_NUM;
            nptr-> type = INT_TYPE;
            nptr-> val.ival = quotient;
            return;
        }
        else{
                    handle_error(ERR_TYPE);
                    return;
        }
    }
    if(nptr-> tok == TOK_MOD) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            if(nptr-> children[1]->val.ival == 0){
            handle_error(ERR_EVAL);
            return;
        }
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            int mod = childone-> val.ival % childtwo-> val.ival;
            nptr-> tok = TOK_NUM;
            nptr-> type = INT_TYPE;
            nptr-> val.ival = mod;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_AND) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == BOOL_TYPE && nptr-> children[1]->type  == BOOL_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool and = childone-> val.bval && childtwo-> val.bval;
            if(and)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = and;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_OR) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == BOOL_TYPE && nptr-> children[1]->type  == BOOL_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool or = childone-> val.bval || childtwo-> val.bval;
            if(or)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = or;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_EQ) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool eq = childone-> val.bval == childtwo-> val.bval;
            if(eq)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = eq;
            return;
        }
        if(nptr-> children[0]-> type == STRING_TYPE && nptr-> children[1]->type  == STRING_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool eq = strcmp(childone-> val.sval, childtwo-> val.sval) == 0;
            if(eq)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = eq;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_LT) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool lt = childone-> val.bval < childtwo-> val.bval;
            if(lt)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = lt;
            return;
        }
        if(nptr-> children[0]-> type == STRING_TYPE && nptr-> children[1]->type  == STRING_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool lt = strcmp(childone-> val.sval, childtwo-> val.sval) < 0;
            if(lt)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = lt;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr-> tok == TOK_GT) {
        eval_node(nptr->children[0]);
        eval_node(nptr->children[1]);
        if(nptr-> children[0]-> type == INT_TYPE && nptr-> children[1]->type  == INT_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool gt = childone-> val.bval > childtwo-> val.bval;
            if(gt)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = gt;
            return;
        }
        if(nptr-> children[0]-> type == STRING_TYPE && nptr-> children[1]->type  == STRING_TYPE){
            node_t *childone = nptr->children[0];
            node_t *childtwo = nptr->children[1];
            bool gt = strcmp(childone-> val.sval, childtwo-> val.sval) > 0;
            if(gt)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = gt;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr->tok == TOK_UMINUS) {
        eval_node(nptr->children[0]);
        if(nptr->children[0]->type == INT_TYPE) {
            nptr->tok = TOK_NUM;
            nptr->type = INT_TYPE;
            int sub = 0 - (nptr->children[0]->val.ival);
            nptr->val.ival = sub;
            return;
        }
        else if(nptr->children[0]->type == STRING_TYPE) {
            nptr->tok = TOK_NUM;
            nptr->type = STRING_TYPE;
            char *rev = malloc(sizeof(nptr->children[0]->val.sval));
            strcpy(rev, nptr->children[0]->val.sval);
            //char *reversed = strrev(nptr->children[0]->val.sval);
            nptr->val.sval = strrev(rev);
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr->tok == TOK_NOT) {
        eval_node(nptr->children[0]);
        if(nptr->children[0]->type == BOOL_TYPE) {
            bool val = !nptr->children[0]->val.bval;
            if(val)
                nptr-> tok = TOK_TRUE;
            else
                nptr-> tok = TOK_FALSE;
            nptr-> type = BOOL_TYPE;
            nptr-> val.bval = val;
            return;
        }
        handle_error(ERR_TYPE);
    }
    if(nptr->tok == TOK_ID) {
        entry_t *var = calloc(1, sizeof(entry_t));
        var = get(nptr->val.sval);
        nptr->type = var->type;
        nptr->val = var->val;
        if(nptr-> type == STRING_TYPE) {
            nptr->tok = TOK_STR;
        }
        else if(nptr-> type == INT_TYPE) {
            nptr->tok = TOK_NUM; 
        }
        else {
            if(nptr-> val.bval == true) {
                nptr->tok = TOK_TRUE;
            }
            if(nptr-> val.bval == false) {
                nptr->tok = TOK_FALSE;
            }
        }
    }
    if(nptr->tok == TOK_QUESTION) {
        eval_node(nptr->children[0]);
        if(nptr->children[1]->type == nptr->children[2]->type && nptr->children[0]->type == BOOL_TYPE) {
            if(nptr->children[0]->val.bval) {
                eval_node(nptr->children[1]);
                nptr->type = nptr->children[1]->type;
                nptr->tok = nptr->children[1]->tok;
                if(nptr->type == INT_TYPE)
                    nptr->val.ival = nptr->children[1]-> val.ival;
                if(nptr->type == STRING_TYPE)
                    nptr->val.sval = nptr->children[1]-> val.sval;
                if(nptr->type == BOOL_TYPE)
                    nptr->val.bval = nptr->children[1]-> val.bval;   
                return;      
            }
            else {
                eval_node(nptr->children[2]);
                nptr->type = nptr->children[2]->type;
                nptr->tok = nptr->children[2]->tok;
                if(nptr->type == INT_TYPE)
                    nptr->val.ival = nptr->children[2]-> val.ival;
                if(nptr->type == STRING_TYPE)
                    nptr->val.sval = nptr->children[2]-> val.sval;
                if(nptr->type == BOOL_TYPE)
                    nptr->val.bval = nptr->children[2]-> val.bval; 
                return;    
            }
        }
        handle_error(ERR_TYPE);
    }
    
}

/* eval_root() - set the value of the root node based on the values of children 
 * Parameter: A pointer to a root node, possibly NULL.
 * Return value: None.
 * Side effect: The val dield of the node is updated. 
 */

void eval_root(node_t *nptr) {
    if (nptr == NULL) return;
    // check running status
    if (terminate || ignore_input) return;

    // check for assignment
    if (nptr->type == ID_TYPE) {
        eval_node(nptr->children[1]);
        if (terminate || ignore_input) return;
        
        if (nptr->children[0] == NULL) {
            logging(LOG_ERROR, "failed to find child node");
            return;
        }
        put(nptr->children[0]->val.sval, nptr->children[1]);
        return;
    }

    for (int i = 0; i < 2; ++i) {
        eval_node(nptr->children[i]);
    }
    if (terminate || ignore_input) return;
    if (nptr->type == STRING_TYPE) {
        (nptr->val).sval = (char *) malloc(strlen(nptr->children[0]->val.sval) + 1);
        if (! nptr->val.sval) {
            logging(LOG_FATAL, "failed to allocate string");
            return;
        }
        strcpy(nptr->val.sval, nptr->children[0]->val.sval);
    } else {
        nptr->val.ival = nptr->children[0]->val.ival;
    }
    return;
}

/* infer_and_eval() - wrapper for calling infer() and eval() 
 * Parameter: A pointer to a root node.
 * Return value: none.
 * Side effect: The type and val fields of the node are updated. 
 */

void infer_and_eval(node_t *nptr) {
    infer_root(nptr);
    eval_root(nptr);
    return;
}

/* strrev() - helper function to reverse a given string 
 * Parameter: The string to reverse.
 * Return value: The reversed string. The input string is not modified.
 * (STUDENT TODO)
 */

char *strrev(char *str) {
    char *f, *b, temp;
    int length = strlen(str);
    f = str;
    b = str;
    for(int i = 0; i < length - 1; i++) {
        b++;
    }
    for(int i = 0; i < length/2; i++) {
        temp = *b;
        *b = *f;
        *f = temp;
        f++;
        b--;
    }
    return str;
}