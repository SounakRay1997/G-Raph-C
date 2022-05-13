#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#ifndef BUILDSTDLIB
#include "node.h"
#endif

struct NListElem
{
    struct Node *node;
    struct NListElem *next;
    struct NListElem *prev;
};

struct NList
{

    struct NListElem *head;
    struct NListElem *tail;
};

struct NList *createNList();
int nlistLen(struct NList *n_list);
bool findNode(struct NList *n, struct Node *node);
void deleteNode(struct NList *n, struct Node *node);
void appendNode(struct NList *n_list, struct Node *n);
