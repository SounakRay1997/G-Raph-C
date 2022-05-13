#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#ifndef BUILDSTDLIB
#include "edge.h"
#endif

struct EListElem
{
    struct Edge *edge;
    struct EListElem *next;
    struct EListElem *prev;
};

struct EList
{

    struct EListElem *head;
    struct EListElem *tail;
};

struct EList *createEList();
int elistLen(struct EList *e_list);
bool findEdge(struct EList *n, struct Edge *edge);
void deleteEdge(struct EList *e, struct Edge *edge);
void appendEdge(struct EList *e_list, struct Edge *e);