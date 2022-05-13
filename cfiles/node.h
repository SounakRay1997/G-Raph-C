#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#ifndef BUILDSTDLIB
#include "nodelist.h"
#endif

struct Node
{
    char *id;
    int val;
    int cost;
    char *path;
};

struct Node *makeNode(char *id, int val);
int getNodeValue(struct Node *n);
char *getNodeLabel(struct Node *n);
void setNodeValue(struct Node *n, int val);
int print_node(struct Node *x);
