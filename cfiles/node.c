#include "node.h"
#include <string.h>
#include <stdio.h>

struct Node *makeNode(char *id, int val)
{
    struct Node *newnode = malloc(sizeof(struct Node));
    newnode->id = id;
    newnode->val = val;
    newnode->cost = 100000000;
    newnode->path = "";
    return newnode;
}

int getNodeValue(struct Node *n)
{
    return n->val;
}
char *getNodeLabel(struct Node *n)
{
    return n->id;
}
void setNodeValue(struct Node *n, int val)
{
    n->val = val;
}
