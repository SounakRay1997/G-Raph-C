#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#ifndef BUILDSTDLIB
#include "node.h"
#include "edgelist.h"
#endif

struct Edge
{
    struct Node *s;
    struct Node *d;
    int w;
    bool isdir;
};

struct Edge *makeEdge(struct Node *s, struct Node *d, int w, bool directed);
int getEdgeValue(struct Edge *e);
