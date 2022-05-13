#include "edge.h"

struct Edge *makeEdge(struct Node *s, struct Node *d, int w, bool directed)
{
    struct Edge *newedge = (struct Edge *)malloc(sizeof(struct Edge));
    newedge->s = s;
    newedge->d = d;
    newedge->w = w;
    newedge->isdir = directed;
    return newedge;
};

int getEdgeValue(struct Edge *e)
{
    return e->w;
}

void setEdgeValue(struct Edge *e, int w)
{
    e->w = w;
    return;
}
