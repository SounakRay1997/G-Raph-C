#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#ifndef BUILDSTDLIB
#include "nodelist.h"
#include "edgelist.h"
#endif

struct Graph
{
    struct NList *nodes;
    struct EList *edges;
};

struct Graph *makeGraph(struct NList *n, struct EList *e);
void addEdge(struct Graph *g, struct Edge *e);
void addNode(struct Graph *g, struct Node *n);
void removeEdge(struct Graph *g, struct Edge *e);
void removeNode(struct Graph *g, struct Node *n);
bool search(struct Graph *g, char *val, char *search_options);
bool findPath(struct Graph *g, struct Node *src, struct Node *dest);
struct Node *getNodeFromGraph(struct Graph *g, char *name);
struct Edge *getEdgeFromGraph(struct Graph *g, struct Node *a, struct Node *b);
struct EList *getAllEdges(struct Graph *g);
struct NList *getAllNodes(struct Graph *g);
struct NList *getNeighbours(struct Graph *g, struct Node *n);
bool isAdjacent(struct Graph *g, struct Node *a, struct Node *b);
void updateNode(struct Graph *g, struct Node *n, int val);
bool BFS(struct Graph *g, bool visited[], struct Node *n, char *val, int index, bool path, struct NList *pathlist);
bool DFS(struct Graph *g, bool visited[], struct Node *n, char *val, int index, bool path, struct NList *pathlist);
void djikstras(struct Graph *g, struct Node *src, struct Node *dest);