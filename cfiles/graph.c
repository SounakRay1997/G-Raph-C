#include "graph.h"
#include <string.h>
struct Graph *makeGraph(struct NList *n, struct EList *e)
{
    struct Graph *newgraph = (struct Graph *)malloc(sizeof(struct Graph));
    newgraph->nodes = n;
    newgraph->edges = e;
    return newgraph;
}

void addEdge(struct Graph *g, struct Edge *e)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    if (e == NULL)
    {
        fprintf(stderr, "EDGE NOT FOUND");
        exit(1);
    }
    appendEdge(g->edges, e);
}
void addNode(struct Graph *g, struct Node *n)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    if (n == NULL)
    {
        fprintf(stderr, "NODE NOT FOUND");
        exit(1);
    }
    appendNode(g->nodes, n);
}
void removeEdge(struct Graph *g, struct Edge *e)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    if (e == NULL)
    {
        fprintf(stderr, "EDGE NOT FOUND");
        exit(1);
    }
    deleteEdge(g->edges, e);
}
void removeNode(struct Graph *g, struct Node *n)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    if (n == NULL)
    {
        fprintf(stderr, "NODE NOT FOUND");
        exit(1);
    }
    deleteNode(g->nodes, n);
    struct EListElem *h = g->edges->head;
    while (h->edge != NULL)
    {
        if (h->edge->s == n)
        {
            deleteEdge(g->edges, h->edge);
        }
        h = h->next;
    }
}

void updateNode(struct Graph *g, struct Node *n, int val)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    struct NListElem *h = g->nodes->head;
    while (h != NULL)
    {
        if (h->node == n)
        {
            h->node->val = val;
            break;
        }
        h = h->next;
    }
}

struct Node *getNodeFromGraph(struct Graph *g, char *name)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    struct NListElem *h = g->nodes->head;
    while (h != NULL)
    {
        if (h->node->id == name)
        {
            return h->node;
        }
        h = h->next;
    }
    return NULL;
}

struct Edge *getEdgeFromGraph(struct Graph *g, struct Node *a, struct Node *b)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    struct EListElem *h = g->edges->head;
    while (h != NULL)
    {
        if (h->edge->s == a && h->edge->d == b)
        {
            return h->edge;
        }
        h = h->next;
    }
    return NULL;
}

struct NList *getNeighbours(struct Graph *g, struct Node *n)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    struct NList *new_list = (struct NList *)malloc(sizeof(struct NList));

    struct EListElem *h = g->edges->head;
    while (h != NULL)
    {
        if (h->edge->s == n)
        {

            struct NListElem *x = (struct NListElem *)malloc(sizeof(struct NListElem));
            if (nlistLen(new_list) == 0)
            {

                x->node = h->edge->d;
                x->next = NULL;
                x->prev = NULL;
                new_list->head = x;
                new_list->tail = x;
            }
            else
            {

                x->node = h->edge->d;
                x->next = NULL;
                new_list->tail->next = x;
                x->prev = new_list->tail;
                new_list->tail = x;
            }
        }

        h = h->next;
    }
    return new_list;
}
bool isAdjacent(struct Graph *g, struct Node *a, struct Node *b)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    if (a == NULL || b == NULL)
    {
        fprintf(stderr, "NODE NOT FOUND");
        exit(1);
    }
    struct EListElem *h = g->edges->head;
    while (h != NULL)
    {
        if (h->edge->s == a && h->edge->d == b)
        {
            return true;
        }
        h = h->next;
    }
    return false;
}

struct NList *getAllNodes(struct Graph *g)
{
    return g->nodes;
}

struct EList *getAllEdges(struct Graph *g)
{
    return g->edges;
}

bool search(struct Graph *g, char *val, char *search_options)
{

    int len_nodes = nlistLen(g->nodes);
    bool visited[len_nodes];
    if (strcmp(search_options, "DFS"))
    {
        if (DFS(g, visited, g->nodes->head->node, val, 0, false, createNList()))
        {
            return true;
        }
    }
    else
    {
        if (BFS(g, visited, g->nodes->head->node, val, 0, false, createNList()))
        {
            return true;
        }
    }

    return false;
}

int findIndexofNode(struct Graph *g, struct Node *n)
{
    if (g == NULL)
    {
        fprintf(stderr, "GRAPH NOT FOUND");
        exit(1);
    }
    struct NListElem *h = g->nodes->head;
    int i = 0;
    while (h != NULL)
    {
        if (h->node == n)
        {
            return i;
        }
        i = i + 1;
        h = h->next;
    }
    return -1;
}

void printPath(struct NList *l)
{
    struct NListElem *h = l->head;

    char *p;
    while (h != NULL)
    {
        printf("Path Found : ");
        printf("%s->", h->node->id);
        h = h->next;
    }
    printf("No Path Found\n");
}

bool DFS(struct Graph *g, bool visited[], struct Node *n, char *val, int index, bool path, struct NList *pathlist)
{
    if (strcmp(n->id, val) == 0)
    {
        if (path)
        {
            appendNode(pathlist, n);
            printPath(pathlist);
        }

        return true;
    }

    visited[index] = true;
    if (path)
    {
        appendNode(pathlist, n);
    }
    struct NList *neighbours = getNeighbours(g, n);

    struct NListElem *h = neighbours->head;

    while (h != NULL)
    {

        if (visited[findIndexofNode(g, h->node)] != true)
        {
            if (DFS(g, visited, h->node, val, findIndexofNode(g, h->node), path, pathlist))
            {
                return true;
            }
        }
        h = h->next;
    }

    return false;
}

bool BFS(struct Graph *g, bool visited[], struct Node *n, char *val, int index, bool path, struct NList *pathlist)
{
    if (strcmp(n->id, val) == 0)
    {
        if (path)
        {
            appendNode(pathlist, n);
            printPath(pathlist);
        }

        return true;
    }

    visited[index] = true;
    if (path)
    {
        appendNode(pathlist, n);
    }
    struct NList *neighbours = getNeighbours(g, n);

    struct NListElem *h = neighbours->head;

    while (h != NULL)
    {

        if (visited[findIndexofNode(g, h->node)] != true)
        {
            if (BFS(g, visited, h->node, val, findIndexofNode(g, h->node), path, pathlist))
            {
                return true;
            }
        }
        h = h->next;
    }

    return false;
}

bool findPath(struct Graph *g, struct Node *src, struct Node *dest)
{
    int len_nodes = nlistLen(g->nodes);
    bool visited[len_nodes];
    if (DFS(g, visited, src, dest->id, findIndexofNode(g, src), true, createNList()))
    {
        return true;
    }
    printPath(createNList());
    return false;
}
char *concat(const char *s1, const char *s2)
{
    char *result = malloc(strlen(s1) + strlen(s2) + 1);

    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

void djikstras(struct Graph *g, struct Node *src, struct Node *dest)
{
    struct NListElem *nodehead = g->nodes->head;
    src->cost = 0;
    src->path = concat(src->id, "->");
    while (nodehead != NULL)
    {
        struct NList *neighbours = getNeighbours(g, nodehead->node);
        struct NListElem *h = neighbours->head;
        while (h != NULL)
        {
            struct Edge *e = getEdgeFromGraph(g, nodehead->node, h->node);
            if ((nodehead->node->cost + e->w) < h->node->cost)
            {
                h->node->cost = nodehead->node->cost + e->w;

                h->node->path = concat(nodehead->node->path, h->node->id);
                h->node->path = concat(h->node->path, "->");
            }
            h = h->next;
        }
        nodehead = nodehead->next;
    }
    printf("%d\n", dest->cost);
    printf("%s", dest->path);
}