#include "edgelist.h"
struct EList *createEList()
{
    struct EList *e_list = malloc(sizeof(struct EList));
    e_list->head = NULL;
    e_list->tail = NULL;
    return e_list;
}
bool findEdge(struct EList *e, struct Edge *edge)
{
    struct EListElem *h = e->head;
    while (h != NULL)
    {
        if (h->edge == edge)
        {
            printf("%d", h->edge->w);
            return true;
        }
        h = h->next;
    }
    return false;
}
void deleteEdge(struct EList *e, struct Edge *edge)
{
    struct EListElem *cur = e->head;
    struct EListElem *pv = NULL;

    if (cur->edge == edge)
    {
        printf("HERE\n");
        struct EListElem *temp = cur;
        cur = cur->next;
        free(temp);
        // (e->length)--;
        return;
    }
    else
    {
        while (cur->next != NULL)
        {
            if (cur->next->edge == (struct Edge *)edge)
            {
                
                struct EListElem *temp = cur->next;
                cur->next = temp->next->next;
                free(temp);
                return;
                // (e->length)--;
            }
            cur = cur->next;
        }
    }
    fprintf(stderr, "EDGE NOT FOUND IN LIST");
    exit(1);
}
int elistLen(struct EList *e_list)
{
    int length = 0;
    struct EListElem *current;
    current = e_list->head;
    while (current != NULL)
    {
        length++;
        current = current->next;
    }
    return length;
}

struct Edge *getHead(struct EList *e_list)
{
    return e_list->head->edge;
}
void appendEdge(struct EList *e_list, struct Edge *e)
{
    if (findEdge(e_list, e))
    {
        printf("EDGE ALL READY EXISTS IN LIST");
        exit(1);
    }
    struct EListElem *current = (struct EListElem *)malloc(sizeof(struct EListElem));
    if (elistLen(e_list) == 0)
    {
        current->edge = e;
        current->next = NULL;
        current->prev = NULL;
        e_list->head = current;
        e_list->tail = current;
    }
    else
    {
        current->edge = e;
        current->prev = e_list->tail;
        e_list->tail->next = current;
        e_list->tail = current;
    }
}