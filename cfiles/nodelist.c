#include "nodelist.h"
struct NList *createNList()
{
    struct NList *n_list = malloc(sizeof(struct NList));
    n_list->head = NULL;
    n_list->tail = NULL;
    return n_list;
}
int nlistLen(struct NList *n_list)
{
    int length = 0;
    struct NListElem *current;
    current = n_list->head;
    while (current != NULL)
    {

        length++;
        current = current->next;
    }
    return length;
}
bool findNode(struct NList *n, struct Node *node)
{
    struct NListElem *h = n->head;
    while (h != NULL)
    {
        if (h->node == node)
        {
            return true;
        }
        h = h->next;
    }
    return false;
}
void deleteNode(struct NList *n, struct Node *node)
{
    struct NListElem *cur = n->head;
    struct NListElem *pv = NULL;

    if (cur->node == node)
    {
        struct NListElem *temp = cur;
        cur = cur->next;
        free(temp);

        return;
    }
    else
    {
        while (cur->next != NULL)
        {
            if (cur->next->node == (struct Node *)node)
            {
                struct NListElem *temp = cur->next;
                cur->next = temp->next->next;
                free(temp);
                return;
            }
            cur = cur->next;
        }
    }
    fprintf(stderr, "NODE NOT FOUND IN LIST");
    exit(1);
}
void appendNode(struct NList *n_list, struct Node *n)
{
    if (findNode(n_list, n))
    {
        printf("CANNOT INSERT NODE WITH SAME ID");
        exit(1);
    }
    struct NListElem *current = (struct NListElem *)malloc(sizeof(struct NListElem));

    if (nlistLen(n_list) == 0)
    {
        current->node = n;
        current->next = NULL;
        current->prev = NULL;
        n_list->head = current;
        n_list->tail = current;
    }
    else
    {
        current->node = n;
        current->prev = n_list->tail;
        n_list->tail->next = current;
        n_list->tail = current;
    }
}