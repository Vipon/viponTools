/***
 * MIT License
 *
 * Copyright (c) 2026 Konychev Valera
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/* Implementation of linux like list */

#ifndef _LINUX_LIST_H
#define _LINUX_LIST_H

#include <linux/types.h>
#include <linux/container_of.h>

/**
 * @brief initialize a list_head links to point to itself
 * @param[in] name of the list_head
 */
#define LIST_HEAD_INIT(name) { &(name), &(name) }

/**
 * @brief definition of a &struct list_head with initialization values
 * @param[in] name of the list_head
 */
#define LIST_HEAD(name) \
    struct list_head name = LIST_HEAD_INIT(name)

/**
 * @brief get the struct for this entry
 * @param[in] ptr    the &struct list_head pointer.
 * @param[in] type   the type of the struct this is embedded in.
 * @param[in] member the name of the list_head within the struct.
 */
#define list_entry(ptr, type, member) \
    container_of(ptr, type, member)

/**
 * @brief get the first element from a list
 * @param[in] ptr    the list head to take the element from.
 * @param[in] type   the type of the struct this is embedded in.
 * @param[in] member the name of the list_head within the struct.
 *
 * Note, that list is expected to be not empty.
 */
#define list_first_entry(ptr, type, member) \
    list_entry((ptr)->next, type, member)

/**
 * @brief get the last element from a list
 * @param[in] ptr    the list head to take the element from.
 * @param[in] type   the type of the struct this is embedded in.
 * @param[in] member the name of the list_head within the struct.
 *
 * Note, that list is expected to be not empty.
 */
#define list_last_entry(ptr, type, member) \
    list_entry((ptr)->prev, type, member)

/**
 * @brief get the next element in list
 * @param[in] pos    the type * to cursor
 * @param[in] member the name of the list_head within the struct.
 */
#define list_next_entry(pos, member) \
    list_entry((pos)->member.next, typeof(*(pos)), member)

/**
 * @brief iterate over a list
 * @param[in] i iterator.
 * @param[in] head the head for your list.
 */
#define list_for_each(i, head) \
    for (i = (head)->next; !list_is_head(i, (head)); i = i->next)

/**
 * @brief iterate over a list safe against removal of list entry
 * @param[in] cur   the &struct list_head to use as a loop cursor
 * @param[in] next  another &struct list_head to use as temporary storage
 * @param[in] head  the head for your list.
 */
#define list_for_each_safe(cur, next, head) \
    for (cur = (head)->next, next = cur->next; \
         !list_is_head(cur, (head)); \
         cur = next, next = cur->next)

/**
 * @brief count nodes in the list
 * @param[in] head the head for your list.
 */
static inline size_t
list_count_nodes(struct list_head *head)
{
    struct list_head *pos;
    size_t count = 0;

    list_for_each(pos, head)
        count++;

    return count;
}

/**
 * @brief test if the entry points to the head of the list
 * @param[in] pos    the type * to cursor
 * @param[in] head   the head for your list.
 * @param[in] member the name of the list_head within the struct.
 */
#define list_entry_is_head(pos, head, member) \
    list_is_head(&pos->member, (head))

/**
 * @brief iterate over list of given type
 * @param[in] pos    the type * to use as a loop cursor.
 * @param[in] head   the head for your list.
 * @param[in] member the name of the list_head within the struct.
 */
#define list_for_each_entry(pos, head, member)               \
    for (pos = list_first_entry(head, typeof(*pos), member); \
         !list_entry_is_head(pos, head, member);             \
         pos = list_next_entry(pos, member))

/**
 * @brief Initialize a list_head structure
 * @param[in] list list_head structure to be initialized.
 */
static inline
void INIT_LIST_HEAD(struct list_head *list)
{
    list->next = list;
    list->prev = list;
}

/**
 * @brief Insert a new entry after the specified head.
 * @param[in] node new entry to be added
 * @param[in] head list head to add it after
 */
static inline
void list_add(struct list_head *node, struct list_head *head)
{
    node->next = head->next;
    node->prev = head;
    head->next->prev = node;
    head->next = node;
}

/**
 * @brief Insert a new entry before the specified head.
 * @param[in] node new entry to be added
 * @param[in] head list head to add it before
 */
static inline
void list_add_tail(struct list_head *node, struct list_head *head)
{
    list_add(node, head->prev);
}

/**
 * @brief Delete a list entry by making the prev/next entries
 * point to each other.
 * @param[in] node entry should be removed
 */
static inline
void list_del(struct list_head *node)
{
    node->prev->next = node->next;
    node->next->prev = node->prev;
}

/**
 * @brief replace old entry by new one
 * @param[in] old the element to be replaced
 * @param[in] new the new element to insert
 */
static inline
void list_replace(struct list_head *old, struct list_head *new)
{
    new->next = old->next;
    new->next->prev = new;
    new->prev = old->prev;
    new->prev->next = new;
}

/**
 * @brief replace first with second and re-add first at second's position
 * @param[in] first the location to place second
 * @param[in] second the location to place first
 */
static inline
void list_swap(struct list_head *first, struct list_head *second)
{
    struct list_head *pos = second->prev;

    list_del(second);
    list_replace(first, second);
    if (pos == first)
        pos = second;
    list_add(first, pos);
}

/**
 * @brief tests whether @param list is the first entry in list @param head
 * @param[in] list the entry to test
 * @param[in] head the head of the list
 */
static inline
int list_is_first(const struct list_head *list, const struct list_head *head)
{
    return list->prev == head;
}

/**
 * @brief tests whether @param list is the last entry in list @param head
 * @param[in] list the entry to test
 * @param[in] head the head of the list
 */
static inline
int list_is_last(const struct list_head *list, const struct list_head *head)
{
    return list->next == head;
}

/**
 * @brief tests whether @param list is the list @param head
 * @param[in] list the entry to test
 * @param[in] head the head of the list
 */
static inline
int list_is_head(const struct list_head *list, const struct list_head *head)
{
    return list == head;
}

/**
 * @brief tests whether a list is empty
 * @param[in] head the list to test.
 */
static inline
int list_empty(const struct list_head *head)
{
    return head->next == head;
}

#define HLIST_HEAD_INIT { .first = NULL }
#define HLIST_HEAD(name) struct hlist_head name = {  .first = NULL }
#define INIT_HLIST_HEAD(ptr) ((ptr)->first = NULL)


#endif /* _LINUX_LIST_H */
