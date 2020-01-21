#include "all.h"
/*
 * Updating lists of expressions within contexts
 * 
 * \secrefschemes.context-list-updates describes several
 * functions I use to implement the evaluation of \
 * xapply, \xlet, and other forms that use an
 * [[Explist]] to remember a list of values.
 * 
 * <context-lists.c>=
 */
/*
 * <private functions for updating lists of expressions in contexts>=
 */
static void fill_hole(Exp e, Value v) {
  assert(e->alt == HOLE);
  e->alt = LITERAL;
  e->u.literal = v;
}
/*
 * Function [[find_explist_hole]] returns a pointer to
 * the first hole in a list of expressions, or if there
 * is no hole, returns [[NULL]].
 * <private functions for updating lists of expressions in contexts>=
 */
static Explist find_explist_hole(Explist es) {
  while (es && es->hd->alt != HOLE)
    es = es->tl;
  return es;
}
/*
 * To move hole from one position to the next, I find
 * the hole, fill it, and then place a hole at the
 * beginning of the rest of the list.
 * <context-lists.c>=
 */
Exp transition_explist(Explist es, Value v) {
  Explist p = find_explist_hole(es);
  assert(p);
  fill_hole(p->hd, v);
  return head_replaced_with_hole(p->tl);
}
/*
 * Function [[head_replaced_with_hole(es)]] replaces the
 * head of list [[es]] with a hole, returning the old
 * head. If list [[es]] is empty,
 * [[head_replaced_with_hole]] returns [[NULL]].
 * Function [[head_replaced_with_hole]] doesn't allocate
 * space for each new result—all results share the same
 * space.
 * <context-lists.c>=
 */
Exp head_replaced_with_hole(Explist es) {
  static struct Exp a_copy; // overwritten by subsequent calls
  if (es) {
    a_copy = *es->hd;
    *es->hd = mkHoleStruct();
    return &a_copy;
  } else {
    return NULL;
  }
}
/*
 * Function [[copyEL]] copies not only the [[Explist]]
 * pointers but also the [[Exp]] pointers they hold.
 * <context-lists.c>=
 */
Explist copyEL(Explist es) {
  if (es == NULL)
    return NULL;
  else {
    Exp e = malloc(sizeof(*e));
    assert(e);
    *e = *es->hd;
    return mkEL(e, copyEL(es->tl));
  }
}
/*
 * Correspondingly, [[freeEL]] frees both the
 * [[Explist]] pointers and the internal [[Exp]]
 * pointers.
 */

/*
 * <context-lists.c>=
 */
void freeEL(Explist es) {
  if (es != NULL) {
    freeEL(es->tl);
    free(es->hd);
    free(es);
  }
}
/*
 * By contrast, a [[Valuelist]] contains no internal
 * pointers, so only the [[Valuelist]] pointers can be
 * freed.
 * <context-lists.c>=
 */
void freeVL(Valuelist vs) {
  if (vs != NULL) {
    freeVL(vs->tl);
    free(vs);
  }
}
/*
 * Conversion of an [[Explist]] to a [[Valuelist]]
 * requires allocation and therefore incurs an
 * obligation to call [[freeVL]] on the result.
 * <context-lists.c>=
 */
Valuelist asLiterals(Explist es) {
  if (es == NULL)
    return NULL;
  else
    return mkVL(asLiteral(es->hd), asLiterals(es->tl));

}
/*
 * By contrast, because a [[Value]] is not a pointer,
 * [[asLiteral]] need not allocate.
 * <context-lists.c>=
 */
Value asLiteral(Exp e) {
  assert(e->alt == LITERAL);
  return validate(e->u.literal);
}
