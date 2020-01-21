#include "all.h"
/*
 * <name.c>=
 */
struct Name {
    const char *s;
};
/*
 * Returning the string associated with a name is
 * trivial.
 * <name.c>=
 */
const char* nametostr(Name np) {
    assert(np != NULL);
    return np->s;
}
/*
 * Finding the name associated with a string is harder.
 * To meet the specification, if I get a string I have
 * seen before, I must return the same name I returned
 * before. To remember what I have seen and returned,
 * I use the simplest possible data structure:
 * [[all_names]], a list of all names we ever returned.
 * Given a string [[s]], a simple linear search finds
 * the name associated with it, if any.
 * <name.c>=
 */
Name strtoname(const char *s) {
    static Namelist all_names;
    assert(s != NULL);

    for (Namelist unsearched = all_names; unsearched; unsearched = unsearched->
                                                                             tl)
        if (strcmp(s, unsearched->hd->s) == 0)
            return unsearched->hd;

    /*
     * If the string [[s]] isn't associated with any name on
     * the list [[all_names]], I make a new name and add it.
     * <allocate a new name, add it to [[all_names]], and return it>=
     */
    Name np = malloc(sizeof(*np));
    assert(np != NULL);
    np->s = malloc(strlen(s) + 1);
    assert(np->s != NULL);
    strcpy((char*)np->s, s);
    all_names = mkNL(np, all_names);
    return np;
}
/*
 * A faster implementation might use a search tree or a
 * hash table, not a simple list. \citet[Chapter 3]
 * hanson:interfaces shows such an implementation.
 */

