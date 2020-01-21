#include "all.h"
/* name.c 56a */
struct Name {
    const char *s;
};
/* name.c 56b */
const char* nametostr(Name np) {
    assert(np != NULL);
    return np->s;
}
/* name.c 56c */
Name strtoname(const char *s) {
    static Namelist all_names;
    assert(s != NULL);

    for (Namelist unsearched = all_names; unsearched; unsearched = unsearched->
                                                                             tl)
        if (strcmp(s, unsearched->hd->s) == 0)
            return unsearched->hd;

    /* allocate a new name, add it to [[all_names]], and return it 56d */
    Name np = malloc(sizeof(*np));
    assert(np != NULL);
    np->s = malloc(strlen(s) + 1);
    assert(np->s != NULL);
    strcpy((char*)np->s, s);
    all_names = mkNL(np, all_names);
    return np;
}
