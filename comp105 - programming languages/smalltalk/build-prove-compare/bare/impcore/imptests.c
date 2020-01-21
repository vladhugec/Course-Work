#include "all.h"
/* imptests.c 1171 */
void process_tests(UnitTestlist tests, Valenv globals, Funenv functions) {
    set_error_mode(TESTING);
    int npassed = number_of_good_tests(tests, globals, functions);
    set_error_mode(NORMAL);
    int ntests  = lengthUL(tests);
    report_test_results(npassed, ntests);
}
/* imptests.c 1172c */
int number_of_good_tests(UnitTestlist tests, Valenv globals, Funenv functions) {
    if (tests == NULL)
        return 0;
    else {
        int n = number_of_good_tests(tests->tl, globals, functions);
        switch (test_result(tests->hd, globals, functions)) {
        case TEST_PASSED: return n+1;
        case TEST_FAILED: return n;
        default:          assert(0);
        }
    }
}
/* imptests.c 1173a */
TestResult test_result(UnitTest t, Valenv globals, Funenv functions) {
    switch (t->alt) {
    case CHECK_EXPECT:
        /* run [[check-expect]] test [[t]], returning [[TestResult]] 1173b */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {

/* report that evaluating [[t->u.check_expect.check]] failed with an error 1174d */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->u.check_expect.check, t->u.check_expect.expect
                                                                               ,
                               t->u.check_expect.check, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value check = eval(t->u.check_expect.check, globals, functions,
                                                                     empty_env);

            if (setjmp(testjmp)) {

/* report that evaluating [[t->u.check_expect.expect]] failed with an error 1174e */
                fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                        "value as %e, but evaluating %e causes an error: %s.\n",
                               t->u.check_expect.check, t->u.check_expect.expect
                                                                               ,
                               t->u.check_expect.expect, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value expect = eval(t->u.check_expect.expect, globals, functions,
                                                                     empty_env);

            if (check != expect) {
                /* report failure because the values are not equal 1174c */
                fprint(stderr,
                           "Check-expect failed: expected %e to evaluate to %v",
                       t->u.check_expect.check, expect);
                if (t->u.check_expect.expect->alt != LITERAL)
                    fprint(stderr, " (from evaluating %e)", t->
                                                         u.check_expect.expect);
                fprint(stderr, ", but it's %v.\n", check);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ASSERT:
        /* run [[check-assert]] test [[t]], returning [[TestResult]] 1174a */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {

   /* report that evaluating [[t->u.check_assert]] failed with an error 1175a */
                fprint(stderr,
                    "Check-assert failed: evaluating %e causes an error: %s.\n",
                               t->u.check_assert, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value v = eval(t->u.check_assert, globals, functions, empty_env);

            if (v == 0) {
                /* report failure because the value is zero 1174f */
                fprint(stderr, "Check-assert failed: %e evaluated to 0.\n", t->
                                                                u.check_assert);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ERROR:
        /* run [[check-error]] test [[t]], returning [[TestResult]] 1174b */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                bufreset(errorbuf);
                return TEST_PASSED; // error occurred, so the test passed
            }
            Value check = eval(t->u.check_error, globals, functions, empty_env);

      /* report that evaluating [[t->u.check_error]] produced [[check]] 1175b */
            fprint(stderr,
                    "Check-error failed: evaluating %e was expected to produce "
                           "an error, but instead it produced the value %v.\n",
                           t->u.check_error, check);

            return TEST_FAILED;
        }    
    default: 
        assert(0);
    }
}
