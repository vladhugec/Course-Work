#include "all.h"
/*
 * Running unit tests
 * 
 * [*] Running a list of unit tests is the job of the
 * function [[process_tests]]:
 * <imptests.c>=
 */
void process_tests(UnitTestlist tests, Valenv globals, Funenv functions) {
    set_error_mode(TESTING);
    int npassed = number_of_good_tests(tests, globals, functions);
    set_error_mode(NORMAL);
    int ntests  = lengthUL(tests);
    report_test_results(npassed, ntests);
}
/*
 * Function [[number_of_good_tests]] runs each test,
 * last one first, and counts the number that pass.
 * So it can catch errors during testing, it expects the
 * error mode to be [[TESTING]]; calling
 * [[number_of_good_tests]] when the error mode is
 * [[NORMAL]] is an unchecked run-time error. \iintlabel
 * number_of_good_tests
 */

/*
 * The key fact about the testing interface is that the
 * list of tests coming in contains the last test first,
 * but we must run the first test first. Function
 * [[number_of_good_tests]] therefore recursively runs
 * [[tests->tl]] before calling [[test_result]] on
 * [[tests->hd]]. It returns the number of tests passed.
 * <imptests.c>=
 */
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
/*
 * If the list [[tests]] were very long, this recursion
 * might blow the C stack. But the list is only as long
 * as the number of tests written by hand, so we
 * probably don't have to worry about more than dozens
 * of tests, for which default stack space should be
 * adequate.
 */

/*
 * Function [[test_result]] handles every kind of unit
 * test. In Impcore there are three kinds:
 * [[check-expect]], [[check-assert]], and
 * [[check-error]]. Typed languages, starting with Typed
 * Impcore in \creftypesys.chap, have more. [*]
 * <imptests.c>=
 */
TestResult test_result(UnitTest t, Valenv globals, Funenv functions) {
    switch (t->alt) {
    case CHECK_EXPECT:
        /*
         * To run a [[check-expect]], we evaluate both the
         * ``check'' and ``expect'' expressions, each under the
         * protection of an error handler. If an error occurs
         * under either evaluation, the test fails. Otherwise we
         * compare the values [[check]] and [[expect]]. If they
         * differ, the test fails; if not, the test passes.
         * All failures trigger error messages.
         * <run [[check-expect]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                /*
                 * <report that evaluating [[t->u.check_expect.check]] failed
                                                                 with an error>=
                 */
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
                /*
                 * <report that evaluating [[t->u.check_expect.expect]] failed
                                                                 with an error>=
                 */
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
                /*
                 * <report failure because the values are not equal>=
                 */
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
        /*
         * To run a [[check-assert]], we evaluate just one
         * expression, which should evaluate, without error, to
         * a nonzero value.
         * <run [[check-assert]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                /*
                 * <report that evaluating [[t->u.check_assert]] failed with an
                                                                         error>=
                 */
                fprint(stderr,
                    "Check-assert failed: evaluating %e causes an error: %s.\n",
                               t->u.check_assert, bufcopy(errorbuf));
                bufreset(errorbuf);
                return TEST_FAILED;
            }
            Value v = eval(t->u.check_assert, globals, functions, empty_env);

            if (v == 0) {
                /*
                 * <report failure because the value is zero>=
                 */
                fprint(stderr, "Check-assert failed: %e evaluated to 0.\n", t->
                                                                u.check_assert);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ERROR:
        /*
         * Implementation of Linestream
         * 
         * A [[Linestream]] owns the memory used to store each
         * line. That memory is pointed to by [[buf]], and its
         * size is stored in [[bufsize]]. \implabelLinestream
         * If no line has been read, [[buf]] is [[NULL]] and
         * [[bufsize]] is zero.
         * <run [[check-error]] test [[t]], returning [[TestResult]]>=
         */
        {   Valenv empty_env = mkValenv(NULL, NULL);
            if (setjmp(testjmp)) {
                bufreset(errorbuf);
                return TEST_PASSED; // error occurred, so the test passed
            }
            Value check = eval(t->u.check_error, globals, functions, empty_env);
            /*
             * [*]
             * <report that evaluating [[t->u.check_error]] produced [[check]]>=
             */
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
