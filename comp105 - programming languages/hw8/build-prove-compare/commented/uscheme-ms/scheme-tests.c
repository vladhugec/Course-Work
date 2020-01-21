#include "all.h"
/*
 * micro-Scheme's unit tests
 * 
 * [*] Running a list of unit tests is the job of the
 * function [[process_tests]]. It's just like the
 * [[process_tests]] for Impcore in \cref
 * impcorea.testing, except that instead of Impcore's
 * separate function and value environments, the
 * micro-Scheme version uses the single micro-Scheme
 * environment.
 * <scheme-tests.c>=
 */
void process_tests(UnitTestlist tests, Env rho) {
    set_error_mode(TESTING);
    int npassed = number_of_good_tests(tests, rho);
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
 * [[NORMAL]] is an unchecked run-time error. Again,
 * except for the environment, it's just like the
 * Impcore version.
 */

/*
 * <scheme-tests.c>=
 */
int number_of_good_tests(UnitTestlist tests, Env rho) {
    if (tests == NULL)
        return 0;
    else {
        int n = number_of_good_tests(tests->tl, rho);
        switch (test_result(tests->hd, rho)) {
        case TEST_PASSED: return n+1;
        case TEST_FAILED: return n;
        default:          assert(0);
        }
    }
}
/*
 * <scheme-tests.c>=
 */
TestResult test_result(UnitTest t, Env rho) {
    switch (t->alt) {
    case CHECK_EXPECT:
        /*
         * <run [[check-expect]] test [[t]], returning [[TestResult]]>=
         */
        {   if (setjmp(testjmp)) {
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
            Value check = eval(t->u.check_expect.check,  rho);
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
            pushreg(&check);
            Value expect = eval(t->u.check_expect.expect, rho);
            popreg(&check);

            if (!equalpairs(check, expect)) {
                /*
                 * And the reporting is as in Impcore.
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
         * And [[check-assert]]
         * <run [[check-assert]] test [[t]], returning [[TestResult]]>=
         */
        {   if (setjmp(testjmp)) {
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
            Value v = eval(t->u.check_assert, rho);

            if (v.alt == BOOLV && !v.u.boolv) {
                /*
                 * <report failure because the value is false>=
                 */
                fprint(stderr, "Check-assert failed: %e evaluates to #f.\n", t->
                                                                u.check_assert);
                return TEST_FAILED;
            } else {
                return TEST_PASSED;
            }
        }
    case CHECK_ERROR:
        /*
         * A [[check-error]] needn't test for equality, so
         * again, except for the environment, it is just as in
         * Impcore.
         * <run [[check-error]] test [[t]], returning [[TestResult]]>=
         */
        {   if (setjmp(testjmp)) {
                bufreset(errorbuf);
                return TEST_PASSED; // error occurred, so the test passed
            }
            Value check = eval(t->u.check_error,  rho);
            /*
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
/*
 * <scheme-tests.c>=
 */
bool equalpairs(Value v, Value w) {
    if (v.alt != w.alt)
        return false;
    else
        switch (v.alt) {
        case PAIR:
            return equalpairs(*v.u.pair.car, *w.u.pair.car) &&
                   equalpairs(*v.u.pair.cdr, *w.u.pair.cdr);
        case NUM:
            return v.u.num   == w.u.num;
        case BOOLV:
            return v.u.boolv == w.u.boolv;
        case SYM:
            return v.u.sym   == w.u.sym;
        case NIL:
            return true;
        default:
            return false;
        }
}
