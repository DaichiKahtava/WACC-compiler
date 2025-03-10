package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class commonAncestorTests extends AnyFlatSpec {
    // Tests for sem.getLowestCommonAncestor.

    val sem = new Semantics("foo.txt")

    // Test case for an empty list.
    "sem.getLowestCommonAncestor" should "return S_ANY for an empty list" in {
        val types = List()
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for a single element list of S_BOOL.
    it should "return S_BOOL for a single element list of S_BOOL" in {
        val types = List(S_BOOL)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_BOOL)
    }

    // Test case for a single element list of S_INT.
    it should "return S_INT for a single element list of S_INT" in {
        val types = List(S_INT)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_INT)
    }

    // Test case for a single element list of S_CHAR.
    it should "return S_CHAR for a single element list of S_CHAR" in {
        val types = List(S_CHAR)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_CHAR)
    }

    // Test case for S_BOOL with S_BOOL.
    it should "return S_BOOL for S_BOOL with S_BOOL" in {
        val types = List(S_BOOL, S_BOOL)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_BOOL)
    }

    // Test case for S_CHAR with S_CHAR.
    it should "return S_CHAR for S_CHAR with S_CHAR" in {
        val types = List(S_CHAR, S_CHAR)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_CHAR)
    }

    // Test case for S_INT with S_INT.
    it should "return S_INT for S_INT with S_INT" in {
        val types = List(S_INT, S_INT)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_INT)
    }

    // Test case for S_BOOL with S_INT.
    it should "return S_ANY for S_BOOL with S_INT" in {
        val types = List(S_BOOL, S_INT)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for S_CHAR with S_INT.
    it should "return S_ANY for S_CHAR with S_INT" in {
        val types = List(S_CHAR, S_INT)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for S_BOOL with S_CHAR.
    it should "return S_ANY for S_BOOL with S_CHAR" in {
        val types = List(S_BOOL, S_CHAR)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for S_BOOL with S_PAIR.
    it should "return S_ANY for S_BOOL with S_PAIR" in {
        val types = List(S_BOOL, S_PAIR(S_ANY, S_ANY))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for S_PAIR with S_PAIR.
    it should "return S_PAIR for S_PAIR with S_PAIR" in {
        val types = List(S_PAIR(S_ANY, S_ANY), S_PAIR(S_ANY, S_ANY))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_PAIR(S_ANY, S_ANY))
    }

    // Test case for S_STRING with S_ARRAY(S_CHAR).
    it should "return S_STRING for S_STRING with S_ARRAY(S_CHAR)" in {
        val types = List(S_STRING, S_ARRAY(S_CHAR))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_STRING)
    }

    // Test case for a list with S_PAIR and S_CHAR.
    it should "return S_ANY for a list with S_PAIR and S_CHAR" in {
        val types = List(S_PAIR(S_ANY, S_ANY), S_CHAR)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for a list with S_PAIR, S_INT, and S_BOOL.
    it should "return S_ANY for a list with S_PAIR, S_INT, and S_BOOL" in {
        val types = List(S_PAIR(S_ANY, S_ANY), S_INT, S_BOOL)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for a list with S_PAIR, S_CHAR, and S_BOOL.
    it should "return S_ANY for a list with S_PAIR, S_CHAR, and S_BOOL" in {
        val types = List(S_PAIR(S_ANY, S_ANY), S_CHAR, S_BOOL)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    it should "return S_PAIR(S_INT, S_BOOL) for a list with S_PAIR(S_INT, S_BOOL) and S_PAIR(S_INT, S_BOOL)" in {
        val types = List(S_PAIR(S_INT, S_BOOL), S_PAIR(S_INT, S_BOOL))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_PAIR(S_INT, S_BOOL))
    }

    // Test case for a list with nested S_PAIR.
    it should "return S_PAIR(S_PAIR(S_ANY, S_ANY), S_ANY) for a list with nested S_PAIR" in {
        val types = List(S_PAIR(S_PAIR(S_ANY, S_ANY), S_INT), S_PAIR(S_PAIR(S_ANY, S_ANY), S_BOOL))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_PAIR(S_PAIR(S_ANY, S_ANY), S_ANY))
    }

    // Test case for a list with nested S_PAIR and S_INT.
    it should "return S_ANY for a list with nested S_PAIR and S_INT" in {
        val types = List(S_PAIR(S_PAIR(S_ANY, S_ANY), S_INT), S_INT)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for a list with nested S_PAIR and S_BOOL.
    it should "return S_ANY for a list with nested S_PAIR and S_BOOL" in {
        val types = List(S_PAIR(S_PAIR(S_ANY, S_ANY), S_INT), S_BOOL)
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ANY)
    }

    // Test case for a list with S_ARRAY(S_INT) and S_ARRAY(S_INT).
    it should "return S_ARRAY(S_INT) for a list with S_ARRAY(S_INT) and S_ARRAY(S_INT)" in {
        val types = List(S_ARRAY(S_INT), S_ARRAY(S_INT))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ARRAY(S_INT))
    }

    // Test case for a list with S_ARRAY(S_INT), S_ARRAY(S_BOOL), and S_ARRAY(S_CHAR).
    it should "return S_ANY for a list with S_ARRAY(S_INT), S_ARRAY(S_BOOL), and S_ARRAY(S_CHAR)" in {
        val types = List(
        S_ARRAY(S_INT), 
        S_ARRAY(S_BOOL),
        S_ARRAY(S_CHAR)
        )
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ARRAY(S_ANY))
    }

    // Test case for a list with a nested array S_ARRAY(S_ARRAY(S_INT)).
    it should "return S_ARRAY(S_ARRAY(S_INT)) for a list with a nested array" in {
        val types = List(S_ARRAY((S_ARRAY(S_INT))))
        val ancestor = sem.getLowestCommonAncestor(types)
        ancestor should be (S_ARRAY(S_ARRAY(S_INT)))
    }

}
