package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.semantics._

class semanticsTests extends AnyFlatSpec {
// Tests for findCommonAncestor.
// Test case for S_BOOL with S_BOOL.
  "findCommonAncestor" should "return S_BOOL for S_BOOL with S_BOOL" in {
    val ancestor = findCommonAncestor(S_BOOL, S_BOOL)
    ancestor should be (S_BOOL)
  }

  // Test case for S_CHAR with S_CHAR.
  it should "return S_CHAR for S_CHAR with S_CHAR" in {
    val ancestor = findCommonAncestor(S_CHAR, S_CHAR)
    ancestor should be (S_CHAR)
  }

  // Test case for S_INT with S_INT.
  it should "return S_INT for S_INT with S_INT" in {
    val ancestor = findCommonAncestor(S_INT, S_INT)
    ancestor should be (S_INT)
  }

  // Test case for S_BOOL with S_INT.
  it should "return S_ANY for S_BOOL with S_INT" in {
    val ancestor = findCommonAncestor(S_BOOL, S_INT)
    ancestor should be (S_ANY)
  }

  // Test case for S_CHAR with S_INT.
  it should "return S_ANY for S_CHAR with S_INT" in {
    val ancestor = findCommonAncestor(S_CHAR, S_INT)
    ancestor should be (S_ANY)
  }

  // Test case for S_BOOL with S_CHAR.
  it should "return S_ANY for S_BOOL with S_CHAR" in {
    val ancestor = findCommonAncestor(S_BOOL, S_CHAR)
    ancestor should be (S_ANY)
  }

  // Test case for S_BOOL with S_PAIR.
  it should "return S_ANY for S_BOOL with S_PAIR" in {
    val ancestor = findCommonAncestor(S_BOOL, S_PAIR(S_INT, S_BOOL))
    ancestor should be (S_ANY)
  }

  // Test case for S_PAIR with S_PAIR.
  it should "return S_PAIR for S_PAIR with S_PAIR" in {
    val ancestor = findCommonAncestor(S_PAIR(S_INT, S_BOOL), S_PAIR(S_BOOL, S_CHAR))
    ancestor should be (S_PAIR(S_ANY, S_ANY))
  }

  // Test case for S_STRING with S_ARRAY(S_CHAR).
  it should "return S_STRING for S_STRING with S_ARRAY(S_CHAR)" in {
    val ancestor = findCommonAncestor(S_STRING, S_ARRAY(S_CHAR))
    ancestor should be (S_STRING)
  }

// Tests for getLowestCommonAncestor.
// Test case for S_BOOL with S_BOOL.
  "getLowestCommonAncestor" should "return S_BOOL for S_BOOL with S_BOOL" in {
    val expressions = List(BoolL(true), BoolL(false))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_BOOL)
  }

  // Test case for S_CHAR with S_CHAR.
  it should "return S_CHAR for S_CHAR with S_CHAR" in {
    val expressions = List(CharL('a'), CharL('b'))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_CHAR)
  }

  // Test case for S_INT with S_INT.
  it should "return S_INT for S_INT with S_INT" in {
    val expressions = List(IntL(1), IntL(2))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_INT)
  }

  // Test case for S_BOOL with S_INT.
  it should "return S_ANY for S_BOOL with S_INT" in {
    val expressions = List(BoolL(true), IntL(1))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for S_CHAR with S_INT.
  it should "return S_ANY for S_CHAR with S_INT" in {
    val expressions = List(CharL('a'), IntL(1))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for S_BOOL with S_CHAR.
  it should "return S_ANY for S_BOOL with S_CHAR" in {
    val expressions = List(BoolL(true), CharL('a'))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for S_BOOL with S_PAIR.
  it should "return S_ANY for S_BOOL with S_PAIR" in {
    val expressions = List(BoolL(true), PairL())
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for S_PAIR with S_PAIR.
  it should "return S_PAIR for S_PAIR with S_PAIR" in {
    val expressions = List(PairL(), PairL())
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_PAIR(S_ANY, S_ANY))
  }

  // Test case for S_STRING with S_ARRAY(S_CHAR).
  ignore should "return S_STRING for S_STRING with S_ARRAY(S_CHAR)" in {
    // val expressions: List[Expr] = List(StrL("abc"), ArrL(List(CharL('a'), CharL('b'))))
    // val ancestor = getLowestCommonAncestor(expressions)
    // ancestor should be (S_STRING)
  }
}