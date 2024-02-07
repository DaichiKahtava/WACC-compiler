package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.semantics._

class semanticsTests extends AnyFlatSpec {
  // Tests for getLowestCommonAncestor.
  // Test case for an empty list.
  "getLowestCommonAncestor" should "return S_ANY for an empty list" in {
    val expressions = List()
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a single element list of S_BOOL.
  it should "return S_BOOL for a single element list of S_BOOL" in {
    val expressions = List(BoolL(true))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_BOOL)
  }

  // Test case for a single element list of S_INT.
  it should "return S_INT for a single element list of S_INT" in {
    val expressions = List(IntL(1))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_INT)
  }

  // Test case for a single element list of S_CHAR.
  it should "return S_CHAR for a single element list of S_CHAR" in {
    val expressions = List(CharL('a'))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_CHAR)
  }

  // Test case for S_BOOL with S_BOOL.
  it should "return S_BOOL for S_BOOL with S_BOOL" in {
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
  it should "return S_STRING for S_STRING with S_ARRAY(S_CHAR)" in {
    val expressions: List[Expr] = List(StrL("abc"), ArrL(List(CharL('a'), CharL('b'))))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_STRING)
  }

  // Test case for a list with S_PAIR and S_CHAR.
  it should "return S_ANY for a list with S_PAIR and S_CHAR" in {
    val expressions = List(PairL(), CharL('a'))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a list with S_PAIR, S_INT, and S_BOOL.
  it should "return S_ANY for a list with S_PAIR, S_INT, and S_BOOL" in {
    val expressions = List(PairL(), IntL(1), BoolL(true))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a list with S_PAIR, S_CHAR, and S_BOOL.
  it should "return S_ANY for a list with S_PAIR, S_CHAR, and S_BOOL" in {
    val expressions = List(PairL(), CharL('a'), BoolL(true))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  it should "return S_PAIR(S_INT, S_BOOL) for a list with S_PAIR(S_INT, S_BOOL) and S_PAIR(S_INT, S_BOOL)" in {
    val expressions = List(NewPair(IntL(1), BoolL(true)), NewPair(IntL(2), BoolL(false)))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_PAIR(S_INT, S_BOOL))
  }

  // Test case for a list with nested S_PAIR.
  it should "return S_PAIR(S_PAIR(S_ANY, S_ANY), S_ANY) for a list with nested S_PAIR" in {
    val expressions = List(NewPair(PairL(), IntL(1)), NewPair(PairL(), BoolL(true)))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_PAIR(S_PAIR(S_ANY, S_ANY), S_ANY))
  }

  // Test case for a list with nested S_PAIR and S_INT.
  it should "return S_ANY for a list with nested S_PAIR and S_INT" in {
    val expressions = List(NewPair(PairL(), IntL(1)), IntL(2))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a list with nested S_PAIR and S_BOOL.
  it should "return S_ANY for a list with nested S_PAIR and S_BOOL" in {
    val expressions = List(NewPair(PairL(), IntL(1)), BoolL(true))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a list with S_ARRAY(S_INT) and S_ARRAY(S_INT).
  it should "return S_ARRAY(S_INT) for a list with S_ARRAY(S_INT) and S_ARRAY(S_INT)" in {
    val expressions: List[Expr] = List(ArrL(List(IntL(1), IntL(2))), ArrL(List(IntL(3), IntL(4))))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ARRAY(S_INT))
  }

  // Test case for a list with S_ARRAY(S_INT), S_ARRAY(S_BOOL), and S_ARRAY(S_CHAR).
  it should "return S_ANY for a list with S_ARRAY(S_INT), S_ARRAY(S_BOOL), and S_ARRAY(S_CHAR)" in {
    val expressions: List[Expr] = List(
      ArrL(List(IntL(1), IntL(2))), 
      ArrL(List(BoolL(true), BoolL(false))),
      ArrL(List(CharL('a'), CharL('b')))
    )
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ANY)
  }

  // Test case for a list with a nested array S_ARRAY(S_ARRAY(S_INT)).
  it should "return S_ARRAY(S_ARRAY(S_INT)) for a list with a nested array" in {
    val expressions: List[Expr] = List(
      ArrL(List(ArrL(List(IntL(1), IntL(2))), ArrL(List(IntL(3), IntL(4))))))
    val ancestor = getLowestCommonAncestor(expressions)
    ancestor should be (S_ARRAY(S_ARRAY(S_INT)))
  }
}