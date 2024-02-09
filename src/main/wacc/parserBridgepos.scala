package wacc

import parsley.Parsley
import parsley.position.pos
import parsley.syntax.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.ap.{ap1, ap2, ap3, ap4}
import parsley.generic

// trait ParserBridgePos0[+A] {
//     def apply()(pos: (Int, Int)): A 
//     // private def con(pos: (Int, Int)): A = this.apply()(pos)

//     // def apply(): Parsley[A] = pos.map(con)
//     // def from(op: Parsley[_]): Parsley[A] = pos.map((x, y) =>()) <~ op
//     // final def <#(op: Parsley[_]): Parsley[A] = this from op


// }

trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <~ op
    final def <#(op: Parsley[_]): Parsley[A] = this from op
}

trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = ap1(pos.map(con), x)

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
}

trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C]{
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] = ap2(pos.map(con), x, y)

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)

}

trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: Parsley[B],z: Parsley[C]): Parsley[D] = ap3(pos.map(con), x, y, z)

    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E]{
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], w: Parsley[D]): Parsley[E] = 
        ap4(pos.map(con), x, y, z, w)

    override final def con(pos:(Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
}
