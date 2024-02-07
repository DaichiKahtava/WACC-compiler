package wacc

import parsley.Parsley
import parsley.position.pos
import parsley.syntax.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.ap._

// trait ParserBridgePos0[+A] {
//     def apply()(pos: (Int, Int)): A 
//     // private def con(pos: (Int, Int)): A = this.apply()(pos)

//     // def apply(): Parsley[A] = pos.map(con)
//     // def from(op: Parsley[_]): Parsley[A] = pos.map((x, y) =>()) <~ op
//     // final def <#(op: Parsley[_]): Parsley[A] = this from op


// }

trait ParserBridgePos1[-A, +B] {
    def apply(x: A)(pos: (Int, Int)): B
    private def con(pos: (Int, Int)): A => B = this.apply(_)(pos)

    def apply(x: Parsley[A]): Parsley[B] = ap1(pos.map(con), x)
    def from(op: Parsley[_]): Parsley[A => B] = pos.map(con) <~ op
    final def <#(op: Parsley[_]): Parsley[A => B] = this from op
}

trait ParserBridgePos2[-A, -B, +C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    private def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] = ap2(pos.map(con), x, y)
    def from(op: Parsley[_]): Parsley[(A, B) => C] = pos.map(con) <~ op
    final def <#(op: Parsley[_]): Parsley[(A, B) => C] = this from op
}

trait ParserBridgePos3[-A, -B, -C, +D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    private def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B],z: Parsley[C]): Parsley[D] = ap3(pos.map(con), x, y, z)
    def from(op: Parsley[_]): Parsley[(A, B, C) => D] = pos.map(con) <~ op
    final def <#(op: Parsley[_]): Parsley[(A, B, C) => D] = this from op
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    private def con(pos:(Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], w: Parsley[D]): Parsley[E] = 
        ap4(pos.map(con), x, y, z, w)
    def from(op: Parsley[_]): Parsley[(A, B, C, D) => E] = pos.map(con) <~ op
    final def <#(op: Parsley[_]): Parsley[(A, B, C, D) => E] = this from op
}
