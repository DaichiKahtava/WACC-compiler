package wacc

import parsley.Parsley
import parsley.position.pos
import parsley.syntax.zipped.{Zipped2, Zipped3, Zipped4}
import parsley.ap._

trait ParserBridgePos0[+A] {
    protected def con(pos: (Int, Int)): A
    def from(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
    final def <#(op: Parsley[_]): Parsley[A] = this from op
}

trait ParserBridgePos1[-A, +B] {
    def apply(x: A)(pos: (Int, Int)): B
    private def con(pos: (Int, Int)): A => B = this(_)(pos)

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this(_) _)
    def from(op: Parsley[_]): Parsley[A => B] = pos.map(con) <~ op
    final def <#(op: Parsley[_]): Parsley[A => B] = this from op
}

trait ParserBridgePos2[-A, -B, +C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    private def con(pos: (Int, Int)): (A, B) => C = this(_, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] = pos <**> (x, y).zipped(this(_, _) _)
    def from(op: Parsley[_]): Parsley[(A, B) => C] = pos.map(con) <* op
    final def <#(op: Parsley[_]): Parsley[(A, B) => C] = this from op
}

trait ParserBridgePos3[-A, -B, -C, +D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    private def con(pos: (Int, Int)): (A, B, C) => D = this(_, _, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B],z: Parsley[C]): Parsley[D] = pos <**> (x, y, z).zipped(this(_, _, _) _)
    def from(op: Parsley[_]): Parsley[(A, B, C) => D] = pos.map(con) <* op
    final def <#(op: Parsley[_]): Parsley[(A, B, C) => D] = this from op
}

trait ParserBridgePos4[-A, -B, -C, -D, +E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    private def con(pos:(Int, Int)): (A, B, C, D) => E = this(_, _, _, _)(pos)

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], w: Parsley[D]): Parsley[E] = 
        pos <**> (x, y, z, w).zipped(this(_, _, _, _) _)
    def from(op: Parsley[_]): Parsley[(A, B, C, D) => E] = pos.map(con) <* op
    final def <#(op: Parsley[_]): Parsley[(A, B, C, D) => E] = this from op
}
