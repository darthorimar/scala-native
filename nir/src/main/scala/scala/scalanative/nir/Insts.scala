package scala.scalanative
package nir

sealed abstract class Inst extends Location.WithLocation {
  final def show: String = nir.Show(this)
}

object Inst {
  final case object None extends Inst {
    override def loc: Location.Location = Location.NoLoc
  }
  final case class Label(name: Local, params: Seq[Val.Local], loc: Location.Location) extends Inst
  final case class Let(name: Local, op: Op, loc: Location.Location)                   extends Inst
  object Let {
    def apply(op: Op, loc: Location.Location)(implicit fresh: Fresh): Let = Let(fresh(), op, loc)
  }

  sealed abstract class Cf                                  extends Inst
  final case class Unreachable(loc: Location.Location)                              extends Cf
  final case class Ret(value: Val, loc: Location.Location)                          extends Cf
  final case class Jump(next: Next, loc: Location.Location)                         extends Cf
  final case class If(value: Val, thenp: Next, elsep: Next, loc: Location.Location) extends Cf
  final case class Switch(value: Val, default: Next, cases: Seq[Next], loc: Location.Location)
      extends Cf
  final case class Throw(value: Val, unwind: Next, loc: Location.Location) extends Cf
}
