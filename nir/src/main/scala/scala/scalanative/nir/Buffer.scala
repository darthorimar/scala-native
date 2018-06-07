package scala.scalanative
package nir

import scala.collection.mutable

class Buffer(implicit fresh: Fresh) {
  private val buffer = mutable.UnrolledBuffer.empty[Inst]
  def +=(inst: Inst): Unit =
    buffer += inst
  def ++=(insts: Seq[Inst]): Unit =
    buffer ++= insts
  def ++=(other: Buffer): Unit =
    buffer ++= other.buffer
  def toSeq: Seq[Inst] =
    buffer

  // Control-flow ops
  def label(name: Local, loc: Location.Location): Unit =
    this += Inst.Label(name, Seq.empty, loc)
  def label(name: Local, params: Seq[Val.Local], loc: Location.Location): Unit =
    this += Inst.Label(name, params, loc)
  def unreachable(loc: Location.Location): Unit =
    this += Inst.Unreachable(loc)
  def ret(value: Val, loc: Location.Location): Unit =
    this += Inst.Ret(value, loc)
  def jump(next: Next, loc: Location.Location): Unit =
    this += Inst.Jump(next, loc)
  def jump(to: Local, args: Seq[Val], loc: Location.Location): Unit =
    this += Inst.Jump(Next.Label(to, args), loc)
  def branch(value: Val, thenp: Next, elsep: Next, loc: Location.Location): Unit =
    this += Inst.If(value, thenp, elsep, loc)
  def switch(value: Val, default: Next, cases: Seq[Next], loc: Location.Location): Unit =
    this += Inst.Switch(value, default, cases, loc)
  def raise(value: Val, unwind: Next, loc: Location.Location): Unit =
    this += Inst.Throw(value, unwind, loc)

  // Compute ops
  def let(name: Local, op: Op, loc: Location.Location): Val = {
    this += Inst.Let(name, op, loc)
    Val.Local(name, op.resty)
  }
  def let(op: Op, loc: Location.Location): Val =
    let(fresh(), op, loc)
  def call(ty: Type, ptr: Val, args: Seq[Val], unwind: Next, loc: Location.Location): Val =
    let(Op.Call(ty, ptr, args, unwind), loc)
  def load(ty: Type, ptr: Val,  loc: Location.Location, isVolatile: Boolean = false): Val =
    let(Op.Load(ty, ptr, isVolatile), loc)
  def store(ty: Type, ptr: Val, value: Val,  loc: Location.Location, isVolatile: Boolean = false): Val =
    let(Op.Store(ty, ptr, value, isVolatile), loc)
  def elem(ty: Type, ptr: Val, indexes: Seq[Val], loc: Location.Location): Val =
    let(Op.Elem(ty, ptr, indexes), loc)
  def extract(aggr: Val, indexes: Seq[Int], loc: Location.Location): Val =
    let(Op.Extract(aggr, indexes), loc)
  def insert(aggr: Val, value: Val, indexes: Seq[Int], loc: Location.Location): Val =
    let(Op.Insert(aggr, value, indexes), loc)
  def stackalloc(ty: Type, n: Val, loc: Location.Location): Val =
    let(Op.Stackalloc(ty, n), loc)
  def bin(bin: nir.Bin, ty: Type, l: Val, r: Val, loc: Location.Location): Val =
    let(Op.Bin(bin, ty, l, r), loc)
  def comp(comp: nir.Comp, ty: Type, l: Val, r: Val, loc: Location.Location): Val =
    let(Op.Comp(comp, ty, l, r), loc)
  def conv(conv: nir.Conv, ty: Type, value: Val, loc: Location.Location): Val =
    let(Op.Conv(conv, ty, value), loc)
  def select(cond: Val, thenv: Val, elsev: Val, loc: Location.Location): Val =
    let(Op.Select(cond, thenv, elsev), loc)
  def classalloc(name: Global, loc: Location.Location): Val =
    let(Op.Classalloc(name), loc)
  def field(obj: Val, name: Global, loc: Location.Location): Val =
    let(Op.Field(obj, name), loc)
  def method(obj: Val, name: Global, loc: Location.Location): Val =
    let(Op.Method(obj, name), loc)
  def dynmethod(obj: Val, signature: String, loc: Location.Location): Val =
    let(Op.Dynmethod(obj, signature), loc)
  def module(name: Global, unwind: Next, loc: Location.Location): Val =
    let(Op.Module(name, unwind), loc)
  def as(ty: Type, obj: Val, loc: Location.Location): Val =
    let(Op.As(ty, obj), loc)
  def is(ty: Type, obj: Val, loc: Location.Location): Val =
    let(Op.Is(ty, obj), loc)
  def copy(value: Val, loc: Location.Location): Val =
    let(Op.Copy(value), loc)
  def sizeof(ty: Type, loc: Location.Location): Val =
    let(Op.Sizeof(ty), loc)
  def closure(ty: Type, fun: Val, captures: Seq[Val], loc: Location.Location): Val =
    let(Op.Closure(ty, fun, captures), loc)
  def box(ty: Type, obj: Val, loc: Location.Location): Val =
    let(Op.Box(ty, obj), loc)
  def unbox(ty: Type, obj: Val, loc: Location.Location): Val =
    let(Op.Unbox(ty, obj), loc)
}
