package rainier.compute.asm

import rainier.compute

import scala.collection.mutable

sealed trait IR

case class Parameter(original: compute.Variable) extends IR
case class Const(value: Double) extends IR

case class BinaryIR(left: IR, right: IR, op: compute.BinaryOp) extends IR
case class UnaryIR(original: IR, op: compute.UnaryOp) extends IR

case class Sym private (id: Int)
object Sym {
  private var curIdx = 0
  def freshSym(): Sym = {
    val r = Sym(curIdx)
    curIdx += 1
    r
  }
}
case class VarDef(sym: Sym, rhs: IR) extends IR
case class VarRef(sym: Sym) extends IR

case class MethodDef(sym: Sym, rhs: IR) extends IR
case class MethodRef(sym: Sym) extends IR

object IR {
  private val alreadySeen: mutable.Map[compute.Real, Sym] = mutable.Map.empty
  private val symVarDef: mutable.Map[Sym, VarDef] = mutable.Map.empty
  private val symMethodDef: mutable.Map[Sym, MethodDef] = mutable.Map.empty
  def toIR(r: compute.Real): IR = {
    if (alreadySeen.contains(r))
      VarRef(alreadySeen(r))
    else
      r match {
        case compute.Constant(value) => Const(value)
        // variable access is treated like an atomic operation and is not stored in a VarDef
        case v: compute.Variable => Parameter(v)
        case b: compute.BinaryReal =>
          val bIR = BinaryIR(toIR(b.left), toIR(b.right), b.op)
          createVarDefFromOriginal(b, bIR)
        case u: compute.UnaryReal =>
          val uIR = UnaryIR(toIR(u.original), u.op)
          createVarDefFromOriginal(u, uIR)
      }
  }
  val methodSizeLimit = 20
  def packIntoMethods(p: IR): (MethodRef, Set[MethodDef]) = {
    def internalTraverse(p: IR): (IR, Int) = p match {
      case c: Const     => (c, 1)
      case v: Parameter => (v, 1)
      case vd: VarDef =>
        val (traversedRhs, rhsSize) =
          traverseAndMaybePack(vd.rhs, methodSizeLimit - 1)
        (VarDef(vd.sym, traversedRhs), rhsSize + 1)
      case vr: VarRef =>
        (vr, 1)
      case b: BinaryIR =>
        val (leftIR, leftSize) =
          traverseAndMaybePack(b.left, methodSizeLimit / 2)
        val (rightIR, rightSize) =
          traverseAndMaybePack(b.right, methodSizeLimit / 2)
        (BinaryIR(leftIR, rightIR, b.op), leftSize + rightSize + 1)
      case u: UnaryIR =>
        val (traversedIR, irSize) =
          traverseAndMaybePack(u.original, methodSizeLimit - 1)
        (traversedIR, irSize + 1)
    }
    def traverseAndMaybePack(p: IR, localSizeLimit: Int): (IR, Int) = {
      val (pt, size) = internalTraverse(p)
      if (size >= localSizeLimit)
        (packIntoMethod(pt), 1)
      else
        (pt, size)
    }
    val (pSpillOver, _) = internalTraverse(p)
    val packedSpillOverRef = packIntoMethod(pSpillOver)
    (packedSpillOverRef, symMethodDef.values.toSet)
  }

  private def createVarDefFromOriginal(original: compute.Real,
                                       rhs: IR): VarDef = {
    val s = Sym.freshSym()
    val vd = VarDef(s, rhs)
    alreadySeen(original) = s
    symVarDef(s) = vd
    vd
  }
  private def packIntoMethod(rhs: IR): MethodRef = {
    val s = Sym.freshSym()
    val md = MethodDef(s, rhs)
    symMethodDef(s) = md
    MethodRef(s)
  }

  abstract class ForeachTreeTraverse {
    def traverse(ir: IR): Unit = ir match {
      // leaves
      case (_: Const | _: Parameter | _: VarRef | _: MethodRef) =>
      case vd: VarDef =>
        traverse(vd.rhs)
      case b: BinaryIR =>
        traverse(b.left)
        traverse(b.right)
      case u: UnaryIR =>
        traverse(u.original)
      case md: MethodDef =>
        traverse(md.rhs)
    }
  }

  abstract class ForeachDagTraverse {
    def traverse(ir: IR): Unit = ir match {
      // atomic values
      case (_: Const | _: Parameter) =>
      case VarRef(sym) =>
        traverse(symVarDef(sym))
      case MethodRef(sym) =>
        traverse(symMethodDef(sym))
      case vd: VarDef =>
        traverse(vd.rhs)
      case b: BinaryIR =>
        traverse(b.left)
        traverse(b.right)
      case u: UnaryIR =>
        traverse(u.original)
      case md: MethodDef =>
        traverse(md.rhs)
    }
  }

  case class SymStats(sym: Sym, rawStats: Map[Sym, Int]) {
    lazy val numReferences: Int = rawStats.values.sum
    lazy val referringMethods: Set[Sym] = rawStats.keySet
  }

  case class DepStats(symStats: Map[Sym, SymStats])
  object DepStats {
    def apply(ps: Seq[MethodDef]): DepStats = {
      val m: mutable.Map[Sym, mutable.Map[Sym, Int]] = mutable.Map.empty
        .withDefaultValue(mutable.Map.empty.withDefaultValue(0))
      object markDeps extends ForeachDagTraverse {
        var currentMethodDef: Sym = _
        override def traverse(ir: IR): Unit = ir match {
          case MethodDef(sym, _) =>
            val saved = currentMethodDef
            currentMethodDef = sym
            super.traverse(ir)
            currentMethodDef = saved
          case MethodRef(sym) =>
            super.traverse(ir)
          case VarRef(sym) =>
            m(sym)(currentMethodDef) += 1
            super.traverse(ir)
          case VarDef(sym, _) =>
            m(sym)(currentMethodDef) += 1
            super.traverse(ir)
          case _ =>
            super.traverse(ir)
        }
      }
      ps.foreach { p =>
        markDeps.traverse(p)
      }
      val symStats = m.toMap.map {
        case (sym, rawSingleSymStats) =>
          (sym, SymStats(sym, rawSingleSymStats.toMap))
      }
      DepStats(symStats)
    }
  }
}
