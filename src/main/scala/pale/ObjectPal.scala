package pale

object Printer {
  val INDENT_SIZE: Int = 2
  val NEW_LINE: String = System.getProperty("line.separator")

  var indents: Int = 0

  def formatBlock(lines: List[ObjectPal]): String = {
    indents += 1
    val result: String = lines.map(indent + _.toString + NEW_LINE).mkString
    indents -= 1
    result
  }

  def indent: String = " " * (indents * INDENT_SIZE)
}

trait ObjectPal {}

trait Expr extends ObjectPal {
  def getNames: List[String] = List()
}

trait Val extends Expr {}

case class Num(val n: Long) extends Val {
  override def toString(): String = n.toString
}

case class Str(val s: String) extends Val {
  override def toString(): String = "\"" + s + "\""
}

case class Bool(val b: Boolean) extends Val {
  override def toString(): String = if (b) "true" else "false"
}

trait Get extends Val {}
case class GetRefValue(val ref: Ref) extends Get {
  override def toString(): String = ref.toString + ".value()"
}
case class GetValue(val obj: String) extends Get {
  override def toString(): String = obj + ".value()"
}
case class GetName(val obj: String) extends Get {
  override def toString(): String = obj + ".name()"
}
case class GetAttribute(val obj: String, val attr: String) extends Get {
  override def toString(): String = obj + "." + attr
}

case class Ref(val obj: String, val column: String) extends Val {
  override def toString(): String = obj + ".\"" + column + "\""
}

case class Arr(val name: String, val index: Val) extends Val {
  override def getNames: List[String] = name :: index.getNames
  override def toString(): String = name + "[" + index.toString + "]"
}

case class Fun(val name: String, val args: List[Expr]) extends ObjectPal {
  def getNames: List[String] = args.flatMap(_.getNames)
  override def toString(): String = name + "(" + args.map(_.toString).mkString(", ") + ")"
}

case class Id(val id: String) extends Val {
  override def getNames: List[String] = List(id)
  override def toString(): String = id
}

trait Statement extends ObjectPal {
  def getNames: List[String] = List()
}

case class Call(val obj: String, val fun: Fun) extends Expr with Statement {
  override def getNames: List[String] = obj :: fun.getNames
  override def toString(): String = obj + "." + fun.toString
}
case class RefCall(val ref: Ref, val fun: Fun) extends Val {
  override def getNames: List[String] = ref.getNames ++ fun.getNames
  override def toString(): String = ref.toString() + "." + fun.toString
}

case class Return(val e: Option[Expr]) extends Statement {
  override def toString(): String = "return " + e.getOrElse("").toString
}

trait Assign extends Statement {
  def lvalue: ObjectPal
  def rvalue: ObjectPal
  override def toString(): String = lvalue.toString + " = " + rvalue.toString
}

case class AssignExprToId(override val lvalue: Id, override val rvalue: Expr) extends Assign {
  override def getNames: List[String] = lvalue.getNames ++ rvalue.getNames
}
case class AssignExprToGet(override val lvalue: Get, override val rvalue: Expr) extends Assign {
  override def getNames: List[String] = lvalue.getNames ++ rvalue.getNames
}
case class AssignExprToRef(override val lvalue: Ref, override val rvalue: Expr) extends Assign {
  override def getNames: List[String] = lvalue.getNames ++ rvalue.getNames
}
case class AssignExprToArr(override val lvalue: Arr, override val rvalue: Expr) extends Assign {
  override def getNames: List[String] = lvalue.getNames ++ rvalue.getNames
}

case class Op(val op: String) extends ObjectPal {
  override def toString(): String = op
}

case class ArithExpr(val op: Op, val left: Expr, val right: Expr) extends Expr {
  override def getNames: List[String] = left.getNames ++ right.getNames
  override def toString(): String = left.toString + " " + op.toString + " " + right.toString
}

trait Comparison extends Expr {
  def op: Op
  def lvalue: ObjectPal
  def rvalue: ObjectPal
  override def toString(): String = lvalue.toString + " " + op.toString + " " + rvalue.toString
}

case class ExprToExprComparison(override val op: Op, override val lvalue: Expr, override val rvalue: Expr) extends Comparison {
  override def getNames: List[String] = lvalue.getNames ++ rvalue.getNames
}

trait Cond extends Expr {}
trait BasicCond extends Cond {}
trait ComposedCond extends Cond {}

case class CompareCond(val c: Comparison) extends BasicCond {
  override def getNames: List[String] = c.getNames
  override def toString(): String = c.toString
}
case class VarCond(val v: Id) extends BasicCond {
  override def getNames: List[String] = v.getNames
  override def toString(): String = v.toString
}
case class CallCond(val c: Call) extends BasicCond {
  override def getNames: List[String] = c.getNames
  override def toString(): String = c.toString
}
case class RefCallCond(val r: RefCall) extends BasicCond {
  override def getNames: List[String] = r.getNames
  override def toString(): String = r.toString
}

case class NegCond(val c: Cond) extends ComposedCond {
  override def getNames: List[String] = c.getNames
  override def toString(): String = "(not " + c.toString + ")"
}
case class OrCond(val cs: List[Cond]) extends ComposedCond {
  override def getNames: List[String] = cs.flatMap(_.getNames)
  override def toString(): String = "(" + cs.map(_.toString).mkString(" or ") + ")"
}
case class AndCond(val cs: List[Cond]) extends ComposedCond {
  override def getNames: List[String] = cs.flatMap(_.getNames)
  override def toString(): String = "(" + cs.map(_.toString).mkString(" and ") + ")"
}

case class If(val cond: Cond, val thenBody: List[Statement], val elseBody: Option[List[Statement]]) extends Statement {
  override def getNames: List[String] = cond.getNames ++ thenBody.flatMap(_.getNames) ++ elseBody.getOrElse(List()).flatMap(_.getNames)
  override def toString(): String = {
    val elseStr: String = elseBody.map(Printer.indent + "else" + Printer.NEW_LINE + Printer.formatBlock(_)).getOrElse("")
    "if " + cond.toString + " then" + Printer.NEW_LINE + Printer.formatBlock(thenBody) + elseStr + Printer.indent + "endif"
  }
}

case class Case(val c: Cond, val body: Option[List[Statement]]) extends ObjectPal {
  def getNames: List[String] = c.getNames ++ body.getOrElse(List()).flatMap(_.getNames)
  override def toString(): String = {
    val bodyStr = body.map(Printer.formatBlock(_)).getOrElse("")
    Printer.indent + "case " + c.toString + " :" + Printer.NEW_LINE + bodyStr
  }
}

case class Switch(val conditions: List[Case], val defaultBody: Option[List[Statement]]) extends Statement {
  override def getNames: List[String] = conditions.flatMap(_.getNames) ++ defaultBody.getOrElse(List()).flatMap(_.getNames)
  override def toString(): String = {
    val defaultStr = defaultBody.map(Printer.formatBlock(_)).getOrElse("")
    "switch" + Printer.NEW_LINE + conditions.map(_.toString).mkString + Printer.indent + "endswitch"
  }
}

case class For(val id: String, val start: Expr, val end: Expr, body: List[Statement]) extends Statement {
  override def getNames: List[String] = body.flatMap(_.getNames) ++ start.getNames ++ end.getNames
  override def toString(): String =
    "for " + id + " from " + start.toString + " to " + end.toString + Printer.NEW_LINE + Printer.formatBlock(body) + Printer.indent + "endfor"
}

case class Scan(val table: String, val column: Ref, val e: Expr, val body: List[Statement]) extends Statement {
  override def getNames: List[String] = body.flatMap(_.getNames) ++ e.getNames
  override def toString(): String =
    "scan " + table + " for " + column + " = " + e.toString + " :" + Printer.NEW_LINE + Printer.formatBlock(body) + Printer.indent + "endscan"
}

case class Try(val tryBlock: List[Statement], val catchBlock: List[Statement]) extends Statement {
  override def getNames: List[String] = tryBlock.flatMap(_.getNames) ++ catchBlock.flatMap(_.getNames)
  override def toString(): String =
    "try" + Printer.NEW_LINE + Printer.formatBlock(tryBlock) + Printer.indent + "onfail" + Printer.NEW_LINE + Printer.formatBlock(catchBlock) + Printer.indent + "endtry"
}

case class Command(val name: String) extends Statement {
  override def toString(): String = name
}

trait Type extends ObjectPal {
}
case class SimpleType(val name: String) extends Type {
  override def toString(): String = name
}
case class ArrayType(val name: String, val size: Option[Int]) extends Type {
  override def toString(): String = size match {
    case None => "dynarray[] " + name
    case Some(s) => "Array[" + s + "] " + name
  }
}

case class Var(val name: String, val t: Type) extends ObjectPal {
  override def toString(): String = name + " " + t
}

case class Args(val vars: List[Var]) extends ObjectPal {
  override def toString(): String = "var " + vars.mkString(", ")
}

case class VarBlock(val vars: List[Var]) extends ObjectPal {
  override def toString(): String = "var" + Printer.NEW_LINE + Printer.formatBlock(vars) + "endvar" + Printer.NEW_LINE
}

case class Meth(val name: String, val args: Option[Args], val localVars: Option[VarBlock], val body: List[Statement]) extends ObjectPal {
  def getNames: List[String] = {
    val allNames: List[String] = body.flatMap(_.getNames).map(_.toLowerCase)
    val namesWithoutArgs: List[String] = allNames.filter(n => ! args.exists(_.vars.map(a => a.name.toLowerCase).contains(n)))
    namesWithoutArgs.filter(n => ! localVars.exists(_.vars.map(v => v.name.toLowerCase).contains(n)))
  }
  override def toString(): String =
    "method " + name + "(" + args.getOrElse("").toString + ")" + Printer.NEW_LINE + localVars.getOrElse("").toString + Printer.formatBlock(body) + "endmethod" + Printer.NEW_LINE
}

trait Form extends ObjectPal {
  def table: String
  def isField: Boolean = false
  def isFiche: Boolean = false
  def getNames: Set[String] = Set()
}
case class Field(override val table: String, val column: String, val meth: Meth) extends Form {
  override def isField: Boolean = true
  override def getNames: Set[String] = meth.getNames.filter{_ != column}.filter{_ != "self"}.toSet
  override def toString(): String = "FIELD." + table + ".pg1." + column + Printer.NEW_LINE + meth.toString + "ENDSOURCE" + Printer.NEW_LINE
}
case class Fiche(override val table: String, val globalVars: Option[VarBlock], val meths: List[Meth]) extends Form {
  override def isFiche: Boolean = true
  override def toString(): String =
    "FICHE." + table + Printer.NEW_LINE + globalVars.getOrElse("").toString + Printer.NEW_LINE + meths.map(_.toString).mkString(Printer.NEW_LINE) + Printer.NEW_LINE + "ENDSOURCE" + Printer.NEW_LINE
}
