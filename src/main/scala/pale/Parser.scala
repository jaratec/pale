package pale

import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers with PackratParsers {

  def number: Parser[Num] = wholeNumber ^^ {n => Num(n.toLong)}

  def str: Parser[Str] = stringLiteral ^^ {s => Str(s.init.tail)}

  def bool: Parser[Bool] = (
    """(?i)\Qtrue\E""".r  ^^ {_ => Bool(true)}
    | """(?i)\Qfalse\E""".r ^^ {_ => Bool(false)}
  )

  def value: Parser[Val] = number | str | get | refCall | ref | arr | bool | id

  def ret: Parser[Return] = ("return" | "Return") ~> opt(bool)  ^^ {e => Return(e)}

  def assign: Parser[Assign] = (
    id ~ "=" ~ expr ^^ {case lvalue ~ "=" ~ rvalue => AssignExprToId(lvalue, rvalue)}
    | get ~ "=" ~ expr ^^ {case lvalue ~ "=" ~ rvalue => AssignExprToGet(lvalue, rvalue)}
    | ref ~ "=" ~ expr ^^ {case lvalue ~ "=" ~ rvalue => AssignExprToRef(lvalue, rvalue)}
    | arr ~ "=" ~ expr ^^ {case lvalue ~ "=" ~ rvalue => AssignExprToArr(lvalue, rvalue)}
  )

  def id: Parser[Id] = ident ^^ {i => Id(i)}

  def call: Parser[Call] = (
    ident ~ "." ~ fun ^^ {case obj ~ "." ~ fun => Call(obj, fun)}
    | fun ^^ {fun => Call("self", fun)} // self? this?
  )

  def ref: Parser[Ref] = ident ~ "." ~ "\"" ~ (ident | """[0-9a-zA-Z]+""".r) <~ "\"" ^^ {case obj ~ "." ~ "\"" ~ column => Ref(obj, column)}

  def refCall: Parser[RefCall] = ref ~ "." ~ fun ^^ {case r ~ "." ~ f => RefCall(r,f)}

  def get: Parser[Get] = (
    ref <~ ".value()" ^^ {obj => GetRefValue(obj)}
    | ident <~ ".value()" ^^ {obj => GetValue(obj)}
    | ident <~ ".name()" ^^ {obj => GetName(obj)}
    | ident <~ ".VALUE" ^^ {obj => GetValue(obj)}
    | ident ~ "." ~ ("visible" | "Readonly" | "readonly") ^^ {case obj ~ _ ~ attr => GetAttribute(obj, attr.toLowerCase)}
  )
  
  def arr: Parser[Arr] = ident ~ "[" ~ (number | str) <~ "]" ^^ {case name ~ "[" ~ v => Arr(name, v)}

  def fun: Parser[Fun] = ident ~ "(" ~ opt(repsep((comparison | expr), ",")) <~ ")" ^^ {
    case name ~ "(" ~ None => Fun(name, List())
    case name ~ "(" ~ Some(arguments) => Fun(name, arguments)
  }

  lazy val expr: PackratParser[Expr] = arithExpr | number | str | get | refCall | ref | call | arr | bool | id // leaving out cond

  def arithOp: Parser[Op] = ("+" | "-" | "*" | "/") ^^ {op => Op(op)}
  
  lazy val arithExpr: PackratParser[ArithExpr] = expr ~ arithOp ~ expr ^^ {case e1 ~ op ~ e2 => ArithExpr(op, e1, e2)}

  def compOp: Parser[Op] = ("<>" | "<=" | ">=" | "=" | "<" | ">") ^^ {op => Op(op)}

  def comparison: Parser[Comparison] = expr ~ compOp ~ expr ^^ {case lvalue ~ op ~ rvalue => ExprToExprComparison(op, lvalue, rvalue)}

  def basicCond: Parser[Cond] = (
    comparison ^^ {case c => CompareCond(c)}
    | call ^^ {case c => CallCond(c)}
    | refCall ^^ {case c => RefCallCond(c)}
    | id ^^ {case c => VarCond(c)}
  )

  lazy val notCond: PackratParser[Cond] = """(?i)\Qnot\E""".r ~> subCond ^^ {condition => NegCond(condition)}

  lazy val andCond: PackratParser[Cond] = subCond ~ """(?i)\Qand\E""".r ~ rep1sep(subCond, """(?i)\Qand\E""".r) ^^ {case first ~ _ ~ conditions => AndCond(first::conditions)}

  lazy val orCond: PackratParser[Cond] = subCond ~ """(?i)\Qor\E""".r ~ rep1sep(subCond, """(?i)\Qor\E""".r) ^^ {case first ~ _ ~ conditions => OrCond(first::conditions)}
  
  lazy val ands: PackratParser[Cond] = orCond ~ """(?i)\Qand\E""".r ~ rep1sep(orCond, """(?i)\Qand\E""".r) ^^ {case first ~ _ ~ conditions => AndCond(first::conditions)}

  lazy val ors: PackratParser[Cond] = andCond ~ """(?i)\Qor\E""".r ~ rep1sep(andCond, """(?i)\Qor\E""".r) ^^ {case first ~ _ ~ conditions => OrCond(first::conditions)}

  def subCond: Parser[Cond] = ("(" ~> notCond <~ ")") | notCond | ("(" ~> basicCond <~ ")") | basicCond | ("(" ~> (ands| ors| andCond | orCond) <~ ")") | (ands | ors | andCond | orCond)
  def composedCond: Parser[Cond] = ands | ors | andCond | orCond | notCond
  def cond: Parser[Cond] = composedCond | ("(" ~> composedCond <~ ")") | basicCond | ("(" ~> basicCond <~ ")")

  def `if`: Parser[If] = ("if" | "If" | "IF") ~> cond ~ """(?i)\Qthen\E""".r ~ rep(statement) ~ opt("else" ~> rep(statement)) <~ ("endif" | "Endif" | "endIf") ^^ {
    case condition ~ _ ~ thenBody ~ elseBody => If(condition, thenBody, elseBody)
  }

  def `case`: Parser[Case] = ("case" | "Case") ~> cond ~ ":" ~ opt(rep(statement)) ^^ {case condition ~ ":" ~ body => Case(condition, body)}

  def switch: Parser[Switch] = ("Switch" | "switch") ~> rep(`case`) ~ opt("otherwise" ~> ":" ~> rep(statement)) <~ ("endSwitch" | "endswitch") ^^ {
    case conditions ~ default => Switch(conditions, default)
  }

  def loop: Parser[For] = "for" ~> ident ~ "from" ~ expr ~ "to" ~ expr ~ rep(statement) <~ ("endfor" | "endFor") ^^ {
    case name ~ "from" ~ start ~ "to" ~ end ~ body => For(name, start, end, body)
  }

  def scan: Parser[Scan] = "scan" ~> ident ~ "for" ~ ref ~ "=" ~ expr ~ ":" ~ rep(statement)  <~ "endscan" ^^ {
    case table ~ "for" ~ column ~ "=" ~ e ~ ":" ~ body => Scan(table, column, e, body)
  }

  def `try`: Parser[Try] = "try" ~> rep(statement) ~ "onfail" ~ rep(statement) <~ "endtry" ^^ {
    case tryBlock ~ _ ~ catchBlock => Try(tryBlock, catchBlock)
  }

  def command: Parser[Command] = ("""(?i)\Qquitloop\E""".r | """(?i)\Qreturn\E""".r) ^^ {name => Command(name)}

  def statement: Parser[Statement] = ret | `if` | switch | loop | scan | `try` | assign | call | command // temporarily disable commands (make a list of known commands)

  def typeName: Parser[String] = "MoveEvent" | """(?i)\Qanytype\E""".r | "string" | "String" | """(?i)\Qsmallint\E""".r | "Logical" | "logical" | "tcursor" | "library"
  def `type`: Parser[Type] = opt("dynarray[]" | ("Array[]") | ("Array[" ~> wholeNumber <~ "]")) ~ typeName ^^ {
    case None ~ name => SimpleType(name)
    case Some("dynarray[]") ~ name => ArrayType(name, None)
    case Some("Array[]") ~ name => ArrayType(name, Some(0)) // not sure this is the correct choice (seems to be more of an array without fixed size)
    case Some(size) ~ name => ArrayType(name, Some(size.toInt))
  }

  def variable: Parser[Var] = ident ~ `type` ^^ {case name ~ t => Var(name, t)}

  def variables: Parser[List[Var]] = repsep(ident, ",") ~ `type` ^^ {
    case ids ~ t => ids.map(Var(_, t))
  }

  def args: Parser[Args] = "var" ~> repsep(variable, ",") ^^ {vs => Args(vs)}

  def oneOrMoreVars: Parser[List[Var]] = (
    variables ^^ {vs => vs}
    | variable ^^ {v => List(v)}
  )

  def varBlock: Parser[VarBlock] = "var" ~> opt(rep(oneOrMoreVars)) <~ ("endvar" | "endVar") ^^ {
    case None => VarBlock(List())
    case Some(vs) => VarBlock(vs.flatten)
  }

  def meth: Parser[Meth] = "method" ~> ident ~ "(" ~ opt(args) ~ ")" ~ opt(varBlock) ~ rep(statement) <~ ("endmethod" | "endMethod") ^^ {
    case name ~ "(" ~ arguments ~ ")" ~ localVars ~ body => Meth(name, arguments, localVars, body)
  }

  def field: Parser[Field] = "FIELD." ~> ident ~ ".pg1." ~ ident ~ meth <~ "ENDSOURCE" ^^ {
    case table ~ ".pg1." ~ column ~ method => Field(table, column, method)
  }

  def fiche: Parser[Fiche] = "FICHE." ~> ident ~ opt(varBlock) ~ rep(meth) <~ "ENDSOURCE" ^^ {
    case table ~ globalVars ~ methods => Fiche(table, globalVars, methods)
  }

  def form: Parser[Form] = fiche | field

  def objectpal: Parser[List[Form]] = rep(form)

  def parse(str: String): List[Form] = {
    parseAll(objectpal, str) match {
      // case Failure(msg, next) => println("Could not parse - " + msg); List()
      case Error(msg, next) => println("Could not parse - " + msg); List()
      case Success(result, next) => result
    }
  }

}
