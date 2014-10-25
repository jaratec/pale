package fix

import java.io._
import scala.io.Source

import pale._

object AddMissingForms {

  val matrix: Map[String, Var] = Map(
    "tc_tabl" -> Var("tc_tabl", SimpleType("tcursor")),
    "tc_tabl2" -> Var("tc_tabl2", SimpleType("tcursor")),
    "tc_tabl3" -> Var("tc_tabl3", SimpleType("tcursor")),
    "st" -> Var("st", SimpleType("string")),
    "codebanque" -> Var("codebanque", SimpleType("string")), // longint?
    "sav111" -> Var("sav111", SimpleType("string")), // initialized to " " in method open()
    "sit" -> Var("sit", SimpleType("string")),
    "tab" -> Var("tab", SimpleType("string")),
    "facultatif" -> Var("facultatif", SimpleType("logical")),
    "inter" -> Var("inter", ArrayType("anytype", None)),
    "ws" -> Var("ws", ArrayType("anytype", None)), // initialized
    // cannotdepart // error code - logical? int? string?
    "ar04305" -> Var("ar04305", ArrayType("string", Some(22))), // initialized
    "st04305" -> Var("st04305", SimpleType("string")), // initialized
    "i" -> Var("i", SimpleType("smallint")),
    "bl" -> Var("bl", SimpleType("library")),
    "tca900" -> Var("tca900", SimpleType("tcursor")),
    "tc_510" -> Var("tc_510", SimpleType("tcursor")),
    "val_lori" -> Var("val_lori", SimpleType("smallint"))
  )

  val parser = new Parser()

  def load(fileName: String): List[Form] =  {
    var lst: List[Form] = List()
    try {
      val str = Source.fromFile(fileName, "UTF-8").mkString
      lst = parser.parse(str)
      println("expressions read:" + lst.size)
    } catch {
      case e: Exception => e.printStackTrace()
    }
    lst
  }

  def groupForms(forms: List[Form]): Map[String, List[Form]] = forms.filter(_.isField).groupBy(_.table)
 
  def getFormVars(groups: Map[String, List[Form]]): List[(String, Set[String], List[Form])] = {
    (for ((k,vs) <- groups) yield (k, vs.flatMap(_.getNames).toSet, vs)).toList
  }

  def pickVars(vs: Set[String]): List[Var] = vs.filter(matrix.contains(_)).map(matrix(_)).toList

  def initVars(vs: List[Var]): List[Meth] = {
    val ns: List[String] = vs.map(_.name).filter(n => List("sav111", "ws", "ar04305", "st04305").contains(n))
    val as: List[Assign] = ns.flatMap(name =>
      name match {
        case "sav111" => List(AssignExprToId(Id("sav111"), Str(" ")))
        case "ws" => List(AssignExprToArr(Arr("ws", Str("inv")), Str("N"))) // ws["inv"] = "N"
        case "st04305" => List(AssignExprToId(Id("st04305"), Str("[A0, A1, A2, A3, A4, A5, A6, A7, A8, C1, C2, F2, F3, F4, I0, I1, I4, P0, P1, P2, Q0, R1]")))
        case "ar04305" => List(
          AssignExprToArr(Arr("ar04305", Num(1)), Str("A0")), // ar04305[1]="A0"
          AssignExprToArr(Arr("ar04305", Num(2)), Str("A1")), // ar04305[2]="A1"
          AssignExprToArr(Arr("ar04305", Num(3)), Str("P0")), // ar04305[3]="P0"
          AssignExprToArr(Arr("ar04305", Num(4)), Str("A2")), // ar04305[4]="A2"
          AssignExprToArr(Arr("ar04305", Num(5)), Str("A3")), // ar04305[5]="A3"
          AssignExprToArr(Arr("ar04305", Num(6)), Str("A4")), // ar04305[6]="A4"
          AssignExprToArr(Arr("ar04305", Num(7)), Str("P1")), // ar04305[7]="P1"
          AssignExprToArr(Arr("ar04305", Num(8)), Str("A5")), // ar04305[8]="A5"
          AssignExprToArr(Arr("ar04305", Num(9)), Str("A6")), // ar04305[9]="A6"
          AssignExprToArr(Arr("ar04305", Num(10)), Str("A7")), // ar04305[10]="A7"
          AssignExprToArr(Arr("ar04305", Num(11)), Str("P2")), // ar04305[11]="P2"
          AssignExprToArr(Arr("ar04305", Num(12)), Str("A8")), // ar04305[12]="A8"
          AssignExprToArr(Arr("ar04305", Num(13)), Str("C1")), // ar04305[13]="C1"
          AssignExprToArr(Arr("ar04305", Num(14)), Str("C2")), // ar04305[14]="C2"
          AssignExprToArr(Arr("ar04305", Num(15)), Str("I0")), // ar04305[15]="I0"
          AssignExprToArr(Arr("ar04305", Num(16)), Str("I1")), // ar04305[16]="I1"
          AssignExprToArr(Arr("ar04305", Num(17)), Str("I4")), // ar04305[17]="I4"
          AssignExprToArr(Arr("ar04305", Num(18)), Str("Q0")), // ar04305[18]="Q0"
          AssignExprToArr(Arr("ar04305", Num(19)), Str("F2")), // ar04305[19]="F2"
          AssignExprToArr(Arr("ar04305", Num(20)), Str("F3")), // ar04305[20]="F3"
          AssignExprToArr(Arr("ar04305", Num(21)), Str("F4")), // ar04305[21]="F4"
          AssignExprToArr(Arr("ar04305", Num(22)), Str("R1")) // ar04305[22]="R1"
        )
    })
    if (as.size > 0) List(Meth("open", None, None, as)) else List()
  }

  def makeFiche(data: (String, Set[String], List[Form])): (String, Fiche, List[Form]) = {
    val globalVars = pickVars(data._2)
    val meths = initVars(globalVars)
    val fiche = Fiche(data._1, Some(VarBlock(globalVars)), meths)
    (data._1, fiche, data._3)
  }

  def fix(forms: List[Form]): List[(String, Fiche, List[Form])] = {
    val ts = getFormVars(groupForms(forms))
    val fs = for (t <- ts) yield makeFiche(t)
    fs.sortBy(data => data._1)
  }

  def write(f: File)(op: PrintWriter => Unit): Unit = {
    val pw = new PrintWriter(f)
    try {
      op(pw)
    } finally {
      pw.close()
    }
  }

  def save(fileName: String, forms: List[Form]): Unit = {
    val file = new File(fileName)
    write(file)(pw => forms.foreach(pw.println(_)))
  }

  def runFix(input: String, output: String): Unit = {
    val forms = load(input)
    val result = fix(forms).flatMap(data => data._2 :: data._3)
    save(output, result)
  }

}
