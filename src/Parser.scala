import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  val integer: Parser[Int] = "[0-9]+".r ^^ { case num => num.toInt }
  val float: Parser[Float] = "[0-9]+\\.[0-9]+".r ^^ { case num => num.toFloat }
  val numeral = integer | float

  //val add: Parser[Int] = integer ~ "+" ~ integer ^^ { case t1 ~ _ ~ t2 => t1 + t2 }
  val add: Parser[Int] = chainl1(integer, "+" ^^^ { case (acc, "+" ~ next) => acc + next})

  val identifier: Parser[String] = "[a-zA-Z_][a-zA-Z_0-9]*".r
  val equals: Parser[String] = "="



  val whitespace: Parser[String] = "[ ]|\\t|\\f|\\n|\\r".r
//  val illegal: Parser[String] = "[^]".r

  val declaration = identifier ~ equals ~ (add | float | integer) ~ ";"
  val language = declaration.*

  def parse(input: String) = parseAll(language, input)
}

object Main extends App {
  val input =
    """
      |anton = 223.22;
      |kalle = 3 + 2;
    """.stripMargin

  val parsed = Parser.parse(input)

  println(parsed)
}