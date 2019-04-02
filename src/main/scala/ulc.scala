/** Interpreter for heavily sugared pure lambda calculus.
  *
  * Provides syntactic sugar for integers, lists, function definitions,
  * mutual recursion etc.
  */
object UntypedLambdaCalculus {

  // ###########################################################################
  //                                ABSTRACT SYNTAX
  // ###########################################################################
  
  // Abstract syntax of lambda-terms
  trait Term {
    def apply(other: Term): Term = App(this, other)
  }
  
  case class Var(name: String) extends Term {
    override def toString = name
  }
  
  case class Abs(variableName: String, body: Term) extends Term {
    override def toString = "(\\%s.%s)".format(variableName, body.toString)
  }
  
  case class App(function: Term, argument: Term) extends Term {
    override def toString = 
      "(%s %s)".format(function.toString, argument.toString)
  }
  
  // ###########################################################################
  //                           SUBSTITUTION
  // ###########################################################################
  
  // Generates a fresh variable
  def freshName(preferred: String, blacklist: Set[String]): String = {
    if (!blacklist.contains(preferred)) preferred
    else freshName(preferred + "'", blacklist)
  }
  
  /** Finds all variables of a term (bound and unbound) */
  def allVariables(term: Term): Set[String] = term match {
    case Var(x) => Set(x)
    case Abs(x, t) => allVariables(t) + x
    case App(f, x) => allVariables(f) ++ allVariables(x)
  }
  
  // Finds all free variables in a term
  def freeVariables(term: Term): Set[String] = term match {
    case Var(x) => Set(x)
    case Abs(x, t) => freeVariables(t) - x
    case App(f, x) => freeVariables(f) ++ freeVariables(x)
  }
  
  // Performs brute-force replacement of a free variable by a term.
  // Does not perform any variable-renaming.
  // The recursion is blocked by shadowing: only the free occurrences are
  // replaced.
  // 
  // Example:
  // `graft((\x.x)x, "x", y) = (\x.x)y`
  def graft(term: Term, varName: String, replacement: Term): Term = term match {
    case Var(v) if (varName == v) => replacement
    case Var(x) => term
    case App(f, x) => App(
      graft(f, varName, replacement), 
      graft(x, varName, replacement)
    )
    case Abs(v, b) if (varName == v) => term // shadowing
    case Abs(x, b) => Abs(x, graft(b, varName, replacement))
  }
  
  /** 
   * Substitution (binding-avoiding).
   * Substitutes all occurrences of the free variable `varName` in `term`
   * by `replacement`. Renames variables to avoid new bindings.
   *
   * In usual notation, this computes `term[replacement/varName]` or 
   * `term[varName := replacement]`.
   */
  def substitute(
    term: Term, 
    varName: String, 
    replacement: Term
  ): Term = 
    term match {
      case Var(v) if (varName == v) => replacement
      case Var(_) => term 
      case App(f, x) => App(
        substitute(f, varName, replacement), 
        substitute(x, varName, replacement)
      )
      case Abs(v, b) if (varName == v) => term // shadowing
      case Abs(x, b) => {
        // We should add all variables of `term` to the blacklist.
        // For example, the term `(\x.\x'.x x')` is closed (so it has no free 
        // variables), but it would be wrong to rename `x` to `x'`, if we wanted
        // to reduce `(\x.\x'.x x')x`.
        //
        // We certainly should add `replacement`'s free variables to blacklist,
        // so that `(\x.x y)[y := x]` transforms into `(\x'.x' x)`, not into
        // `(\x'.x' x')`.
        val blacklist = (allVariables(term) - x) ++ freeVariables(replacement)
          // (I thought really hard about it, it should be correct this time,
          //  written 2015-08-24)
        val xPrime = freshName(x, blacklist)
        val sanitizedBody = graft(b, x, Var(xPrime))
        val substBody = substitute(sanitizedBody, varName, replacement)
        Abs(xPrime, substBody)
      }
  }

  // ###########################################################################
  //                                  REDUCTION
  // ###########################################################################
  
  // this function is not complicated,
  // it's only purpose it to remind myself that it is not complicated
  // once the substitution is done right.
  def contractRedex(f: Abs, x: Term) = {
    val Abs(variable, body) = f
    substitute(body, variable, x)
  }
  
  /** 
   * Contracts the leftmost-outermost redex in a term.
   * Returns `Some[Term]` if there was a contraction.
   * Returns `None` if the term contained no redex.
   */
  def leftmostReduction(t: Term): Option[Term] = t match {
    case Var(_) => None
    case App(f @ Abs(x, b), t) => Some(contractRedex(f, t))
    case App(f, x) => leftmostReduction(f) match {
      case Some(newF) => Some(App(newF, x))
      case None => leftmostReduction(x) match {
        case Some(newX) => Some(App(f, newX))
        case None => None
      }
    }
    case Abs(x, b) => for (bb <- leftmostReduction(b)) yield Abs(x, bb)
  }
  
  /**
   * Lalement, Theorem 3.7: The leftmost reduction strategy is normalizing.
   *
   * Just keeps applying the leftmost reduction until nothing changes.
   */
  @annotation.tailrec
  def normalize(t: Term): Term = {
    leftmostReduction(t) match {
      case Some(x) => normalize(x)
      case None => t
    }
  }
  
  // ###########################################################################
  //                            SYNTACTIC SUGAR
  // ###########################################################################
  // Here, we make some useful special lambda-terms available
  
  /** Church-encoding of booleans */
  val church_true = Abs("y", Abs("n", Var("y")))
  val church_false = Abs("y", Abs("n", Var("n")))
  
  /** Church-encoding of natural numbers */
  def churchEncoding(i: Int): Term = {
    def rec(i: Int): Term = 
      if (i == 0) Var("x") 
      else App(Var("f"), rec(i - 1))
    Abs("f", Abs("x", rec(i)))
  }
  
  /** 
   * List-of-booleans based encoding of naturals.
   * Each entry of the pair-list stores the `isZero`-value,
   * so that a number `n` will be represented by `n-1` `false`'s,
   * followed by a single `true`.
   */
  def nat_boolListEncoding(i: Int): Term = {
    // Don't move outside of this method: 
    // it does not check bound variable names,
    // and therefore could break terms in general.
    def pair(x: Term, y: Term): Term = 
      Abs("t", App(App(Var("t"), x), y))
  
    if (i == 0) {
      pair(church_true, church_true)
    } else {
      pair(church_false, nat_boolListEncoding(i - 1))
    }
  }
  
  val yCombinator = Abs("F", 
    App(
      Abs("x", App(Var("F"), App(Var("x"), Var("x")))),
      Abs("x", App(Var("F"), App(Var("x"), Var("x"))))
    )
  )
  
  def makePair(a: Term, b: Term) = {
    // Standard pair constructor: `\t.\x.\y.t x y`
    val pairTerm = 
      Abs("a", Abs("b", Abs("t", App(App(Var("t"), Var("a")), Var("b")))))
    App(App(pairTerm, a), b)
  }                         
  
  def makeList(ts: List[Term]): Term = ts match {
    case Nil => makePair(church_true, makePair(church_true, church_true))
    case h :: t => makePair(church_false, makePair(h, makeList(t)))
  }
  
  // Transforms list of mutually recursive definitions into plain lambda terms.
  //
  // This "desugaring"-step that is sufficiently complex to be considered
  // kind of "compilation" of source code into plain lambda calculus
  def defineMutuallyRecursiveFunctions(
    funcs: List[((String,List[String]),Term)], 
    remainder: Term
  ): Term = {
    require(
      funcs.size >= 2,
      "There must be at least 2 mutually recursive functions"
    )
    // first step: bring the function declarations into form
    // f_0 = F_0(f_0, ..., f_n)
    // ...
    // f_n = F_n(f_1, ..., f_n)
    // that is, move the variables of the functions to the right hand side
    val noArgFuncs = for (((name, args), rhs) <- funcs) yield {
      val rhsWithArgs = args.foldRight(rhs) {
        case (argName, modifiedRhs) => Abs(argName, modifiedRhs)
      }
      (name, rhsWithArgs)
    }
    // create a name for a variable that will represent the fixed point
    // (we simply use some weird symbols in the name to make sure that there
    //  are no collisions)
    val fp = "mutrec#" + noArgFuncs.map(_._1).mkString("#") 
    
    // represent each function as projection of `fp`,
    // build terms that look like `(...(fp false) false) ... false) true`
    // where `false` is repeated `i` number of times
    def projectionReprHelper(i: Int, wrappedTerm: Term): Term = {
      if (i == 0) App(wrappedTerm, church_true)
      else projectionReprHelper(i - 1, App(wrappedTerm, church_false))
    }
    def projectionRepr(i: Int) = projectionReprHelper(i, Var(fp))
  
    // build a list with function names and
    // corresponding `fp false^i true`-representations
    val functionProjectionReprs: List[(String, Term)] = 
      funcs.map(_._1._1) zip Stream.from(0).map(projectionRepr)
  
    // replace function names by their projection-representations in each
    // right hand side
    val projectionReprRhss = noArgFuncs.map{ case (name, rhs) => 
      functionProjectionReprs.foldLeft(rhs) { 
        case (modifiedRhs, (fName, fRepr)) => 
          substitute(modifiedRhs, fName, fRepr)
      }
    }
    
    // combine the modified right-hand sides into a `pair`-based linked 
    // list (big nested tuple)
    def listHelper(rhss: List[Term]): Term = {
      rhss match {
        case Nil => church_true
        case head :: tail => makePair(head, listHelper(tail))
      }
    }
    val rhsCombined = listHelper(projectionReprRhss)
  
    // now we can define the `fp` using Y-combinator
    val functional = Abs(fp, rhsCombined)
    val solution = App(yCombinator, functional)
  
    // now we have to prepend projection-representations of all 
    // functions to the `remainder`, and then wrap it into a 
    // redex that defines `fp`.
    val withFuncsPrepended = functionProjectionReprs.foldRight(remainder) {
      case ((fName, fRepr), r) => App(Abs(fName, r), fRepr)
    }
    
    val result = App(Abs(fp, withFuncsPrepended), solution)
   
    result
  }
  
  // ###########################################################################
  //                                  PARSER
  // ###########################################################################
  import scala.util.parsing.combinator._
  
  object SugaredLambdaCalculusParser extends JavaTokenParsers {
    def variableId: Parser[String] = 
      "[_a-zA-Z][_a-zA-Z0-9]*".r |
      failure("expected variableId")
    def variable: Parser[Term] = variableId ^^ {
      case v => Var(v)
    }
    def term: Parser[Term] = (factor ~ rep(factor)) ^^ {
      case f~xs => xs.foldLeft(f) {
        case (a, b) => App(a, b)
      }
    }
    def factor: Parser[Term] = 
      valDecl | defDecl | mutRecDecl | lambda | 
      "(" ~> term <~ ")" | "{" ~> term <~ "}" | variable | 
      naturalNumberSugar | listSugar
      
    def lambda: Parser[Term] = (("\\" ~> variableId <~ ".") ~ term) ^^ {
      case name~t => Abs(name, t)
    }
    // def inspected: Parser[Term] = 
    //   ("INSPECT[" ~> "[ a-zA-Z0-9]+".r  <~ "]" <~ ";") ~ term  ^^ {
    //     case label~remainder => Inspect(label, remainder)
    //   }
    def valDecl: Parser[Term] = 
      (("val" ~> variableId <~ "=") ~ (term <~ ";") ~ term) ^^ {
        case (x~r)~t => App(Abs(x, t), r)
      }
    def defDecl: Parser[Term] = 
      (
        ("def" ~> (variableId ~ rep(variableId)) <~ "=") ~ 
        (term <~ ";") ~
        term
      ) ^^ {
        case ((f~args)~rhs)~t => {
          if (freeVariables(rhs).contains(f)) {
            // recursive function. Building it using fixpoint
            // combinator
            
            val abstractedArgs = args.foldRight(rhs) {
              case (arg, modRhs) => Abs(arg, modRhs)
            }
            val functional = Abs(f, abstractedArgs)
            val recFuncDef = App(yCombinator, functional)
            App(Abs(f, t), recFuncDef)
          } else {
            // No recursion, simple alias
            val functionDef = args.foldRight(rhs) {
              case (argName, modifiedRhs) => Abs(argName, modifiedRhs)
            }
            App(Abs(f, t), functionDef)
          }
        }
      }
    
    // block with mutually recursive functions
    def mutRecDecl: Parser[Term] = 
      (("mutuallyRecursive" ~> 
        "{" ~> repsep(mutRecFunc, ",") <~ "}" <~
        ";"
      ) ~ term) ^^ {
        case funcs~remainder => {
          defineMutuallyRecursiveFunctions(funcs, remainder)
        }
      }
  
    // one of multiple mutually recursive functions
    // in a `mutuallyRecursive` block
    def mutRecFunc: Parser[((String,List[String]),Term)] =  
      ("def" ~> (variableId ~ rep(variableId)) <~ "=") ~ term ^^ {
        case (x~y)~z => ((x,y),z)
      }
      
    def naturalNumberSugar: Parser[Term] = "[0-9]+".r ^^ {
      case s => nat_boolListEncoding(s.toInt)
    }
  
    def listSugar: Parser[Term] = "[" ~> repsep(term, ",") <~ "]" ^^ {
      case terms => makeList(terms)
    }
  
    def parseTerm(s: String): scala.util.Either[String, Term] = 
    parseAll(term, s) match {
      case Success(t, _) => scala.util.Right(t)
      case f => scala.util.Left(f.toString)
    }
  }
  
  // ###########################################################################
  //                               DECODING THE OUTPUT
  // ###########################################################################
  // The result of each computation is a lambda term.
  // If lambda terms are used to encode data structures like integers and lists,
  // the output becomes essentially unreadable.
  // Therefore, we give the user the option to "re-sugar" the output.
  // For this, we have to know how to interpret the output: the user will thus
  // be given the opportunity to specify the "type" of the output.
  
  trait OutputDecoder[X] {
    def decode(t: Term): Option[X]
  }
  
  object TrivialDecoder extends OutputDecoder[Term] {
    override def toString = "Term"
    def decode(term: Term) = Some(term)
  }
  
  object BooleanDecoder extends OutputDecoder[Boolean] {
    override def toString = "Boolean"
    def decode(term: Term) = term match {
      case Abs(t, Abs(f, Var(q))) => {
        if (t == q) Some(true)
        else if (f == q) Some(false)
        else None 
      }
      case _ => None
    }
  }
  
  case class PairDecoder[X, Y](
    xDec: OutputDecoder[X], 
    yDec: OutputDecoder[Y]
  ) extends OutputDecoder[(X, Y)] {
    override def toString = "(" + xDec + "," + yDec + ")"
    def decode(term: Term) = term match {
      case Abs(s, App(App(Var(ss), left), right)) if (s == ss) => {
        for (
          x <- xDec.decode(left);
          y <- yDec.decode(right)
        ) yield (x, y)
      }
      case _ => None
    }
  }
  
  // decodes stuff like `(false,(false,(false,I)))` into `3`,
  // where `(,)` denote the pair constructor
  object NatDecoder extends OutputDecoder[Int] {
    override def toString = "Int"
    def decode(term: Term) = 
      PairDecoder(BooleanDecoder, TrivialDecoder).decode(term) match {
        case Some((isZero, tail)) => {
          if (isZero) Some(0)
          else (for (pred <- this.decode(tail)) yield (pred + 1))
        }
        case None => None
      }
  }
  
  // Decodes linked lists, where each element of the list looks as
  // `(isEmpty, (head, tail))`, 
  // with standard pair encoding.
  class ListDecoder[X](elemDec: OutputDecoder[X])
  extends OutputDecoder[List[X]] {
    override def toString = "List[" + elemDec + "]"
    def decode(term: Term) = 
      PairDecoder(BooleanDecoder, TrivialDecoder).decode(term) match {
        case Some((isEmpty, headTail)) => {
          if (isEmpty) Some(Nil)
          else PairDecoder(elemDec, TrivialDecoder).decode(headTail) match {
            case Some((h, t)) => {
              for (decodedTail <- decode(t)) yield (h :: decodedTail)
            }
            case None => None // list structure broken (no inner h-t-pair)
          }
        }
        case None => None // list structure broken (no outer pair)
      }
  }
  
  object ResultTypeParser extends JavaTokenParsers {
  
    def trivial: Parser[OutputDecoder[_]] = "Term" ^^ {
      case _ => TrivialDecoder
    }
    def bool: Parser[OutputDecoder[Boolean]] = "Boolean" ^^ {
      case _ => BooleanDecoder
    }
    def nat: Parser[OutputDecoder[Int]] = "Int" ^^ { case _ => NatDecoder }
    def list: Parser[OutputDecoder[_]] = "List[" ~> typ <~ "]" ^^ {
      case t => new ListDecoder(t)
    }
    def typ: Parser[OutputDecoder[_]] = nat | bool | list | trivial
  
    def parseType(str: String): scala.util.Either[String, OutputDecoder[_]] =
    parseAll(typ, str) match {
      case Success(dec, _) => scala.util.Right(dec)
      case f => scala.util.Left(f.toString)
    }
  
  }
  
 
  // ###########################################################################
  //                              RUNNING THE INTERPRETER
  // ###########################################################################
  
  /** Evaluates term loaded from `filePath`, decodes it using the 
    * specified decoder.
    *
    * Returns `Right` with the output in case of success, a `Left` with an
    * error message otherwise.
    */
  def runNoninteractive(decoderDescriptor: String, filePath: String)
  : Either[String, String] = {
    for {
      src <- {
        val f = new java.io.File(filePath)
        if (f.exists) {
          val src = scala.io.Source.fromFile(filePath)
          val sourceCode =
            src.getLines.filterNot(_.trim.startsWith("//")).mkString("\n")
          src.close()
          Right(sourceCode)
        } else {
          Left(s"File `${filePath}` doesn't exist.")
        }
      }
      dec <- ResultTypeParser.parseType(decoderDescriptor)
      trm <- SugaredLambdaCalculusParser.parseTerm(src)
      nrm = normalize(trm)
      res <- ((dec.decode(nrm) match {
        case Some(t) => Right(t.toString)
        case None => Left("Failed to decode the result as " + dec)
      }): Either[String, String])
    } yield res
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("""
        |Usage:
        |
        |Interactive mode:
        |
        |    scala ulc.scala --interactive
        |
        |Script mode:
        |
        |    scala ulc.scala --type=<TYP> <INPUT_FILE_PATH>
        |
        |where <TYP> is the type of the expected output.
        |
        |Supported types and type constructors are:
        |  
        |  Int            (natural numbers)
        |  Boolean        (true, false)
        |  Term           (raw lambda terms, default)
        |  List[A]        (lists of type `A`)
        |  (A, B)         (pairs of types `A` and `B`)
        |""".stripMargin
      )
    } else {
      val fileName = args.last
      var outputDecoderDescriptor = "Term"
      var interactiveMode = false
      
      for (a <- args) {
        if (a.startsWith("--")) {
          if (a == "--interactive") {
            interactiveMode = true
          } else if (a.startsWith("--type=")) {
            outputDecoderDescriptor = a.drop("--type=".size)
                   } else {
            println("Unknown option: " + a)
            System.exit(1)
          }
        }
      } 
      
      if (interactiveMode) {
        var inputLine = ""
        while (inputLine != "stop") {
          print("Enter lambda expression:")
          inputLine = scala.io.StdIn.readLine()
          SugaredLambdaCalculusParser.parseTerm(inputLine) match {
            case scala.util.Right(t) => println(normalize(t))
            case scala.util.Left(f) => println(f)
          }
        }
      } else {
        runNoninteractive(outputDecoderDescriptor, fileName) match {
          case Right(res) => println(res)
          case Left(err) => {
            println("Error: " + err)
            System.exit(1)
          }
        }
      }
    }
  }
}
