import org.scalatest._
import java.io.{File, FilenameFilter}
import scala.sys.process._

class ValidationTests extends FunSuite with Matchers {

  /** Files in the validation suite that should be used as tests should start
    * with "example", followed by a number, followed by underscore, followed by
    * a name consisting of `[A-Za-z0-9_]`, followed by a single dot, and then
    * by the file ending.
    */
  private val TestFileNameRegex = "example(\\d+)_([a-zA-Z0-9_]+)\\..*".r

  /** Expected type of the output (how the result term should be interpreted) */
  private val TypeRegex = "^[/* ]*TYPE:\\s*\"(.*)\"[ /*]*$".r

  /** The expected output is saved in the comments in the test files themselves.
    * For now, only a single format (basic string) is supported.
    */
  private val ExpectRegex = "^[/* ]*EXPECT(?:ED)?:\\s*\"(.*)\"[ /*]*$".r

  /** Interprets `\n`, `\\` and `\"` in a string. */
  private def unescape(rawFromFile: String): String = {
    rawFromFile
      .replaceAll("\\\\\"", "\"")
      .replaceAll("\\\\\\\\", "\\\\")
      .replaceAll("\\\\n", "\n")
  }

  // Who tests the tester...?
  assert(unescape(""" \"Hi\" """) == " \"Hi\" ")
  assert(unescape(""" A\nB """) == " A\nB ")
  assert(unescape(""" A\\B """) ==  " A\\B ")

  /** Enumerates all test files in the specified / discovered validation 
    * suite, or returns `None` if no validation suite could be found.
    */
  private def discoverValidationSuite: Option[List[File]] = {
    Option(new File("examples"))
      .filter(_.exists)
      .filter(_.isDirectory)
      .map{ 
        _.listFiles(new FilenameFilter {
          def accept(dir: File, name: String): Boolean = {
            TestFileNameRegex.pattern.matcher(name).matches
          }
        }).toList
      }
  }

  // Programmatically generate test suite from the files
  discoverValidationSuite match {
    case None => ignore("No validation suite discovered") {}
    case Some(testFiles) => {
      for (f <- testFiles.sortBy(_.getName)) {
        val src = scala.io.Source.fromFile(f)
        val lines = src.getLines.toList
        src.close()
      
        val expected = lines
          .collect {
            case ExpectRegex(escapedStringContent) =>
              unescape(escapedStringContent)
          }
          .mkString("\n")
      
        val typ = lines
          .collect {
            case TypeRegex(t) => t
          }
          .headOption
          .getOrElse("Term")

        if (expected.isEmpty) {
          ignore(f.getName + " (no `EXPECT:` found)") {}
        } else {
          test(f.getName) {
            UntypedLambdaCalculus.runNoninteractive(typ, f.getAbsolutePath) match {
              case Left(err) => withClue(err) { fail }
              case Right(res) => res.trim shouldEqual expected.trim
            }
          }
        }
      }
    }
  }
}
