//> using target.platform jvm

package halotukozak.made.util

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.{Diagnostic, StoreReporter}

import java.nio.file.{Files, Path}
import scala.util.Using

/**
 * Compiles ad-hoc Scala snippets in-process via `dotty.tools.dotc.Driver` and exposes the raw
 * `Diagnostic` list, including warnings (`scala.compiletime.testing.typeCheckErrors` filters
 * those out even under `-Werror`).
 *
 * Use to assert macro `report.warning` / `report.info` output.
 */
object SnippetCompiler:

  /**
   * Compile `code` as a single source file against the current JVM classpath.
   *
   * @param code     full Scala source contents (no implicit imports)
   * @param options  extra scalac options (e.g. `-Werror`); classpath and outdir are added
   *                 automatically
   */
  def compile(code: String, options: Seq[String] = Nil): List[Diagnostic] =
    val src = Files.createTempFile("made-snippet-", ".scala")
    val outDir = Files.createTempDirectory("made-snippet-out-")
    val classpath = sys.props.getOrElse("java.class.path", "").nn

    object reporter extends StoreReporter(null, fromTyperState = false):
      def diagnostics: List[Diagnostic] = if infos == null then Nil else infos.nn.toList

    try
      Files.writeString(src, code)
      new Driver().process(
        options.concat(Seq("-classpath", classpath, "-d", outDir.toString, src.toString)).toArray,
        reporter,
        null,
      )
      reporter.diagnostics
    finally
      Files.deleteIfExists(src)
      deleteRecursively(outDir)

  private def deleteRecursively(p: Path): Unit =
    if Files.exists(p) then
      Using.resource(Files.walk(p)): stream =>
        stream.sorted(java.util.Comparator.reverseOrder).nn.forEach(Files.deleteIfExists(_))

  extension (diags: List[Diagnostic])
    /** True iff any diagnostic's message contains `substring`. */
    def containsMessage(substring: String): Boolean =
      diags.iterator.filter(_ != null).map(_.message).exists(_.contains(substring))
