package halotukozak.made

import halotukozak.made.annotation.*

class DoneTest extends munit.FunSuite:

  test("trait with multiple methods, annotations, and mixed arities") {
    val _: Done {
      type Type = NamedService
      type Label = "NamedService"
      type Metadata = (Meta @ServiceMarker) *: EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "external_id"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = NamedService
        type OutputType = String
      } *: DoneOperation.SingleApply {
        type Label = "ping"
        type Metadata = (Meta @MethodMarker) *: EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "message"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = NamedService
        type OutputType = Boolean
        type Arg = String
      } *: DoneOperation {
        type Label = "combine"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = EmptyTuple
          } *: InputElem {
            type Type = Int
            type Label = "right"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: 1 *: EmptyTuple
        type OuterType = NamedService
        type OutputType = String
      } *: DoneOperation.EmptyApply {
        type Label = "version"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = NamedService
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[NamedService]
  }

  test("class with inherited @name on overridden method") {
    val _: Done {
      type Type = AliasImpl
      type Label = "AliasImpl"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "endpoint"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = AliasImpl
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[AliasImpl]
  }

  test("abstract class with abstract and concrete methods") {
    val _: Done {
      type Type = AbstractJob
      type Label = "AbstractJob"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "run"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "task"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = AbstractJob
        type OutputType = Either[String, Int]
        type Arg = String
      } *: DoneOperation.EmptyApply {
        type Label = "cached"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = AbstractJob
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[AbstractJob]
  }

  test("generic trait specializes output type") {
    val _: Done {
      type Type = Converter[Int]
      type Label = "Converter"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "convert"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "raw"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = Converter[Int]
        type OutputType = Int
        type Arg = String
      } *: EmptyTuple
    } = Done.derived[Converter[Int]]
  }

  test("singleton object with multiple methods") {
    val _: Done {
      type Type = Toolbox.type
      type Label = "Toolbox"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "hello"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = Toolbox.type
        type OutputType = String
      } *: DoneOperation {
        type Label = "concat"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = EmptyTuple
          } *: InputElem {
            type Type = String
            type Label = "right"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 2 *: EmptyTuple
        type OuterType = Toolbox.type
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[Toolbox.type]
  }

  test("enum with methods declared on the enum itself") {
    val _: Done {
      type Type = ServiceState
      type Label = "ServiceState"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "code"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ServiceState
        type OutputType = String
      } *: DoneOperation.EmptyApply {
        type Label = "isTerminal"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ServiceState
        type OutputType = Boolean
      } *: EmptyTuple
    } = Done.derived[ServiceState]
  }

  test("trait with @name on the type and a method") {
    val _: Done {
      type Type = RenamedService
      type Label = "custom_service"
      type Metadata <: Tuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "id"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = RenamedService
        type OutputType = String
      } *: DoneOperation.EmptyApply {
        type Label = "ping"
        type Metadata = (Meta @MethodMarker) *: EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = RenamedService
        type OutputType = Boolean
      } *: EmptyTuple
    } = Done.derived[RenamedService]
  }

  test("methods returning complex types are preserved") {
    val _: Done {
      type Type = ComplexReturns
      type Label = "ComplexReturns"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "items"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = List[String]
      } *: DoneOperation.EmptyApply {
        type Label = "mapping"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = Map[String, Int]
      } *: DoneOperation.SingleApply {
        type Label = "either"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "name"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = Either[String, Int]
        type Arg = String
      } *: EmptyTuple
    } = Done.derived[ComplexReturns]
  }

  test("trait without methods has empty operations") {
    val _: Done {
      type Type = EmptyService
      type Label = "EmptyService"
      type Metadata = EmptyTuple
      type Operations = EmptyTuple
    } = Done.derived[EmptyService]
  }

  test("multi-param-list method derives faithfully (flat InputElems + per-list arity)") {
    // `def combine(left: String)(right: Int)` keeps a flat InputElems (left, right) but
    // additionally records ParamLists = (1, 1) so dispatch can reconstruct list boundaries.
    val _: Done {
      type Type = MultiListService
      type Label = "MultiListService"
      type Metadata = EmptyTuple
      type Operations = DoneOperation {
        type Label = "combine"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = EmptyTuple
          } *: InputElem {
            type Type = Int
            type Label = "right"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: 1 *: EmptyTuple
        type OuterType = MultiListService
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[MultiListService]
  }

  test("object methods with different return types") {
    val _: Done {
      type Type = MathTools.type
      type Label = "MathTools"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "zero"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = MathTools.type
        type OutputType = Int
      } *: DoneOperation.SingleApply {
        type Label = "toText"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = Int
            type Label = "value"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = MathTools.type
        type OutputType = String
        type Arg = Int
      } *: EmptyTuple
    } = Done.derived[MathTools.type]
  }

  test("abstract val declarations are treated as nullary operations") {
    val _: Done {
      type Type = ValHolder
      type Label = "ValHolder"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "magic"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ValHolder
        type OutputType = Int
      } *: DoneOperation.EmptyApply {
        type Label = "message"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = ValHolder
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[ValHolder]
  }

  test("Unit return type is preserved") {
    val _: Done {
      type Type = UnitReturning
      type Label = "UnitReturning"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "log"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "msg"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = UnitReturning
        type OutputType = Unit
        type Arg = String
      } *: DoneOperation.EmptyApply {
        type Label = "tick"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = UnitReturning
        type OutputType = Unit
      } *: EmptyTuple
    } = Done.derived[UnitReturning]
  }

  test("function and tuple return types are preserved") {
    val _: Done {
      type Type = FuncAndTuple
      type Label = "FuncAndTuple"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "mkAdder"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = Int
            type Label = "base"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = FuncAndTuple
        type OutputType = Int => Int
        type Arg = Int
      } *: DoneOperation.EmptyApply {
        type Label = "pair"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = FuncAndTuple
        type OutputType = (Int, String)
      } *: EmptyTuple
    } = Done.derived[FuncAndTuple]
  }

  test("no-parens `def a` and empty-parens `def b()` derive to DISTINCT shapes") {
    // Both have empty InputElems, but the ParamLists discriminator differs:
    //   no-parens   `def a`  -> ParamLists = EmptyTuple        (zero param lists)
    //   empty-parens `def b()` -> ParamLists = 0 *: EmptyTuple  (one param list of arity 0)
    // The param-list shape is recorded via the `ParamLists` type member (tuple of
    // singleton-Int per-list arities); the full tuple-of-tuples InputElems split is deferred.
    // This is the "faithful enough to dispatch" representation.
    val _: Done {
      type Type = EmptyParens
      type Label = "EmptyParens"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "a"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = EmptyParens
        type OutputType = Int
      } *: DoneOperation.EmptyApply {
        type Label = "b"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = 0 *: EmptyTuple
        type OuterType = EmptyParens
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[EmptyParens]
  }

  test("inherited-not-overridden methods appear in child operations") {
    // Child extends Base; `inherited` is declared on the parent and NOT overridden.
    // It must surface in the child's Operations (alongside the child's own `childOnly`).
    val _: Done {
      type Type = NonOverridingChild
      type Label = "NonOverridingChild"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.EmptyApply {
        type Label = "inherited"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = NonOverridingChild
        type OutputType = String
      } *: DoneOperation.EmptyApply {
        type Label = "childOnly"
        type Metadata = EmptyTuple
        type InputElems = EmptyTuple
        type ParamLists = EmptyTuple
        type OuterType = NonOverridingChild
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[NonOverridingChild]
  }

  test("apply invokes parameterless method on target instance") {
    val done = Done.derived[Calculator]
    val _ *: _ *: answerOp *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(answerOp.apply(impl, EmptyTuple), 42)
  }

  test("apply invokes method with single parameter list") {
    val done = Done.derived[Calculator]
    val addOp *: _ *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(addOp.apply(impl, (7, 5)), 12)
  }

  test("apply invokes method with multiple parameter lists") {
    val done = Done.derived[Calculator]
    val _ *: mulOp *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(mulOp.apply(impl, (6, 7)), 42)
  }

  test("Done.invoke fixes target to Done.Type for type safety") {
    val done = Done.derived[Calculator]
    val addOp *: _ *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(done.invoke(addOp, impl, (2, 3)), 5)
  }

  test("apply dispatches virtually through an abstract member") {
    val done = Done.derived[Calculator]
    val _ *: _ *: answerOp *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = 0
      def mul(a: Int)(b: Int): Int = 0
      override def answer: Int = 99

    assertEquals(answerOp.apply(impl, EmptyTuple), 99)
    assertEquals(answerOp.apply(impl), 99)
  }

  test("a method's synthesized $default$N accessor is not enumerated as its own operation") {
    // Regression test: `op`'s compiler-generated `op$default$1` accessor carries neither
    // Flags.Synthetic nor Flags.Artifact (it is a real, callable Scala 3 trait default method), so
    // it used to leak through as a spurious extra DoneOperation alongside `op` and `other`.
    val opOp *: otherOp *: EmptyTuple = Done.derived[WithDefaultParam].operations

    assertEquals(opOp.label, "op")
    assertEquals(otherOp.label, "other")

    val impl: WithDefaultParam = new WithDefaultParam:
      def op(z: Int): Int = z
      def other(): Int = 0

    assertEquals(opOp.apply(impl, Tuple1(5)), 5)
  }

  test("apply invokes methods on a singleton object") {
    val done = Done.derived[MathTools.type]
    val zeroOp *: toTextOp *: EmptyTuple = done.operations

    assertEquals(zeroOp.apply(MathTools, EmptyTuple), 0)
    assertEquals(zeroOp.apply(MathTools), 0)
    assertEquals(toTextOp.apply(MathTools, Tuple(123)), "123")
    assertEquals(toTextOp.apply(MathTools, 123), "123")
  }

  test("overloaded methods both appear as separate operations with the same label") {
    val _: Done {
      type Type = OverloadedCompute
      type Label = "OverloadedCompute"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "compute"
        type Metadata = EmptyTuple
        type InputElems = InputElem {
          type Type = Int
          type Label = "x"
          type Metadata = EmptyTuple
        } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = OverloadedCompute
        type OutputType = Int
        type Arg = Int
      } *: DoneOperation.SingleApply {
        type Label = "compute"
        type Metadata = EmptyTuple
        type InputElems =
          InputElem {
            type Type = String
            type Label = "x"
            type Metadata = EmptyTuple
          } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = OverloadedCompute
        type OutputType = String
        type Arg = String
      } *: EmptyTuple
    } = Done.derived[OverloadedCompute]
  }

  // --- parameter-level annotations are captured into InputElem.Metadata ---

  test("annotated parameter yields non-EmptyTuple InputElem.Metadata") {
    val _: Done {
      type Type = AnnParamSvc
      type Label = "AnnParamSvc"
      type Metadata = EmptyTuple
      type Operations = DoneOperation.SingleApply {
        type Label = "f"
        type Metadata = EmptyTuple
        type InputElems = InputElem {
          type Type = Int
          type Label = "arg"
          type Metadata = (Meta @ParamMarker) *: EmptyTuple
        } *: EmptyTuple
        type ParamLists = 1 *: EmptyTuple
        type OuterType = AnnParamSvc
        type OutputType = Unit
        type Arg = Int
      } *: EmptyTuple
    } = Done.derived[AnnParamSvc]
  }

  test("getAnnotation/hasAnnotation read the annotation on the param InputElem") {
    val done = Done.derived[AnnParamSvc]
    val fOp *: EmptyTuple = done.operations
    val argElem *: EmptyTuple = fOp.inputElems

    assert(argElem.hasAnnotation[ParamMarker])
    assert(argElem.getAnnotation[ParamMarker] != null)
  }

// --- Fixtures ---

class ServiceMarker extends MetaAnnotation
class MethodMarker extends MetaAnnotation
class ParamMarker extends MetaAnnotation

trait AnnParamSvc:
  def f(@ParamMarker arg: Int): Unit

@ServiceMarker
trait NamedService:
  @name("external_id")
  def id: String

  @MethodMarker
  def ping(message: String): Boolean

  def combine(left: String)(right: Int): String

  def version: Int

trait AliasBase:
  @name("endpoint")
  def id: String

class AliasImpl extends AliasBase:
  override def id: String = "impl"

abstract class AbstractJob:
  def run(task: String): Either[String, Int]
  def cached: Int = 42

trait Converter[A]:
  def convert(raw: String): A

object Toolbox:
  def hello: String = "hi"
  def concat(left: String, right: String): String = left + right

enum ServiceState:
  case Open, Closed

  def code: String = productPrefix

  def isTerminal: Boolean = this match
    case Open => false
    case Closed => true

@name("custom_service")
trait RenamedService:
  def id: String

  @MethodMarker
  def ping: Boolean

trait ComplexReturns:
  def items: List[String]
  def mapping: Map[String, Int]
  def either(name: String): Either[String, Int]

trait EmptyService

trait MultiListService:
  def combine(left: String)(right: Int): String

object MathTools:
  def zero: Int = 0
  def toText(value: Int): String = value.toString

trait ValHolder:
  val magic: Int
  val message: String

trait UnitReturning:
  def log(msg: String): Unit
  def tick: Unit

trait FuncAndTuple:
  def mkAdder(base: Int): Int => Int
  def pair: (Int, String)

trait EmptyParens:
  def a: Int
  def b(): Int

trait NonOverridingParent:
  def inherited: String = "base"

class NonOverridingChild extends NonOverridingParent:
  def childOnly: Int = 42

trait OverloadedCompute:
  def compute(x: Int): Int
  def compute(x: String): String

trait Calculator:
  def add(a: Int, b: Int): Int
  def mul(a: Int)(b: Int): Int
  def answer: Int

// A method with a default parameter value gets a compiler-synthesized `<method>$default$N`
// accessor on the SAME type (even though `op` itself is abstract) — it must NOT be enumerated as
// its own DoneOperation.
trait WithDefaultParam:
  def op(z: Int = 99): Int
  def other(): Int
