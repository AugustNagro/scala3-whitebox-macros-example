import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.collection.mutable as m
import scala.reflect.{ClassTag, classTag}

class Builder[T](
    mirror: Mirror.ProductOf[T],
    getters: IArray[String],
    setters: IArray[String],
    setterClasses: IArray[Class[?]]
) extends Selectable:
  private val values = Array.ofDim[Object](getters.length)

  def selectDynamic(name: String): Any =
    values(getters.indexOf(name))

  def applyDynamic(name: String, paramTypes: Class[?]*)(args: Object*): Unit =
    val setterIndex = setters.indexOf(name)
    val arg = args.head
    require(setterClasses(setterIndex).isInstance(arg))
    values(setters.indexOf(name)) = args.head

  def build: T = mirror.fromProduct(Tuple.fromArray(values))

transparent inline def builder[T <: Product] = ${ builderImpl[T] }

private def builderImpl[T: Type](using Quotes): Expr[Any] =
  Expr.summon[Mirror.ProductOf[T]].get match
    case '{
          $m: Mirror.ProductOf[T] {
            type MirroredElemLabels = mels
            type MirroredElemTypes = mets
          }
        } =>
      refineBuilder[T, mets, mels, Builder[T]](m)

private def refineBuilder[T: Type, Mets: Type, Mels: Type, Res: Type](
    m: Expr[Mirror.ProductOf[T]],
    getters: IArray[String] = IArray.empty,
    setters: IArray[String] = IArray.empty,
    setterClasses: Vector[Expr[Class[?]]] = Vector.empty
)(using Quotes): Expr[Any] =
  import quotes.reflect.*
  (Type.of[Mets], Type.of[Mels]) match
    case ('[met *: metTail], '[mel *: melTail]) =>
      val getter: String = Type.valueOfConstant[mel].get.toString
      val setter = "with" + getter.head.toUpper + getter.substring(1)
      val clsTag = Expr.summon[ClassTag[met]].get
      val clsExpr = '{ $clsTag.runtimeClass }
      val getterRefinement =
        Refinement(TypeRepr.of[Res], getter, TypeRepr.of[met])
      val setterRefinement = RecursiveType: parent =>
        parent.asType match
          case '[thisTpe] =>
            Refinement(getterRefinement, setter, TypeRepr.of[met => thisTpe])
      setterRefinement.asType match
        case '[tpe] =>
          refineBuilder[T, metTail, melTail, tpe](
            m,
            getters :+ getter,
            setters :+ setter,
            setterClasses :+ clsExpr
          )
    case ('[EmptyTuple], '[EmptyTuple]) =>
      '{
        new Builder[T](
          $m,
          ${ Expr(getters) },
          ${ Expr(setters) },
          IArray.from(${ Expr.ofSeq(setterClasses) })
        ).asInstanceOf[Res]
      }
