import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.collection.mutable as m
import scala.reflect.{ClassTag, classTag}

class Builder[T](
    mirror: Mirror.ProductOf[T],
    getters: IArray[String],
    setters: IArray[String]
) extends Selectable:
  private val values = Array.ofDim[Object](getters.length)

  def selectDynamic(name: String): Any =
    values(getters.indexOf(name))

  def applyDynamic(name: String)(args: Any*): this.type =
    values(setters.indexOf(name)) = args.head.asInstanceOf[Object]
    this

  def build: T = mirror.fromProduct(Tuple.fromArray(values))

object Builder:
  transparent inline def apply[T <: Product] = ${ builderImpl[T] }

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
      setters: IArray[String] = IArray.empty
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*
    (Type.of[Mets], Type.of[Mels]) match
      case ('[met *: metTail], '[mel *: melTail]) =>
        val getter: String = Type.valueOfConstant[mel].get.toString
        val setter = "with" + getter.head.toUpper + getter.substring(1)
        val getterRefinement =
          Refinement(TypeRepr.of[Res], getter, TypeRepr.of[met])
        val setterRefinement = RecursiveType: parent =>
          val mt =
            MethodType(List("value"))(
              _ => List(TypeRepr.of[met]),
              _ => parent.recThis
            )
          Refinement(getterRefinement, setter, mt)
        setterRefinement.asType match
          case '[tpe] =>
            refineBuilder[T, metTail, melTail, tpe](
              m,
              getters :+ getter,
              setters :+ setter
            )
      case ('[EmptyTuple], '[EmptyTuple]) =>
        report.info(TypeRepr.of[Res].show)
        '{
          new Builder[T](
            $m,
            ${ Expr(getters) },
            ${ Expr(setters) }
          ).asInstanceOf[Res]
        }

//  inline def showMe(inline a: Any): Unit = ${ showMeImpl('{ a }) }
//
//  private def showMeImpl(value: Expr[Any])(using Quotes): Expr[Unit] =
//    import quotes.reflect.*
//    report.info(value.asTerm.show(using Printer.TreeStructure))
//    '{ () }
