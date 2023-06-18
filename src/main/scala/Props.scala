import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*

class Props extends Selectable:
  def selectDynamic(name: String): String = name.toUpperCase

transparent inline def props[T <: Product] = ${ propsImpl[T] }

private def propsImpl[T: Type](using Quotes): Expr[Any] =
  Expr.summon[Mirror.ProductOf[T]].get match
    case '{
          $m: Mirror.ProductOf[T] {
            type MirroredElemLabels = mels
            type MirroredElemTypes = mets
          }
        } =>
      refineProps[mels, Props]

private def refineProps[Mels: Type, Res: Type](using Quotes): Expr[Any] =
  import quotes.reflect.*
  Type.of[Mels] match
    case '[mel *: melTail] =>
      val fieldName = Type.valueOfConstant[mel].get.toString
      val refinement =
        Refinement(TypeRepr.of[Res], fieldName, TypeRepr.of[String])
      refinement.asType match
        case '[tpe] => refineProps[melTail, tpe]
    case '[EmptyTuple] =>
      '{ new Props().asInstanceOf[Res] }
