package com.amazuzu.fsm

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/**
 * Created by taras.beletsky on 8/15/15.
 */
class Fsm extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro FsmAnnotationImpl.impl
}

object FsmAnnotationImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def modifiedDeclaration(classDecl: ClassDef) = {

      val (className, body) = classDecl match {
        case q"class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents{ ..$body } " => (className, body)
      }

      val stateDecl = body.asInstanceOf[List[c.Tree]].flatMap {
        case el@q"val model:$tpe = $value" => Some((tpe, value))
        case x@_ => None
      }.head

      val elemz: List[(c.TermName, c.Tree, c.Tree, Boolean)] = (body.asInstanceOf[List[c.Tree]].flatMap {
        case q"case class Elements(..$fields)" => Some(fields)
        case x@_ => None
      }.head.map {
        case q"$acc val $name:Option[$typ]" => (name, typ, true)
        case q"$acc val $name:$typ" => (name, typ, false)
      }).zip(body.asInstanceOf[List[c.Tree]].flatMap {
        case q"val elems = Elements(..$defaults)" => Some(defaults)
        case x@_ => None
      }.head).map(el => (el._1._1, el._1._2, el._2, el._1._3))

      val argz: List[(c.TermName, c.Tree, c.Tree)] = (body.asInstanceOf[List[c.Tree]].flatMap {
        case q"case class Arguments(..$fields)" => Some(fields)
        case x@_ => None
      }.head.map {
        case q"$acc val $name:$typ" => (name, typ)
      }).zip(body.asInstanceOf[List[c.Tree]].flatMap {
        case q"val args = Arguments(..$defaults)" => Some(defaults)
        case x@_ => None
      }.head).map(el => (el._1._1, el._1._2, el._2))

      val elemsDecls = elemz.map { el =>
        if (el._4) q"var ${el._1}:Option[${el._2}] = ${el._3}"
        else q"var ${el._1}:${el._2} = ${el._3}"
      }
      val argsDecls = argz.map(el => q"var ${el._1}:${el._2} = ${el._3}")

      def capital(str: String) = str.substring(0, 1).toUpperCase + str.substring(1)

      val joinz = elemz.map { el =>
        val ename = el._1.toString
        val funcname = if (el._4) TermName(s"join${capital(ename.substring(1))}") else TermName(s"join${capital(ename)}")
        if (el._4)
          q"""def $funcname(el:${el._2}) = {
          elems.${TermName(ename)} = Some(el)
          onjoin($ename)
        }"""
        else
          q"""def $funcname(el:${el._2}) = {
          elems.${TermName(ename)} = el
          onjoin($ename)
        }"""
      }

      val tranz: Set[String] = (body.asInstanceOf[List[c.Tree]].flatMap {
        case q"val transitions = List(..$trans)" => Some(trans)
        case x@_ => None
      }.head.map {
        case q"Transition($from,$event,$to)" =>
          val ev = "" + event
          ev.substring(1, ev.length - 1)
      }).toSet[String]

      val tellz = argz.map { arg =>
        val ename = arg._1.toString

        tranz.map { tr =>
          val funcname = TermName(tr)

          q"""def $funcname(el:${arg._2}) = {
          args.${TermName(ename)} = el
          tell($tr)
        }"""
        }
      }.flatten

      val preserved = (body.asInstanceOf[List[c.Tree]].flatMap {
        case q"case class Elements(..$fields)" => None
        case q"case class Arguments(..$fields)" => None
        case q"val elems = Elements(..$defaults)" => None
        case q"val args = Arguments(..$defaults)" => None
        case q"val model:$tpe = $initmodelvalue" => None
        case q"val transitions = $tranz" => None
        case x@_ => Some(x)
      })


      val transitionsDecl = body.asInstanceOf[List[c.Tree]].flatMap {
        case el@q"val transitions = $value" => Some(value)
        case x@_ => None
      }.head


      //val oelemsstr = "" + stateDecl
      //val info: String = $oelemsstr

      c.Expr[Any](
        q"""
      class $className extends com.amazuzu.fsm.BaseFsm[${stateDecl._1}](${stateDecl._2},$transitionsDecl){
        ..$preserved
        ..$joinz
        ..$tellz

        val elems = new {
          ..$elemsDecls
        }
        val args = new {
          ..$argsDecls
        }

      }
      """
      )
    }

    annottees map (_.tree) toList match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}