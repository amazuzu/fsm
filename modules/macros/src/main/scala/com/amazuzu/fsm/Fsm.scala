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

    type Ast = List[c.Tree]
    val EmptyAst: Ast = List()

    def modifiedDeclaration(classDecl: ClassDef) = {


      val (className, body: Ast) = classDecl match {
        case q"class $className[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents{ ..$body } " => (className, body)
      }

      val stateDecl = body.asInstanceOf[Ast].flatMap {
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

      val elemsDecls = elemz.map { el =>
        if (el._4) q"var ${el._1}:Option[${el._2}] = ${el._3}"
        else q"var ${el._1}:${el._2} = ${el._3}"
      }

      def capital(str: String) = str.substring(0, 1).toUpperCase + str.substring(1)

      def unquote(str: String) = str.substring(1, str.length - 1)

      val joinz = elemz.map { el =>
        val ename = el._1.toString
        val funcname = if (el._4) TermName(s"join${capital(ename.substring(1))}") else TermName(s"join${capital(ename)}")
        if (el._4)
          q"""def $funcname(el:${el._2}) = {
          elems.${TermName(ename)} = Some(el)
          model = onjoin($ename)
          model
        }"""
        else
          q"""def $funcname(el:${el._2}) = {
          elems.${TermName(ename)} = el
          model = onjoin($ename)
          model
        }"""
      }

      def xtractTuples(l: List[c.Tree]): List[(Int, Int)] = l.map { case q"($a -> $b)" => (("" + a).toInt, ("" + b).toInt) }

      def xtractAnnotation(mods: Ast) = mods.flatMap {
        case q"new transition(..$args)" => Some(xtractTuples(args))
        case _ => None
      }.headOption

      def xtractArgs(args: Ast) = args.flatMap {
        case q"$_ val $name:$atype" => Some(("" + name, atype))
        case _ => None
      }

      def xtractTransitions(body: Ast) = body.flatMap {
        case q"@..${mods: Ast} def ${name: c.TermName}(..$fields) = $body" =>
          Some((name.toString, xtractAnnotation(mods), xtractArgs(fields)))
        case _ => None
      }.filter(_._2.isDefined)

      val transitions = xtractTransitions(body)

      val transitionsList = transitions.flatMap { trs =>
        trs._2.get.map { t =>
          q"Transition(${t._1}, ${trs._1}, ${t._2})"
        }
      }


      val preserved = (body.flatMap {
        case q"case class Elements(..$fields)" => None
        case q"val elems = Elements(..$defaults)" => None
        case q"val model:$tpe = $initmodelvalue" => None
        case fun@q"$mods def $funcname(..$args) = $body" =>

          transitions.find(el => el._1 == ("" + funcname)) match {
            case Some(t) =>
              val interm = TermName("_" + t._1)
              Some(q"def $interm(..$args) = $body")
            case _ => Some(fun)
          }

        case x@_ => Some(x)
      })

      val tranzFunctions = transitions.map { t =>
        val term = TermName(t._1)
        val interm = TermName("_" + t._1)
        val str: String = "" + t._1
        if (t._3.isEmpty)
          q"def $term() = tell(()=>$interm())($str)"
        else
          q"def $term(arg:${t._3.head._2}) = tell(()=> $interm(arg) )($str)"
      }



      val info = "" + transitionsList
      //q"_ match {case ..$bigSwitch case _ => model}"
      //val term = TermName("_" + t._1)
      //if (t._3.isEmpty) cq"${t._1} => tell(()=>$term())(${t._1}})" else cq"${t._1} => $term(arg)"
      //
      c.Expr[Any](
        q"""
        class $className extends com.amazuzu.fsm.BaseFsm[${stateDecl._1}](${stateDecl._2}, List(..$transitionsList)) {
            ..$preserved
            ..$joinz
            ..$tranzFunctions

          val info = ${"" + info}

          val elems = new {
            ..$elemsDecls
          }

        }
        """
      )
    }

    annottees.map(a => a.tree).toList match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case x@_ => c.abort(c.enclosingPosition, "Invalid annottee" + x)
    }

    //def printCode(tree: c.Tree, msg: String = "") = c.info(c.enclosingPosition, s"$msg\n${showCode(tree)}", true)
  }


}