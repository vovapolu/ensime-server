package org.ensime.core.javac

import com.sun.source.tree.{ MemberSelectTree, MethodInvocationTree, Tree }
import com.sun.source.util.TreePath
import javax.lang.model.`type`.{ DeclaredType, PrimitiveType, ReferenceType, TypeKind, TypeMirror }
import javax.lang.model.element.{ Element, ExecutableElement, TypeElement }
import javax.lang.model.util.{ ElementFilter, Elements }
import org.ensime.core.{ DocFqn, DocSig, DocSigPair }
import scala.collection.JavaConversions._

trait JavaDocFinding extends Helpers {

  def docSignature(info: CompilationInfo, p: TreePath): Option[DocSigPair] = {
    val leaf = p.getLeaf
    val enclosing = Option(p.getParentPath.getLeaf)
    (leaf, enclosing) match {
      case (t: MemberSelectTree, Some(m: MethodInvocationTree)) => {
        val name = t.getIdentifier().toString()
        fqn(info, t.getExpression()).map { fqn =>
          val sig = DocSig(DocFqn.tupled(fqn), memberSig(info, m, candidates(info, t.getExpression, name), name))
          DocSigPair(sig, sig)
        }
      }
      case (t: Tree, _) => {
        fqn(info, p).map { fqn =>
          val sig = DocSig(DocFqn.tupled(fqn), None)
          DocSigPair(sig, sig)
        }
      }
    }
  }

  private def candidates(info: CompilationInfo, target: Tree, name: String): List[ExecutableElement] = {
    typeElement(info, target).map { el =>
      el match {
        case tel: TypeElement => {
          val elements: Elements = info.getElements()
          ElementFilter.methodsIn(
            elements.getAllMembers(tel).filter { e => e.getSimpleName.toString() == name }
          ).toList
        }
        case _ => {
          List()
        }
      }
    }.getOrElse(List())
  }

  private def memberSig(info: CompilationInfo, m: MethodInvocationTree,
    candidates: List[ExecutableElement], name: String): Option[String] = {
    val args = m.getArguments
    val byArity = candidates.filter { c => c.getParameters.length == args.length }
    val withTypeDeltas = byArity.map { c => typeDeltas(info, m, c) }.toArray
    val compatible = withTypeDeltas.filter { case (c, deltas) => !deltas.contains(Int.MaxValue) }
    val sorted = compatible.sortWith { (c1, c2) => c1._2.sum < c2._2.sum }
    sorted.headOption.map {
      case (c, deltas) =>
        val args = c.getParameters.map { p => fqn(info, p.asType).map(DocFqn.tupled(_).mkString).getOrElse("NA") }.mkString(",")
        s"$name($args)"
    }
  }

  // For each (argument, parameter) pair compute an integer distance between them.
  // Int.MaxValue denotes an incompatible pairing (e.g. reference argument supplied for
  // primitive param). 0 denotes type equality. 
  private def typeDeltas(info: CompilationInfo, m: MethodInvocationTree, c: ExecutableElement): (ExecutableElement, Array[Int]) = {
    val types = info.getTypes()
    val args = m.getArguments.map(typeMirror(info, _))
    val params = c.getParameters.map { p => Option(p.asType) }
    val distances = args zip params map {
      case (Some(p: TypeMirror), Some(a: TypeMirror)) if types.isSameType(p, a) => 0
      case (Some(p: PrimitiveType), Some(a: PrimitiveType)) if types.isAssignable(a, p) => 1
      case (Some(p: ReferenceType), Some(a: ReferenceType)) if types.isSubtype(a, p) => 1
      case _ => Int.MaxValue
    }
    (c, distances.toArray)
  }

}
