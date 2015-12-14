package org.ensime.core.javac

import com.sun.source.util.TreePath
import org.ensime.core.{ DocSigPair }

trait JavaDocFinding extends Helpers {

  def docSignature(info: CompilationInfo, p: TreePath): Option[DocSigPair] = {
    fqn(info, p).map { fqn =>
      val sig = fqn.toDocSig
      DocSigPair(sig, sig)
    }
  }

}
