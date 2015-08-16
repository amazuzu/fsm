package com.amazuzu.fsm
import scala.language.dynamics

/**
 * Created by taras.beletsky on 8/16/15.
 */
class IamDynamic extends Dynamic {
  def applyDynamic(name: String)(args: Any*) = AnyRef

  def selectDynamic(name: String) = AnyRef
}
