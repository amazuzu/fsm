package com.amazuzu.fsm

/**
 * Created by taras.beletsky on 8/14/15.
 */
abstract class BaseFsm[S](val model0: S, transitions: List[Transition]) {
  var model = model0
  var current = transitions.head.from

  private var thenEvent: String = ""

  final def tell(event: String):S = transitions.find(tr => tr.from == current && tr.event == event) match {
    case Some(trans) =>
      //println(s"${trans.from} -> ${trans.event} -> ${trans.to}")
      model = onevent(event)
      onStateExit()
      current = trans.to
      if (thenEvent != "") {
        val tmpThenEvent = thenEvent
        thenEvent = ""
        tell(tmpThenEvent)
      } else model
    case None => //throw new Error(s"fsm can't find transition for state=${current} event=${event}")
      model
  }

  protected def then(event: String) = thenEvent = event

  protected def onevent(event: String): S

  protected def onStateExit(): Unit = {}
}

case class Transition(from: Int, event: String, to: Int)
