package com.amazuzu.fsm

/**
 * Created by taras.beletsky on 8/14/15.
 */
abstract class BaseFsm[S](val model0: S, transitions: List[Transition]) {
  var model = model0
  var current = 0

  private var thenEvent: String = ""

  final def tell(eventFunc: () => S)(event: String): S = transitions.find(tr => tr.from == current && tr.event == event) match {
    case Some(trans) =>
      model = eventFunc()
      onStateExit()
      current = trans.to
      if (thenEvent != "") {
        val tmpThenEvent = thenEvent
        thenEvent = ""
        tell(eventFunc)(tmpThenEvent)
      } else model
    case None =>
      model
  }

  protected def then(event: String) = thenEvent = event

  protected def onStateExit(): Unit = {}
}

case class Transition(from: Int, event: String, to: Int)
