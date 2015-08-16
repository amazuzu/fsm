package com.amazuzu

import com.amazuzu.fsm.{IamDynamic, Fsm, Transition}
import com.amazuzu.someapp.items.{GoogleMap, GoogleMapMarker, Location}

/**
 * Created by taras.beletsky on 8/13/15.
 */

@Fsm
class MarkersFsm extends IamDynamic {

  //elements with initial values
  case class Elements(omap: Option[GoogleMap])

  val elems = Elements(None)

  //arguments with initial values
  case class Arguments(locations: List[Location])

  val args = Arguments(List())

  //data
  val model: (List[Location] /* awaiting map */ , List[GoogleMapMarker] /* markers */ ) = (List(), List())

  //states
  val transitions = List(
    Transition(0, "plusData", 1),
    Transition(1, "plusData", 1),
    Transition(1, "reset", 0))

  def onjoin(elem: String) = {
    (List(), createMarkers(args.locations))
  }

  def onevent(event: String) = event match {
    case "plusData" => elems.omap match {
      case Some(map) => model.copy(_2 = (model._2 ::: createMarkers(args.locations)))
      case None => model.copy(_1 = model._1 ::: args.locations)
    }
    case "reset" =>
      model._2.foreach(_.setMap(null))
      (List(), List())
  }

  def createMarkers(list: List[Location]) = list.map(l => new GoogleMapMarker(elems.omap.get, l))
}
