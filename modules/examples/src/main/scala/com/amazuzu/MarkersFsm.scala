package com.amazuzu

import com.amazuzu.fsm.{Fsm, IamDynamic, transition}
import com.amazuzu.someapp.items.{GoogleMap, GoogleMapMarker, Location}

/**
 * Created by taras.beletsky on 8/13/15.
 */

@Fsm
class MarkersFsm extends IamDynamic {

  @transition(1 -> 0)
  def reset() = {
    model._2.foreach(_.setMap(null))
    (List(), List())
  }

  @transition(0 -> 1, 1 -> 1)
  def plusData(locations: List[Location]) = elems.omap match {
    case Some(map) => model.copy(_2 = (model._2 ::: createMarkers(locations)))
    case None => model.copy(_1 = model._1 ::: locations)
  }

  //elements with initial values
  case class Elements(omap: Option[GoogleMap])

  val elems = Elements(None)

  //data
  val model: (List[Location] /* awaiting map */ , List[GoogleMapMarker] /* markers */ ) = (List(), List())

  def onjoin(elem: String) = (List(), createMarkers(model._1))

  def createMarkers(list: List[Location]) = list.map(l => new GoogleMapMarker(elems.omap.get, l))
}
