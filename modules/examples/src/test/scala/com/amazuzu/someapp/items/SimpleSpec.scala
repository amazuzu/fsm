package com.amazuzu.someapp.items

import com.amazuzu.MarkersFsm
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by taras.beletsky on 8/15/15.
 */
class SimpleSpec extends FlatSpec with Matchers {

  "simple fsm" should "be created" in {
    val fsm = new MarkersFsm()
    assert(fsm != null)
  }

  "simple fsm" should "accept new element as Map" in {
    val fsm = new MarkersFsm()

    assert(fsm.elems.omap.isEmpty)

    val data = fsm.joinMap(new GoogleMap)
    assert(fsm.elems.omap.isDefined)
    assert(data != null)
  }

  "simple fsm" should "move to '1' state" in {
    val fsm = new MarkersFsm()
    assert(fsm.current == 0)

    val data0 = fsm.model
    assert(data0._1.size == 0)
    fsm.plusData(List(new Location, new Location))
    val data1 = fsm.model
    assert(fsm.current == 1)
    assert(data1._1.size == 2)
  }
}
