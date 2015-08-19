val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe

import com.amazuzu.MarkersFsm
import com.amazuzu.someapp.items.{Location, GoogleMap}
import universe._
import scala.language.experimental.macros
val fsm = new MarkersFsm()
fsm.info
fsm.plusData(List(new Location()))
fsm.joinMap(new GoogleMap)
fsm.current
//fsm.plusData(List(1))d