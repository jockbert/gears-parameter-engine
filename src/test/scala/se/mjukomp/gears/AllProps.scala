
package se.mjukomp.gears

import org.scalacheck.Properties

object AllProps extends Properties("se.mjukomp.gears") {
  include(ParameterProps)
}
