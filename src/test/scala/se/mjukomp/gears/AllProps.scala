
package se.mjukomp.gears

import org.scalacheck.Properties

object AllProps extends Properties("gears") {
  include(ParameterProps)
  include(AmountProps)
}
