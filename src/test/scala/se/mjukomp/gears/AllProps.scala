
package se.mjukomp.gears

import org.scalacheck.Properties

object AllProps extends Properties("se.mjukomp") {
  include(SomeProps)
}
