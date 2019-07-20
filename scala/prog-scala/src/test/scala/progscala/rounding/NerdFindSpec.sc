import org.scalatest.FunSpec
import org.specs2.matcher.ShouldMatchers
import progscala.rounding.NerdFinder

class NerdFinderSpec extends FunSpec with ShouldMatchers {
  describe ("nerd finder") {
    it ("identify nerds from a List") {
      val actors = List("Rich Moranis","James Deam","Woody Allen")
      val finder = NerdFinder(actors)
      finder.findNerds shouldEqual actors
    }
  }
}