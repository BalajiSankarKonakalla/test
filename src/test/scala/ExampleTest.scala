import collection.mutable.Stack
import org.scalatest._

class ExampleTest extends FlatSpec with Matchers{

  "test" should "validate scenario 1" in {

    val testObj = new Test
    testObj.createRange("1,2,3,4") shouldBe "1-4"
  }

  "test" should "validate scenario 2" in {

    val testObj = new Test
    testObj.createRange("1-3,4") shouldBe "1-4"
  }

  "test" should "validate scenario 3" in {

    val testObj = new Test
    testObj.createRange("1,2-4") shouldBe "1-4"
  }

  "test" should "validate scenario 4" in {

    val testObj = new Test
    testObj.createRange("9,10-20,50-60") shouldBe "9-20,50-60"
  }

  "test" should "validate scenario 5" in {

    val testObj = new Test
    testObj.createRange("1-10,11-20,50-60") shouldBe "1-20,50-60"
  }


}
