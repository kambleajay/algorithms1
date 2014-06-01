import org.scalatest._

class TwoSumSuite extends FlatSpec {

	trait TestData {
		val arr1 = ArrayBuilder.build("twosummedium.txt") 
	}

	"ArrayBuilder" should "create correct array" in {
		new TestData {
			assert(arr1.size === 100)
			assert(arr1(0) === 16)
			assert(arr1(99) === 598)
		}

		assert(ArrayBuilder.build("HashInt.txt").size === 500000)
	}

	"TwoSumFinder" should "correctly count two sums" in {
		new TestData {
			val result1 = TwoSumFinder.count(arr1, new Interval(30, 60))
			assert(result1.size === 9)

			val result2 = TwoSumFinder.count(arr1, new Interval(60, 100))
			assert(result2.size === 28)

			val arr2 = ArrayBuilder.build("HashInt.txt")
			val result3 = TwoSumFinder.count(arr2, new Interval(2500, 4000))
			println("Answer: " +result3.size)
		}
	}

}