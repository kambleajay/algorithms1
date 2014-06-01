import org.scalatest._

class MedianMaintSuite extends FlatSpec {

	trait TestData {
		val arr1 = ArrayBuilder.build("mmtest1.txt")
		val arr2 = ArrayBuilder.build("mmtest2.txt")
		val arr3 = ArrayBuilder.build("mmtest3.txt")
		val arr4 = ArrayBuilder.build("Median.txt")

		val sarr1 = ArrayBuilder.build("mmsmall1.txt")
		val sarr2 = ArrayBuilder.build("mmsmall2.txt")
		val sarr3 = ArrayBuilder.build("mmsmall3.txt")
	}
	
	"ArrayBuilder" should "correctly build array" in {
		new TestData {
			assert(arr1.size === 6)
			assert(arr2.size === 5000)
			assert(arr3.size === 20)
			assert(arr4.size === 10000)
		}
	}

	"MedianMaint" should "correctly return sums" in {
		new TestData {
			assert(MedianMaint.sum(arr1) === 50)
			assert(MedianMaint.sum(arr2) === 8046)
			assert(MedianMaint.sum(arr3) === 110)
			assert(MedianMaint.sum(sarr3) === 201)

			println("Big: " +MedianMaint.sum(arr4))
		}
	}

}