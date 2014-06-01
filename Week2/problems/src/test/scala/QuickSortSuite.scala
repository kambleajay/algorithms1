import org.scalatest._
import scala.io.Source
import QuickSort.{sort => qusort}

class QuickSortSuite extends FlatSpec {

	trait TestData {
		val arr1 = (1 to 10).toArray //45-45-19
		val arr2 = (1 to 100).toArray //4950-4950-480
		val arr3 = Array(2, 8, 9, 3, 7, 5, 10, 1, 6, 4) //25-20-19
		val arr4 = Array(1, 11, 5, 15, 2, 999, 3, 2, 98, 765, 8, 14, 15, 16, 88, 145, 100, 12, 9, 99, 77, 0) //96-109-82
		val arr5 = Array(1, 11, 5, 15, 2, 12, 9, 99, 77, 0) //24-33-22
		val arr6 = Array(0, 9, 8, 7, 6, 5, 4, 3, 2, 1) //45-37-25
		val arr7 = Array(999, 3, 2, 98, 765, 8, 14, 15, 16, 88, 145, 100) //44-29-29
	}

	"QuickSort" should "correctly sort array" in {
		val input1 = Array(3, 8, 2, 5, 1, 4, 7, 6)
		val result1 = QuickSort.sort(input1)
		assert(Array(1, 2, 3, 4, 5, 6, 7, 8) === result1._1)

		val input = Array(5, 2, 4, 7, 1, 3, 2, 6)
		val result = QuickSort.sort(input)
		assert(Array(1, 2, 2, 3, 4, 5, 6, 7) === result._1)

		val input2 = Array(5, 4, 3, 2, 1)
		val result2 = QuickSort.sort(input2)
		assert(Array(1, 2, 3, 4, 5) === result2._1)

		val input3 = Array(1, 1, 1)
		val result3 = QuickSort.sort(input3)
		assert(Array(1, 1, 1) === result3._1)

		val input4 = Array(1, 2, 3, 4, 5, 6, 7)
		val result4 = QuickSort.sort(input4)
		assert(Array(1, 2, 3, 4, 5, 6, 7) === result4._1)
	}

	it should "correctly count comparisons using median pivot" in {
		new TestData {
			assert(qusort(arr1)._2 === 19)
			assert(qusort(arr2)._2 === 480)
			assert(qusort(arr3)._2 === 19)
			assert(qusort(arr4)._2 === 82)
			assert(qusort(arr5)._2 === 22)
			assert(qusort(arr6)._2 === 25)
			assert(qusort(arr7)._2 === 29)
		}
	}

	ignore should "correctly count comparisons using first index as pivot" in {
		new TestData {
			assert(qusort(arr1)._2 === 45)
			assert(qusort(arr2)._2 === 4950)
			assert(qusort(arr3)._2 === 25)
			assert(qusort(arr4)._2 === 96)
			assert(qusort(arr5)._2 === 24)
			assert(qusort(arr6)._2 === 45)
			assert(qusort(arr7)._2 === 44)
		}
	}

	ignore should "correctly count comparisons using last index as pivot" in {
		new TestData {
			assert(qusort(arr1)._2 === 45)
			assert(qusort(arr2)._2 === 4950)
			assert(qusort(arr3)._2 === 20)
			assert(qusort(arr4)._2 === 109)
			assert(qusort(arr5)._2 === 33)
			assert(qusort(arr6)._2 === 37)
			assert(qusort(arr7)._2 === 29)
		}
	}

	it should "correctly count comparisons for big array in exercise" in {
		val bigInput = (for {line <- Source.fromURL(getClass.getResource("QuickSort.txt")).getLines} 
			yield line.toInt).toArray
		val result = QuickSort.sort(bigInput)
		println("Big Array no of comparisons = " +result._2)
	}

	it should "give correct median" in {
		val array1 = Array(3, 8, 2, 5, 1, 4, 7, 6)
		val med1 = QuickSort.getMedian(array1, 0, 7)
		assert(med1._2 === 5)
		assert(med1._1 === 3)

		val med2 = QuickSort.getMedian(array1, 3, 7)
		assert(med2._2 === 5)
		assert(med1._1 === 3)
	}

}