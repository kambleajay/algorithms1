import org.scalatest._
import scala.io.Source

class InversionTestSuite extends FlatSpec {

	"Inversion" should "correctly sort array" in {
		val input = Array(5, 2, 4, 7, 1, 3, 2, 6)
		assert(Array(1, 2, 2, 3, 4, 5, 6, 7) === Inversion.mergeSort(input))

		val input2 = Array(5, 4, 3, 2, 1)
		assert(Array(1, 2, 3, 4, 5) === Inversion.mergeSort(input2))

		val input3 = Array(1, 1, 1)
		assert(Array(1, 1, 1) === Inversion.mergeSort(input3))
	}

	it should "correctly count inversion" in {
		val input1 = Array(2, 4, 1, 3, 5)
		val result = Inversion.countInv(input1)
		assert(3 === result._2)

		val input2 = Array(1, 3, 5, 2, 4, 6)
		val result2 = Inversion.countInv(input2)
		assert(3 === result2._2)

		val input3 = Array(1, 2, 3, 4, 5)
		val result3 = Inversion.countInv(input3)
		assert(result3._2 === 0)

		val input4 = Array(5, 4, 3, 2, 1)
		val result4 = Inversion.countInv(input4)
		assert(result4._2 === 10)
	}

	it should "correctly count inversion for big array from exercise" in {
		val bigInput = (for {line <- Source.fromFile("/Users/ajay/algo/ProgrammingExercises/1-Inversions/IntegerArray.txt").getLines} 
			yield line.toInt).toArray
		assert(bigInput.size === 100000)
		assert(bigInput(0) === 54044)
		assert(bigInput(6) === 2995)
		assert(bigInput(59696) === 7903)
		assert(bigInput(73170) === 11780)
		assert(bigInput(49118) === 5933)
		assert(bigInput(99999) === 91901)
		val result = Inversion.countInv(bigInput)
		println("<Answer> " +result._2)
	}
}