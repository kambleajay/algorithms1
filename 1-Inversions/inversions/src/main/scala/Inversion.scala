import scala.collection.mutable.ListBuffer

object Inversion {

	type Inv = (Int, Int)

	def mergeSort(input: Array[Int]):Array[Int] = {
		if(input.length > 1) {
			val mid = input.length / 2
			val left = input take mid
			val right = input drop mid
			merge(mergeSort(left), mergeSort(right))
		} else {
			input
		}
	}

	private def merge(left: Array[Int], right: Array[Int]):Array[Int] = {
		var result = new Array[Int](left.size + right.size)
		var i = 0
		var j = 0
		for(k <- 0 until result.size) {
			if(i < left.size && j < right.size) {
				if(left(i) < right(j)) {
					result(k) = left(i)
					i = i + 1
				} else {
					result(k) = right(j)
					j = j + 1
				}
			} else if(i == left.size && j < right.size) {
				result(k) = right(j)
				j = j + 1
			} else {
				result(k) = left(i)
				i = i + 1
			}
		}
		result
	}

	def countInv(input: Array[Int]): (Array[Int], Long) = {
		val result = sortAndCount(input)
		//println("[CountInv]" +pa(result._1) +" contains " +result._2)
		result
	}

	private def sortAndCount(input: Array[Int]): (Array[Int], Long) = {
		if(input.size == 1) {
			(input, 0)
		} else {
			val mid = input.size / 2
			val left = sortAndCount(input take mid)
			val right = sortAndCount(input drop mid)
			val merged = mergeAndCount(left._1, right._1)
			(merged._1, left._2 + right._2 + merged._2)
		}
	}

	private def mergeAndCount(left: Array[Int], right: Array[Int]): (Array[Int], Long) = {
		//println("[M&C_Start] left=[" +pa(left) +"]/right=[" +pa(right) +"]")
		var result = new Array[Int](left.size + right.size)
		var invCount:Long = 0
		var i = 0
		var j = 0
		for(k <- 0 until result.size) {
			if(i < left.size && j < right.size) {
				if(left(i) < right(j)) {
					result(k) = left(i)
					i = i + 1
				} else {
					//inversions found!
					//println("[Inv] i=" +i +"/j=" +j)
					invCount = invCount + (left.size - i)
					result(k) = right(j)
					j = j + 1
					
				}
			} else if(i == left.size && j < right.size) {
				result(k) = right(j)
				j = j + 1
			} else {
				result(k) = left(i)
				i = i + 1
			}
		}
		//println("[M&C_End] count=" +invCount +"/merged = [" +pa(result) +"]")
		(result, invCount)	
	}

	def pa(arr: Array[Int]) = arr mkString ","
}