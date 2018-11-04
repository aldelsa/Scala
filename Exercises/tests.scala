object tests extends App {

	/* Example curryfication */

	val curry : ( Int => ( Int => Int )) = 
	(a) => { (b) => (a+b)/2 }

	val c2 = curry(2)(4)
	println("curry = " + c2)

	/* Example tail-recursion */

	def factorial(n: BigInt): BigInt = {
		@annotation.tailrec
		def go(acc: BigInt, n: BigInt): BigInt = {
		    if (n <= 1)
		    	acc
		    else
		    	go(acc * n, n - 1)
		    }
	    go(1, n)
	}
	println("factorial = " + factorial(10))

	def quicksort(l: List[Int]): List[Int] = l match {
		case Nil => Nil
		case hd::td => append(quicksort(td.filter(_<=hd)), hd :: quicksort(td.filter(_>hd)))
	} 

	def append(l1: List[Int], l2: List[Int]): List[Int] = {
		if (l1.isEmpty && l2.isEmpty) Nil
		else if (l1.isEmpty) l2.head :: append(l1,l2.tail)
		else l1.head :: append(l1.tail,l2)
	}

	println("quicksort = " + quicksort(List(5,6,8,3,2,7,9)))

	def averages(n:Int, s: Stream[Double]): Stream[Double] = {
		((s.take(n).sum) * 1/n ) #:: averages(n,s.tail)
	}

	val s = Stream(3.2, 5.3, 5.6, 4.4, 3.4, 7.5, 6.2, 9.4, 2.3)
	println(averages(3, s).take(3).toList)
}