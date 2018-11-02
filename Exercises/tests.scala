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
}