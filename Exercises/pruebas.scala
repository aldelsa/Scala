object pruebas extends App {

	//println(factorial(10))

	val curry : ( Int => ( Int => Int )) = 
		(a) => { (b) => (a+b)/2 }

	//val c2 = curry(2)(4)
	//println(interleave(Stream(1,2,3,4),Stream(10,20,30,40)).take(8).toList)

	def interleave (s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
		s1.head #:: s2.head #:: interleave(s1.tail,s2.tail)
	}
	//println(from(3,7))

	val result = stream2Words(('H' #:: 'e' #:: 'l' #::'l' #:: 'o' #:: '.' #:: 'I' #:: '.' #:: 'a' #:: 'm' #::'.' #:: 'a' #::'.' #:: 'c' #::'a' #::
  	't' #:: Stream.empty),'.')

	//println(result.head)

	def combinations(num: Int): List[List[Int]] = {
		def combinations_2(num:Int, res: List[Int]): List[Int]= {
			if (index == num) res
			else
		}
	}



	def from(n: Int, m: Int): List[Int] =  {
		if (n == m) Nil
		else n :: from(n + 1, m)
	}

	def stream2Words(s: Stream[Char], separator: Char): Stream[List[Char]] = {
		def loop (s: Stream[Char], separator: Char, res: List[Char], res2: Stream[List[Char]]): Stream[List[Char]] = {
			if (s.isEmpty) res2
			else if (s.head != separator) loop(s.tail, separator, res :+ s.head, res2)
			else res #:: loop(s.tail, separator, Nil, res #:: res2)
		}
		loop(s,separator,Nil,Stream.Empty)
	}
	def secondSmallest(l: List[Int]) : Int = {
		def min2 (l: List[Int], min: Int) : Int = {
			if (l.isEmpty) min
			else if (l.head < min) min2(l.tail, l.head)
			else min2(l.tail, min)
		}
		def loop(l: List[Int], res: List[Int],min: Int): List[Int] = {
			if (l.isEmpty) res
			else if (l.head == min) loop(l.tail,res,min)
			else loop(l.tail,l.head :: res,min)
		}
		min2(loop(loop(l, Nil,min2(l,l.head)), Nil, min2(loop(l, Nil,min2(l,l.head)),loop(l, Nil,min2(l,l.head)).head)),l.head)
	}
	println(secondSmallest(List(6,4,5,1,3,2)))


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
}