object exams extends App {

	/////////////////////
	// Exam 2015
	/////////////////////

	/*
	*	Ejer 1
	*/
	def from(n: Int, m: Int): List[Int] =  {
		if (n == m) Nil
		else n :: from(n + 1, m)
	}
	println("from = " + from(3,7))

	/*
	*	Ejer 3
	*/
	def interleave (s1: Stream[Int], s2: Stream[Int]): Stream[Int] = {
		s1.head #:: s2.head #:: interleave(s1.tail,s2.tail)
	}

	println("interleave = " + interleave(Stream(1,2,3,4),Stream(10,20,30,40)).take(8).toList)

	/////////////////////
	// Exam 2016
	/////////////////////

	/*
	*	Ejer 1
	*/
	def combinations(n: Int): List[Int] = {
		def loop(n: Int, first: List[Int], temp: List[Int]): List[Int] = {
			if (temp.size == n) temp
			else if (first.head == 1) loop(n,first.tail, 0 :: temp)
			else loop(n,first.tail, 1 :: temp)
		}
		loop(n, List.fill(n)(0), Nil)
	}

	/*
	*	Ejer 2
	*/
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
	println("secondSmallest = " + secondSmallest(List(6,4,5,1,3,2)))

	/*
	*	Ejer 3
	*/
	def stream2Words(s: Stream[Char], separator: Char): Stream[List[Char]] = {
		def loop (s: Stream[Char], separator: Char, res: List[Char], res2: Stream[List[Char]]): Stream[List[Char]] = {
			if (s.isEmpty) res2
			else if (s.head != separator) loop(s.tail, separator, res :+ s.head, res2)
			else res #:: loop(s.tail, separator, Nil, res #:: res2)
		}
		loop(s,separator,Nil,Stream.Empty)
	}

	val result = stream2Words(('H' #:: 'e' #:: 'l' #::'l' #:: 'o' #:: '.' #:: 'I' #:: '.' #:: 'a' #:: 'm' #::'.' #:: 'a' #::'.' #:: 'c' #::'a' #::
  	't' #:: Stream.empty),'.')

	println("stream2Words = " + result.head)

	/////////////////////
	// Exam 2017
	/////////////////////

	/*
	*	Ejer 2
	*/

	def selectedMean(l: List[(Double,Int)], n: Int): Double = {
		def selectedMean_2(l: List[(Double,Int)], accu: Double): Double = {
			if (l.isEmpty) accu
			else selectedMean_2(l.tail, accu + l.head._1 )
		}
		selectedMean_2(l.filter(_._2 == n),0) / l.filter(_._2 == n).size
	}

	val example = List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3))
	println("selectedMean : " + selectedMean(example,3))


}