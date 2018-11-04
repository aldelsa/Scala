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

	/*
	*	Ejer 4
	*/
	def append(l1: List[Int], l2: List[Int]): List[Int] = {
		if (l1.isEmpty && l2.isEmpty) Nil
		else if (l1.isEmpty) l2.head :: append(l1,l2.tail)
		else l1.head :: append(l1.tail,l2)
	}
	println("append = " + append(List(1,2,3,4),List(5,6,2,8)))

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
	*	Ejer 1
	*/

	def euclidean(x: List[Double], y: List[Double]): Double = {
		def euclidean_2(x: List[Double], y: List[Double], curr: Double): Double = {
			if (x.isEmpty || y.isEmpty) curr
			else euclidean_2(x.tail, y.tail, curr + Math.pow(x.head - y.head,2))
		}
		Math.sqrt(euclidean_2(x,y,0.0))
	}

	def euclidean_v2(x: List[Double], y: List[Double]): Double = {
		Math.sqrt(x.zip(y).map(x => Math.pow(x._1 - x._2,2)).foldLeft(0.0)((a,b) => a + b))
	}

	val a = List(1.0, 2.0, 3.0, 4.0, 5.0)
	val b = List(1.0, 2.0, 1.0, 2.0, 5.0)

	println("euclidean = " + euclidean(a,b))

	println("euclidean_v2 = " + euclidean_v2(a,b))

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

	/*
	*	Ejer 3
	*/
	def NewtonMethod (n: Double, f: Double => Double, f1: Double => Double): Stream[Double] = {
		((n) - ((f(n))/(f1(n)))) #:: NewtonMethod(((n+1) - ((f(n+1))/(f1(n+1)))),f,f1)
	}

	val f: (Double => Double) = (x) => { x*x - 8.0 }
	val df: (Double => Double) = (x) => { 2*x }
	println("NewtonMethod = " + NewtonMethod(10,f,df).take(5).toList)

	/*
	*	Ejer 4
	*/
  	def addAll(l: List[List[Int]]): Int = {
		def addAll_2(l: List[Int], n: Int): Int = {
			if (l.isEmpty) n
			else addAll_2(l.tail,n + l.head)
		}
		def loop(l: List[List[Int]], sum: Int): Int = {
			if (l.isEmpty) sum
			else loop(l.tail, sum + addAll_2(l.head, 0))
		}
		loop(l,0)
  	}
  	println("addAll = " + addAll(List(List(1,2,3),List(4,5,6))))

}