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
	// author : Christos Garis
	def combinations(n: Int): List[List[Int]] = {
		def numToBin(l: List[Int], n: Int): List[Int] = {
			if ((n == 0) || (n == 1))
				List(n) ::: l.tail
			else
				n % 2 :: numToBin(l.tail, n/2)
		}
		def loop(first: List[Int], n: Int, acum: Int): List[List[Int]] = {
			if (acum >= Math.pow(2, n))
				Nil
			else
				List(numToBin(first, acum).reverse) ::: loop(List.fill(n)(0), n, acum+1)
		}
		loop(List.fill(n)(0), n, 0)
	}

	println(combinations(3))

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
		min2(loop(l, Nil, min2(l,l.head)),l.head)
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

	val result = stream2Words(('H' #:: 'e' #:: 'l' #::'l' #:: 'o' #:: ' ' #:: 'I' #:: ' ' #:: 'a' #:: 'm' #::' ' #:: 'a' #::' ' #:: 'c' #::'a' #::
  	't' #:: Stream.empty),' ')

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

  	//////////////////////////
	//	4 Jan 2018
	//////////////////////////

  	/*
  	* Exercise 1
	*/
	def manhattan(x: List[Double], y: List[Double]): Double = {
		def manhattan_2(x: List[Double], y: List[Double], sum: Double): Double = {
			if (x.isEmpty && y.isEmpty) sum
			else manhattan_2( x.tail, y.tail, sum + Math.abs(x.head - y.head))
		}
		manhattan_2(x,y,0)
	}

	val a_m = List(1.0, 2.0, 3.0, 4.0, 5.0)
	val b_m = List(1.0, 2.0, 1.0, 2.0, 5.0)
	println("manhattan = " + manhattan(a_m, b_m))

	def manhattan_v2(x: List[Double], y: List[Double]): Double = {
		x.zip(y).map(x => x._1 - x._2).foldLeft(0.0)((a,b) => a + b)
	}

	println("manhattan v2 = " + manhattan(a_m, b_m))

  	/*
  	* Exercise 2
	*/
	def allSelected(pos: Int, l: List[(Double,Int)]): Double = {
		def allSelected_2(l: List[(Double,Int)], sum: Double): Double = {
			if (l.isEmpty) sum
			else if (l.head._2 == pos) allSelected_2(l.tail, sum + l.head._1)
			else allSelected_2(l.tail, sum)
		}
		allSelected_2(l,0)
	}
	val example_a = List((2.0,0),(4.5,1),(1.2,1),(3.0,3),(4.4,1),(4.5,0),(1.7,0),(5.3,2),(2.0,3))
	println("allSelected = " + allSelected(3,example_a))

	def addAll(l1: List[(Double,Int)], l2: List[Int]): List[Double] = {
		def sum_3(l1: List[(Double,Int)], pos: Int, sum: Double): Double = {
			if (l1.isEmpty) sum
			else if (l1.head._2 == pos) sum_3(l1.tail, pos, sum + l1.head._1)
			else sum_3(l1.tail, pos, sum )
		}
		def loop(l1: List[(Double,Int)], l2: List[Int], res: List[Double]): List[Double] = {
			if (l2.isEmpty) res
			else loop(l1, l2.tail, sum_3(l1,l2.head,0) :: res)
		}
		loop(l1,l2,Nil).reverse
	}
	println("addAll = " + addAll(example_a,List(0,1,2,3)))

	/*
  	* Exercise 3
	*/

	def sinSeq(): Stream[Double] = {
		def sinSeq_2(i: Int): Stream[Double] = {
			Math.sin((i+1)/2) #:: sinSeq_2(i + 1)
		}
		sinSeq_2(0)
	}
	println("sinSeq = " + sinSeq().take(5).toList)

	/*
  	* Exercise 4
	*/

	def stream2Words_v2(s: Stream[Char], separator: Char => Boolean): Stream[List[Char]] = {
		def stream2Words_v2_2(s: Stream[Char], separator: Char => Boolean, tempList: List[Char], res: Stream[List[Char]]): Stream[List[Char]] = {
			if (s.isEmpty) res
			else if (!separator(s.head)) stream2Words_v2_2(s.tail, separator, tempList :+ s.head, res)
			else tempList #:: stream2Words_v2_2(s.tail, separator, Nil, tempList #:: res)
		}
		stream2Words_v2_2(s, separator, Nil, Stream.Empty)
	}

	val separator : Char => Boolean = (c) => { c == ' ' }

	val result_3 = stream2Words_v2(('H' #:: 'e' #:: 'l' #::'l' #:: 'o' #:: ' ' #:: 'I' #:: ' ' #:: 'a' #:: 'm' #::' ' #:: 'a' #::' ' #:: 'c' #::'a' #::
  	't' #:: Stream.empty),separator)

	println("stream2Words_3 = " + result_3.head)



}