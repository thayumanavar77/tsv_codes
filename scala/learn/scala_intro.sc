object scala_intro {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def example = 2                                 //> example: => Int
  val example1 = 2                                //> example1  : Int = 2
  lazy val example2 = 2                           //> example2: => Int

  /*
  	Evaluation Rules
  	   Call by value: evaluates the function arguments before calling the function
  	   Call by name: evaluates the function first and then evaluates the arguments
  	                  if need be
  */
  def square(x: Double) = x * x                   //> square: (x: Double)Double
  def square1(x: Double) = x * x                  //> square1: (x: Double)Double
  // bindings is a sequence of int containing a varying number of arguments.

  def myFct(bindings: Int*) = {
     bindings.map ( x => x+1)
     }                                            //> myFct: (bindings: Int*)Seq[Int]
  myFct(1,2,3)                                    //> res0: Seq[Int] = ArrayBuffer(2, 3, 4)
  def sum1(a: Int, b: Int): Int = a+b             //> sum1: (a: Int, b: Int)Int
  /*
      Higher order functions:
         Functions that take function as a parameter or return functions
  */

   // sum() returns a function that takes two integers and
   // applies f to the numbers in range of integer and returns an integer
   def sum(f: Int => Int): (Int, Int) => Int = {
   	def sumFunction(start: Int, end: Int): Int = {
   	   if (start > end) 0 else f(start) + sumFunction(start + 1, end)
    }
    sumFunction
   }                                              //> sum: (f: Int => Int)(Int, Int) => Int
   sum(x => x)(1,5)                               //> res1: Int = 15

// The above is equivalent to the below:
	def sumAlternate(f: Int => Int)(start: Int, end: Int): Int =
		if (start > end) 0
		else f(start) + sumAlternate(f)(start+1, end)
                                                  //> sumAlternate: (f: Int => Int)(start: Int, end: Int)Int
  sumAlternate(x => x)(1, 5)                      //> res2: Int = 15

  def cube(x: Int) = x * x * x                    //> cube: (x: Int)Int
  sumAlternate(cube)(1, 10)                       //> res3: Int = 3025
  // Uncurried version, type is (Int, Int) => Int
  def times(x: Int, y: Int): Int = x * y          //> times: (x: Int, y: Int)Int
	// Curried Version, type is Int => Int => Int
	def timesAlternate(x: Int)(y: Int): Int = x * y
                                                  //> timesAlternate: (x: Int)(y: Int)Int
	val thriceTimes = timesAlternate(3) _     //> thriceTimes  : Int => Int = <function1>
	thriceTimes(7)                            //> res4: Int = 21

  // Define a new type Rational with a constructor
	class Rational(numerator: Int, denominator: Int) {
		require(denominator > 0, "denominator must be positive") // Precondition
		def this(numerator: Int) = this(numerator, 1) // Auxiliary constructor

		// Public method computed everytime it is called.
		def getNumerator = numerator
		def getDenominator = denominator
		private def isWholeNumber = denominator == 1 // Private method
		override def toString = numerator + "/" + denominator // Overridden method
	}
	val rationalNum = new Rational(3,4)       //> rationalNum  : scala_intro.Rational = 3/4
  rationalNum.getNumerator                        //> res5: Int = 3
  println(rationalNum)                            //> 3/4


  // Class Hierarchies
  abstract class IntSet {
  	def incl(x: Int): IntSet
  	def contains(x: Int): Boolean
  }

  class Empty extends IntSet {
  	def contains(x: Int): Boolean = false
  	def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  	override def toString = "."
  }
  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  	def contains(x: Int): Boolean =
  		if (x < elem) left contains x
  		else if (x > elem) right contains x
  		else true

  	def incl(x: Int): IntSet =
  		if (x < elem) new NonEmpty(elem, left incl x, right)
  		else if ( x > elem) new NonEmpty(elem, left, right incl x)
  		else this

  	override def toString = "{" + left + elem + right + "}"
  	}
 		val t1 = new NonEmpty(3, new Empty, new Empty)
                                                  //> t1  : scala_intro.NonEmpty = {.3.}
 		val t2 = t1 incl 4                //> t2  : scala_intro.IntSet = {.3{.4.}}

 }