
//Pano numbers
abstract class Nat{
	def isZero:Boolean
	def predecessor:Nat
	def successor:Nat = new Success(this)
	def +(that:Nat):Nat
	def -(that:Nat):Nat
}

object Zero extends Nat{
	
	def isZero = true
	
	def predecessor: Nat = throw new Error("0.predecessor")
	
	def +(that: Nat): Nat = that
	
	def -(that: Nat): Nat = if(that.isZero) this else throw new Error("negative number")
}
class Success(n:Nat) extends Nat{
	
	
	def isZero = false
	
	def predecessor: Nat = n
	
	def +(that:Nat) = new Success(n + that)
	
	def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
}

//Function as Objects

trait Function[A, B]{
	def apply(x:A):B
}

{ //(x:Int) => x*x is expanded to this
	class AnonymousFunction extends Function[Int,Int]{
		
		def apply(x: Int): Int = x*x
	}
	new AnonymousFunction
}//or shorter syntax this way
new Function[Int,Int] {
	def apply(x: Int): Int = x*x
}

trait List[T]{
	def isEmpty:Boolean
	def head:T
	def tail:List[T]
}

class Container[T](val head:T,val tail:List[T]) extends List[T]{
	
	def isEmpty: Boolean = false
}
class Nil[T] extends List[T]{
	
	def isEmpty: Boolean = true
	def head:Nothing = throw  new NoSuchElementException("Nil.head")
	def tail:Nothing = throw  new NoSuchElementException("Nil.tail")
}

object List{
	//List()
	def apply[T](): List[T] = new Nil[T]
	//List(1)
	def apply[T](head:T): List[T] = new Container[T](head,new Nil[T])
	//List(1,2) = List.apply(1,2)
	def apply[T](head:T,last:T):List[T] = new Container[T](head,new Container[T](last,new Nil[T]))
	//List(1,2,3)
	def apply[T](head:T,tail:List[T]): List[T] = new Container[T](head,apply(tail.head,tail.tail))
	
}
List.apply(1,2,3)


/**
  * def Function[S < : T](element:S):S = ...
  * In here S < : T, means S is a subtype of T
  * vice versa S >: T, means S is supertype of T
  *
  * Covariance
  *Given S >:T
  *List[S] >: List[T]?
  *since List is a covariant type
  *List[S] >: List[T] is comparing List[S >: T]
  **/

/** Decomposition
  * normal Object-Orientated Method would need to defined again and again in each class
  * While in Scala ,there is a better solution
  * Pattern Match, slight different to method Switch in programming,
  * Pattern Matching can guard type, method, definition, not only condition expression
  */
case class Number(n:Int) extends Expression
case class Sum(x: Expression, y: Expression) extends Expression
case class Product(x: Expression,y:Expression) extends Expression
case class Test() extends  Expression
trait Expression
{
	def evaluate(e:Expression): Int =e match {
		case Number(n) =>n
		case Sum(x,y) =>evaluate(x)+evaluate(y)
		case Product(x,y)=>evaluate(x)*evaluate(y)
	}
	def show(e:Expression):String = e match {
		case Number(n) => n.toString
		case Sum(x,y) => show(x)+" + "+show(y)
		case Product(x,y) =>(x,y)match {
			case (_:Sum,_) =>"( " + show(x) + " )" +" * " + show(y)
			case (_,_:Sum) => show(x) +" * " + "( " + show(y) + " )"
			case (_,_) =>show(x) + " * " +show(y)
		}
		case _ => null
	}
}



//object Number {
//
//	def apply(n:Int) = new Number(n)
//
//}

//object Sum {
//
//	def apply(x:Expression,y:Expression) = new Sum(x,y)
//
//}
Test().show(Sum(Product(Number(1),Number(3)),Number(3)))
Test().show(Product(Number(1),Sum(Number(1),Number(2))))