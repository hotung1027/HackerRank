package intro

/**
  * Created by Randyt on 2017/7/5.
  */
object intro {
	def rev(arr:List[Int]):List[Int] = arr match{
		case Nil=>List()
		case x::Nil=>List(x)
		case x::xs=>rev(xs)++List(x)
	}

}
