import scala.math.{abs, acos, log10}

object Util {

	// max
  def max[A](a: List[A],  bf:(A,A)=>Int): A = {

    def hagis[A](a: List[A],bf:(A,A)=>Int,index: Int, _max :A): A =
    {
       if(index == a.length) return _max
       var _temp : Int = bf(_max,a(index))
       var max2 : A = null.asInstanceOf[A]
       if(_temp >=0) max2 = _max else max2 = a(index)
       hagis(a,bf,index+1,max2)
    }
    var temp : Int = bf(a(0),a(1))
    var max : A = null.asInstanceOf[A]
    if(temp >= 0) max = a(0) else max = a(1)
    var real_max = hagis(a,bf,2,max)
    return real_max

  } :A

	// map
  def map [A,B,C](a: List[A],f:(A)=>B, f2:(B)=>C) : List[C]= {

    def hagis2(maps: Map[A,C],index:Int,a:List[A],f:(A)=>B,f2:(B)=>C): List[C]={
      val map_list = List(maps)
      if(index == a.length) return map_list.flatMap(_.values)
      var l : B = f(a(index))
      var p : C = f2(l)
      var maps2 = maps
      maps2 += (a(index)->p)
      hagis2(maps2,index+1,a,f,f2)
    }
    var l : B = f(a(0))
    var p : C = f2(l)
    val states = Map(a(0)->p)
    //var maps :Map[A,C] = null.asInstanceOf[Map[A,C]]
    //maps += (a(0) ->p)
    var j = hagis2(states,1,a,f,f2)

    return j
  }

	// isSorted
  def isSorted [A](a:List[A],f:(A,A)=>Boolean) : Boolean = {
    def hagis3(a:List[A],f:(A,A)=>Boolean,index:Int) :Boolean= {
      if(index == a.length-1) return true
      var k = f(a(index),a(index+1))
      if( k == false) return false
      hagis3(a,f,index+1)
    }
    var k = f(a(0),a(1))
    if( k == false) return false
    var j = hagis3(a,f,1)

    return  j
  }

	// probs
  def probs (a:Array[Double]) : Array[Double] = {
    var b : Array[Double] = a.clone()
    def count_hagis(a:Array[Double],element: Double, c:Int, index:Int) : Int = {
      var c2 : Int= c
      if(index == a.length) return c2
      if(a(index) == element) c2 = c2+1
      count_hagis(a,element,c2,index+1)
    }
    def hagis4(a:Array[Double], b : Array[Double], index:Int) : Array[Double] = {
      var b2:Array[Double] = b.clone()
      var a2 :Array[Double]= a.clone()
      if(index == a.length) return b
        var c :Int = count_hagis(a, a(index),0,0)
        var d : Double  = c.toDouble/a.length.toDouble
        b2(index) = d
        hagis4(a2,b2,index+1)
    }
    hagis4(a,b,0)
  }

	// entropy
  def entropy(a:Array[Double]) : Double = {
    var log2 = (x: Double) => log10(x)/log10(2.0)
    def hagis5(a:Array[Double],arr:Array[Double],sum:Double,index:Int) : Double = {
      var sum2:Double = sum
      if(index == arr.length) return sum
      var d = a.slice(0,index)
      if(!d.contains(a(index))) sum2 = sum + (arr(index))*log2(arr(index))
      hagis5(a,arr,sum2,index+1)
    }
    var arr : Array[Double] = probs(a)
    var j = hagis5(a,arr,0,0)
    j= (-1)*j

    return j
  }
	// mu
  def mu(a:Array[Double]) : Double = {
    def hagis6(a:Array[Double],arr:Array[Double],sum:Double,index:Int) : Double={
      var sum2:Double = sum
      if(index == arr.length) return sum
      var d = a.slice(0,index)
      if(!d.contains(a(index))) sum2 = sum + arr(index)*a(index)
      hagis6(a,arr,sum2,index+1)
    }
    var arr : Array[Double] = probs(a)
    var j = hagis6(a,arr,0,0)

    return j
   }

	// variance
  def variance(a:Array[Double]) : Double = {
    var moomoo = mu(a)
    var arr : Array[Double] = probs(a)

    def hagis7(a:Array[Double],arr:Array[Double],sum:Double,index:Int) : Double={
      var sum2:Double = sum
      if(index == arr.length) return sum
      var d = a.slice(0,index)
      if(!d.contains(a(index))) sum2 = sum + arr(index)*(Math.pow(a(index)-moomoo,2))
      hagis7(a,arr,sum2,index+1)
    }

    var j = hagis7(a,arr,0,0)
    return j
  }

	// zscore
  def zscore(a:Array[Double],x:Double) : Double = {
    var thoelet= mu(a)
    var stiyat_teken = Math.sqrt(variance((a)))
    var z_score = (x-thoelet)/stiyat_teken

    return  z_score
  }

	// cov
  def cov(a:Array[Double], b:Array[Double]) :Double = {
    var ex = mu(a)
    var ey = mu(b)
    // ----
    var stiyat_teken1 = Math.sqrt(variance((a)))
    var stiyat_teken2 = Math.sqrt(variance((b)))
    // ---
    def l(a:Array[Double], e:Double, index :Int) : Array[Double] = {
      var a2:Array[Double] = a.clone()
      if(index == a2.length) return  a2
      a2(index) = a2(index) -e
      l(a2, e, index+1)
    }
    var x_ = l(a,ex,0)
    var y_ = l(b,ey,0)
    var xy_ = x_
    def l2(a:Array[Double], b:Array[Double], c:Array[Double],index: Int) :Array[Double] = {
      var a2:Array[Double] = a.clone()
      if(index == a2.length) return a2
      a2(index) = b(index)*c(index)
      l2(a2,b,c,index+1)
    }
    xy_ = l2(xy_,x_,y_,0)

    return mu(xy_)
  }

	// pearson
  def pearson(a:Array[Double], b:Array[Double]) :Double = {
    var stiyat_teken1 :Double = Math.sqrt(variance((a)))
    var stiyat_teken2 :Double = Math.sqrt(variance((b)))
    var mehchane :Double = stiyat_teken1*stiyat_teken2
    var mone :Double = cov(a,b)
    var pear:Double = mone.toDouble/mehchane.toDouble
    return pear
  }

  // additional functions
}
