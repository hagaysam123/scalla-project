import Util.{cov, variance}

class Line(ps:Array[Point]) {

  def getallx(xl: Array[Double],ps:Array[Point],index: Int): Array[Double] =
  {
    if(index == ps.length) return xl
    var xl2 = xl.clone()
    xl2(index) = ps(index).x
    getallx(xl2,ps, index+1)
  }

  def getally(yl: Array[Double],ps:Array[Point],index: Int): Array[Double] =
  {
    if(index == ps.length) return yl
    var yl2 = yl.clone()
    yl2(index) = ps(index).y
    getally(yl2,ps, index+1)
  }

	// read only values a and b
  def a:Double = {
    // Cov(x,y)/ var(x)

    var xl : Array[Double] = Array.fill[Double](ps.length)(0)
    var all_x :Array[Double] = getallx(xl,ps,0)
    var arr_all_x = all_x.toArray

    var yl : Array[Double] = Array.fill[Double](ps.length)(0)
    var all_y :Array[Double] = getally(yl,ps,0)
    var arr_all_y = all_y.toArray

    var mone: Double = cov(arr_all_x,arr_all_y)
    var mechane : Double = variance(arr_all_x)

    var real_a :Double= mone.toDouble/mechane.toDouble
    return  real_a
  }

  def b: Double = {
    var xl : Array[Double] = Array.fill[Double](ps.length)(0)
    var all_x :Array[Double] = getallx(xl,ps,0)
    var arr_all_x = all_x.toArray

    var yl : Array[Double] = Array.fill[Double](ps.length)(0)
    var all_y :Array[Double] = getally(yl,ps,0)
    var arr_all_y = all_y.toArray

    var sumx :Double = arr_all_x.sum
    var sumy: Double = arr_all_y.sum

    var avgx : Double = (sumx.toDouble)/(arr_all_x.length.toDouble)
    var avgy : Double = (sumy.toDouble)/(arr_all_y.length.toDouble)

    var real_b :Double = avgy - this.a*avgx
    return real_b
  }

	// f
  def f(x:Double) :Double = {
    return this.a*x + this.b
  }
	// dist
  def dist(p: Point) : Double = {
    // |f(x) - y|
    var dist_ :Double = this.f(p.x) - p.y
    if(dist_ < 0) dist_ = dist_ *(-1)

    return dist_
  }
}
