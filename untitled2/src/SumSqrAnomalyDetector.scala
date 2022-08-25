
import scala.collection.mutable

object SumSqrAnomalyDetector extends  AnomalyDetector {

  var l : List[Line]= List[Line]()
  var arr_points : List[Array[Point]] = List[Array[Point]]()
  var normal_dist : List[Double] = List[Double]()

  override def learn(normal: TimeSeries): Map[String, String] = {

    // HERE WE FIND FEATURES THAT HAVE CORRELATION
    ///////
    var max:Double =0
    var map :Map[String,String] = Map[String,String]()
    var a = normal.getValues(normal.features(0)).get.toArray
    var b = normal.getValues(normal.features(1)).get.toArray
    var p = Util.pearson(a,b)
    if(p<0) p = p*(-1)
    if(p>=0.9 && p>max){
      map += (normal.features(0) -> normal.features(1))
      max = p
    }

    def help_learn(features:Array[String],index: Int, i:Int,max:Double, map :Map[String, String]): Map[String, String] = {
      if (index == features.size - 1) return map
      else {
        if (i == features.size) {
          help_learn(features, index + 1, index + 2, 0, map)
        }
        else{
          var a = normal.getValues(normal.features(index)).get.toArray
          var b = normal.getValues(normal.features(i)).get.toArray
          var p = Util.pearson(a,b)
          if(p<0) p = p*(-1)
          var kk :Map[String,String] = Map[String,String]()
          var max_ = max
          if(p>=0.9 && p>max){
            kk = kk ++ map
            kk += (normal.features(index) -> normal.features(i))
            max_ = p
            help_learn(features,index,i+1,max_,kk)
          }
          else{
            help_learn(features, index, i+1, max, map)
          }
        }
      }
    }

    var map_fin = help_learn(normal.features,0,2,max, map)
    ///////////
    // HERE WE MAKE THE ARRAY OF POINTS
    ///////////
    if(l.size !=0) l = l.drop(l.size)
    if(arr_points.size!=0) arr_points = arr_points.drop(arr_points.size)
    def make_lines(map :Map[String, String],index:Int): List[Line] ={
      if(index == map.size) return l
      else{
        var x :Array[Double]=normal.getValues(map.keys.toList(index)).get.toArray
        var y :Array[Double]=normal.getValues(map.values.toList(index)).get.toArray
        def make_points(x :Array[Double],y :Array[Double], index:Int,vec:Vector[Point]):Array[Point]={
          if(index == x.size) return vec.toArray
          else{
            var pp:Point = new Point(x(index),y(index))
            val b = vec :+ pp
            make_points(x,y,index+1,b)
          }
        }
        var vec_2 :Vector[Point] = Vector[Point]()
        var arr_p :Array[Point] = make_points(x,y,0,vec_2)
        //arr_points.appended(arr_p)
        arr_points = arr_points:+arr_p
        var line : Line = new Line(arr_p)
        //l.appended(line)
        l = l:+line
        make_lines(map,index+1)
      }
    }
    /////////////////////////////////////////////
    /// find the distance between 2 points
    def dist(arr :Array[Point],i:Int,index:Int):Double = {
      val p1:Point = arr(i)
      val p2:Point = arr(index)

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return dist_
    }

    def sqrSum(arr :Array[Point],i:Int,sum:Double,index:Int) :Double = {
      if(index== arr.size) return sum
      else{
        if(index == i) sqrSum(arr,i,sum, index+1)
        else{
          var s = sum + dist(arr, i, index)
          sqrSum(arr,i,s,index+1)
        }
      }
    }

    // find the max dist from array of points
    def find_max_dist(arr :Array[Point], max:Double,i:Int) : Double = {
      if(i==arr.size){
        return max
      }
      else {
        var max_ = max
        var dis = sqrSum(arr,i,0,0)
        if (dis > max) find_max_dist(arr, dis, i+1)
        else find_max_dist(arr, max, i+1)
        }
    }

    /// find all the maximus
    l = make_lines(map_fin,0)
    def find_all_dist(arr_points : List[Array[Point]],normal_dist : List[Double],index:Int): List[Double] ={
      if(index == l.size) return normal_dist
      else{
        var maxo = -1
        // need to fix real_maxo and find_max_dist, to find the maximum sum of dist from all points for every point
        var real_maxo = find_max_dist(arr_points(index),maxo,0)
        var normal_dist2 : List[Double] = normal_dist
        //normal_dist2.appended(real_maxo)
        normal_dist2 = normal_dist2 :+real_maxo
        find_all_dist(arr_points, normal_dist2, index+1)
      }
    }

    if(normal_dist.size !=0) normal_dist = normal_dist.drop(normal_dist.size)
    normal_dist = find_all_dist(arr_points,normal_dist,0)

    return map_fin
  }

  def learn2(map_fin :Map[String, String],normal: TimeSeries) : Map[String, String] = {

    // HERE WE MAKE THE ARRAY OF POINTS
    ///////////
    if(l.size !=0) l = l.drop(l.size)
    if(arr_points.size!=0) arr_points = arr_points.drop(arr_points.size)
    def make_lines(map :Map[String, String],index:Int): List[Line] ={
      if(index == map.size) return l
      else{
        var x :Array[Double]=normal.getValues(map.keys.toList(index)).get.toArray
        var y :Array[Double]=normal.getValues(map.values.toList(index)).get.toArray
        def make_points(x :Array[Double],y :Array[Double], index:Int,vec:Vector[Point]):Array[Point]={
          if(index == x.size) return vec.toArray
          else{
            var pp:Point = new Point(x(index),y(index))
            val b = vec :+ pp
            make_points(x,y,index+1,b)
          }
        }
        var vec_2 :Vector[Point] = Vector[Point]()
        var arr_p :Array[Point] = make_points(x,y,0,vec_2)
        //arr_points.appended(arr_p)
        arr_points = arr_points:+arr_p
        var line : Line = new Line(arr_p)
        //l.appended(line)
        l = l:+line
        make_lines(map,index+1)
      }
    }
    /////////////////////////////////////////////
    /// find the distance between 2 points
    def dist(arr :Array[Point],i:Int,index:Int):Double = {
      val p1:Point = arr(i)
      val p2:Point = arr(index)

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return dist_
    }

    def dist_shoresh(point1:Point,point2:Point):Double = {
      val p1:Point = point1
      val p2:Point = point2

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return Math.sqrt(dist_)
    }

    def sqrSum(arr :Array[Point],i:Int,sum:Double,index:Int) :Double = {
      if(index== arr.size) return sum
      else{
        if(index == i) sqrSum(arr,i,sum, index+1)
        else{
          var s = sum + dist(arr, i, index)
          sqrSum(arr,i,s,index+1)
        }
      }
    }

    // find the point with max dist from array of points
    // find max dist from min_point
    // if it will be distance between same points its ok because we searching for maximum
    def find_max_dist(arr :Array[Point], max:Double,min_point:Point,i:Int) : Double = {
      if(i==arr.size){
        return max
      }
      else {
        var max_ = max
        // instead of sqrsum we will do dist (with shoresh)
        var dis = dist_shoresh(min_point,arr(i))
        if (dis > max) {
          find_max_dist(arr, dis,min_point,i+1)
        }
        else find_max_dist(arr, max,min_point,i+1)
      }
    }

    // find the point with max dist from array of points
    def find_min_dist(arr :Array[Point], max:Double,max_point:Point,i:Int) : Point = {
      if(i==arr.size){
        return max_point
      }
      else {
        var max_ = max
        var dis = sqrSum(arr,i,0,0)
        if (dis < max) {
          find_min_dist(arr, dis,arr(i),i+1)
        }
        else find_min_dist(arr, max,max_point,i+1)
      }
    }

    /// find all the maximus
    l = make_lines(map_fin,0)
    def find_all_dist(arr_points : List[Array[Point]],normal_dist : List[Double],index:Int): List[Double] ={
      if(index == l.size) return normal_dist
      else{
        var maxo = -1
        // need to fix real_maxo and find_max_dist, to find the maximum sum of dist from all points for every point
        var real_mino = find_min_dist(arr_points(index),maxo,arr_points(index)(0),0)
        var real_maxo = find_max_dist(arr_points(index),maxo,real_mino,0)

        var normal_dist2 : List[Double] = normal_dist
        //normal_dist2.appended(real_maxo)
        normal_dist2 = normal_dist2 :+real_maxo
        find_all_dist(arr_points, normal_dist2, index+1)
      }
    }

    if(normal_dist.size !=0) normal_dist = normal_dist.drop(normal_dist.size)
    normal_dist = find_all_dist(arr_points,normal_dist,0)

    return map_fin

  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {

    def dist(arr :Array[Point],i:Int,index:Int):Double = {
      val p1:Point = arr(i)
      val p2:Point = arr(index)

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return dist_
    }

    def sqrSum(arr :Array[Point],i:Int,sum:Double,index:Int) :Double = {
      if(index== arr.size) return sum
      else{
        if(index == i) sqrSum(arr,i,sum, index+1)
        else{
          var s = sum + dist(arr, i, index)
          sqrSum(arr,i,s,index+1)
        }
      }
    }

    // find the max dist from array of points
    def find_max_dist(arr :Array[Point], max:Double,i:Int) : Double = {
      if(i==arr.size){
        return max
      }
      else {
        var max_ = max
        var dis = sqrSum(arr,i,0,0)
        if (dis > max) find_max_dist(arr, dis, i+1)
        else find_max_dist(arr, max, i+1)
      }
    }

    def x_y(model: Map[String, String], test: TimeSeries,index:Int,vec:Vector[(String, Int)]): Vector[(String, Int)] ={
      if(index == model.size) return vec

      var x :Array[Double]= test.getValues(model.keys.toList(index)).get.toArray
      var y :Array[Double]= test.getValues(model.values.toList(index)).get.toArray
      def make_points(x :Array[Double],y :Array[Double], index:Int,vec:Vector[Point]):Array[Point]={
        if(index == x.size) return vec.toArray
        else{
          var pp:Point = new Point(x(index),y(index))
          val b = vec :+ pp
          make_points(x,y,index+1,b)
        }
      }
      var vec_2 :Vector[Point] = Vector[Point]()
      var arr_p :Array[Point] = make_points(x,y,0,vec_2)

      def check_dist(arr_p :Array[Point],i:Int,vec:Vector[(String, Int)],index:Int):Vector[(String, Int)] = {
        if(i == arr_p.size) return vec
        else{
          //val j1 = find_max_dist(arr_p,-1,0)
          val j11 = sqrSum(arr_p,i,0,0)
          val j2 = normal_dist(index)
          if(j11>j2){
            var kk :Vector[(String, Int)] = Vector[(String, Int)]()
            kk = kk ++ vec
            //kk += (model.keys.toList(index) -> model.values.toList(index))
            var j : String = model.keys.toList(index)+","+model.values.toList(index)
            //kk.appended((j,i))
            kk = kk :+(j,i)
            check_dist(arr_p, i+1, kk, index)
          }
          else{
            check_dist(arr_p, i+1, vec, index)
          }
        }
      }

      var kk2 :Vector[(String, Int)] = Vector[(String, Int)]()
      kk2 = check_dist(arr_p,0,kk2,index)

      var jj2 :Vector[(String, Int)] = Vector[(String, Int)]()
      jj2 = jj2++vec
      jj2 = jj2++kk2

      x_y(model, test, index+1, jj2)
    }

    var hagis :Vector[(String, Int)] = Vector[(String, Int)]()
    hagis = x_y(model,test,0,hagis)

    return hagis
  }

  def detect2(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {

    def dist(arr :Array[Point],i:Int,index:Int):Double = {
      val p1:Point = arr(i)
      val p2:Point = arr(index)

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return dist_
    }
    def dist_shoresh(point1:Point,point2:Point):Double = {
      val p1:Point = point1
      val p2:Point = point2

      val plusx  = (p1.x-p2.x)*(p1.x-p2.x)
      val plusy = (p1.y-p2.y)*(p1.y-p2.y)

      val dist_ = plusx+plusy
      return Math.sqrt(dist_)
    }
    def sqrSum(arr :Array[Point],i:Int,sum:Double,index:Int) :Double = {
      if(index== arr.size) return sum
      else{
        if(index == i) sqrSum(arr,i,sum, index+1)
        else{
          var s = sum + dist(arr, i, index)
          sqrSum(arr,i,s,index+1)
        }
      }
    }
    // find the point with max dist from array of points
    // find max dist from min_point
    // if it will be distance between same points its ok because we searching for maximum
    def find_max_dist(arr :Array[Point], max:Double,min_point:Point,i:Int) : Double = {
      if(i==arr.size){
        return max
      }
      else {
        var max_ = max
        // instead of sqrsum we will do dist (with shoresh)
        var dis = dist_shoresh(min_point,arr(i))
        if (dis > max) {
          find_max_dist(arr, dis,min_point,i+1)
        }
        else find_max_dist(arr, max,min_point,i+1)
      }
    }
    // find the point with max dist from array of points
    def find_min_dist(arr :Array[Point], max:Double,max_point:Point,i:Int) : Point = {
      if(i==arr.size){
        return max_point
      }
      else {
        var max_ = max
        var dis = sqrSum(arr,i,0,0)
        if (dis < max) {
          find_min_dist(arr, dis,arr(i),i+1)
        }
        else find_min_dist(arr, max,max_point,i+1)
      }
    }


    def x_y(model: Map[String, String], test: TimeSeries,index:Int,vec:Vector[(String, Int)]): Vector[(String, Int)] ={
      if(index == model.size) return vec

      var x :Array[Double]= test.getValues(model.keys.toList(index)).get.toArray
      var y :Array[Double]= test.getValues(model.values.toList(index)).get.toArray
      def make_points(x :Array[Double],y :Array[Double], index:Int,vec:Vector[Point]):Array[Point]={
        if(index == x.size) return vec.toArray
        else{
          var pp:Point = new Point(x(index),y(index))
          val b = vec :+ pp
          make_points(x,y,index+1,b)
        }
      }
      var vec_2 :Vector[Point] = Vector[Point]()
      var arr_p :Array[Point] = make_points(x,y,0,vec_2)

      //
      def check_dist(arr_p :Array[Point],i:Int,vec:Vector[(String, Int)],index:Int):Vector[(String, Int)] = {
        if(i == arr_p.size) return vec
        else{
          //val j1 = find_max_dist(arr_p,-1,0)
          var real_mino = find_min_dist(arr_p,-1,arr_p(index),0)

          // instead of sqrsum will do dist_shoresh between min_point and arr_p(i)
          // val j11 = sqrSum(arr_p,i,0,0)
          val j11 = dist_shoresh(real_mino,arr_p(i))
          val j2 = normal_dist(index)
          if(j11>j2){
            var kk :Vector[(String, Int)] = Vector[(String, Int)]()
            kk = kk ++ vec
            //kk += (model.keys.toList(index) -> model.values.toList(index))
            var j : String = model.keys.toList(index)+","+model.values.toList(index)
            //kk.appended((j,i))
            kk = kk :+(j,i)
            check_dist(arr_p, i+1, kk, index)
          }
          else{
            check_dist(arr_p, i+1, vec, index)
          }
        }
      }

      var kk2 :Vector[(String, Int)] = Vector[(String, Int)]()
      kk2 = check_dist(arr_p,0,kk2,index)

      var jj2 :Vector[(String, Int)] = Vector[(String, Int)]()
      jj2 = jj2++vec
      jj2 = jj2++kk2

      x_y(model, test, index+1, jj2)
    }

    var hagis :Vector[(String, Int)] = Vector[(String, Int)]()
    hagis = x_y(model,test,0,hagis)

    return hagis

  }
}
