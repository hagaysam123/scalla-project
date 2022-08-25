import scala.collection.mutable


object ZAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {

    // FIRST IN DEBUUGING WE NEED TO SEE IF THE FEATURES ARE GOOD
    val hagis = normal.features // for checking
    // After debugging I can tell its good :)

    val names = normal.features

    // gets all the double arrays
    def help_func(names : Array[String], index:Int, arr: Vector[Array[Double]]) : Array[Array[Double]] = {
      if(index == names.size){
        return arr.toArray
      }
      else{
        var p = normal.getValues(names(index)).get.toArray
        val b = arr :+ p
        help_func(names,index+1,b)
      }
    }
    var h :Vector[Array[Double]] = Vector[Array[Double]]()
    var o = help_func(names,0,h)

    // find the max zscore for one column
    def help_func2(arr :Array[Double], index:Int, max:Double): Double ={
      if(index == arr.size){
        return max
      }
      else {
        if(index == 22)
          {
            var hagis =Util.zscore(arr,arr(index))
            var jjj2 =0
          }
        if(index == 3)
          {
            var hagis2 =  Util.zscore(arr,arr(index))
            var jjj = 0
          }

        var max_ = max
        var zs= Util.zscore(arr,arr(index))
        if(zs <0) zs = zs*(-1)
        if(zs>max) {
          max_ = zs
          help_func2(arr,index+1,max_)
        }
        else{
          help_func2(arr,index+1,max)
        }
      }
    }

    // gets all the maximum z_scores for all columns
    def help_func3(arr :Array[Array[Double]], index:Int, vector: Vector[Double]):Array[Double] ={
      if(index==arr.size){
        return vector.toArray
      }
      else{
        //vector.appended(help_func2(arr(index),0,Util.zscore(arr(index),arr(index)(0))))
        var p = help_func2(arr(index),0,Util.zscore(arr(index),arr(index)(0)))
        val b = vector :+ p
        help_func3(arr,index+1,b)
      }
    }
    var vector : Vector[Double] = Vector[Double]()

    // the z_score results
    var hhh = help_func3(o,0,vector)
    var results_func3 = help_func3(o,0,vector).map(x=>x.toString)

    // all the colums in a string (here they are as rows but we can do iter.tolist insted
    var colums = normal.iter_col

    // Now we need to map between the list of strings to string (the z_score result)

    // ms += (k -> v)
    var cache = collection.mutable.Map[String, String]()
    def jj(arr:Array[String],arr2:Array[String],map :collection.mutable.Map[String,String],index:Int) : collection.mutable.Map[String,String] = {
      if(index ==arr.length){
        return map
      }
      else{
        var kk = map.clone()
        kk += (arr(index) -> arr2(index))
        jj(arr, arr2, kk, index+1)
      }
    }

    var final_ =jj(normal.features,results_func3,cache,0)

    return  final_.toMap
  }

  def learn2(normal: TimeSeries,names:Array[String]): Map[String, String] ={

    def help_func(names : Array[String], index:Int, arr: Vector[Array[Double]]) : Array[Array[Double]] = {
      if(index == names.size){
        return arr.toArray
      }
      else{
        var p = normal.getValues(names(index)).get.toArray
        val b = arr :+ p
        help_func(names,index+1,b)
      }
    }
    var h :Vector[Array[Double]] = Vector[Array[Double]]()
    var o = help_func(names,0,h)

    // find the max zscore for one column
    def help_func2(arr :Array[Double], index:Int, max:Double): Double ={
      if(index == arr.size){
        return max
      }
      else {
        if(index == 22)
        {
          var hagis =Util.zscore(arr,arr(index))
          var jjj2 =0
        }
        if(index == 3)
        {
          var hagis2 =  Util.zscore(arr,arr(index))
          var jjj = 0
        }

        var max_ = max
        var zs= Util.zscore(arr,arr(index))
        if(zs <0) zs = zs*(-1)
        if(zs>max) {
          max_ = zs
          help_func2(arr,index+1,max_)
        }
        else{
          help_func2(arr,index+1,max)
        }
      }
    }

    // gets all the maximum z_scores for all columns
    def help_func3(arr :Array[Array[Double]], index:Int, vector: Vector[Double]):Array[Double] ={
      if(index==arr.size){
        return vector.toArray
      }
      else{
        //vector.appended(help_func2(arr(index),0,Util.zscore(arr(index),arr(index)(0))))
        var p = help_func2(arr(index),0,Util.zscore(arr(index),arr(index)(0)))
        val b = vector :+ p
        help_func3(arr,index+1,b)
      }
    }
    var vector : Vector[Double] = Vector[Double]()

    // the z_score results
    var hhh = help_func3(o,0,vector)
    var results_func3 = help_func3(o,0,vector).map(x=>x.toString)

    // all the colums in a string (here they are as rows but we can do iter.tolist insted
    var colums = normal.iter_col

    // Now we need to map between the list of strings to string (the z_score result)

    // ms += (k -> v)
    var cache = collection.mutable.Map[String, String]()
    def jj(arr:Array[String],arr2:Array[String],map :collection.mutable.Map[String,String],index:Int) : collection.mutable.Map[String,String] = {
      if(index ==arr.length){
        return map
      }
      else{
        var kk = map.clone()
        kk += (arr(index) -> arr2(index))
        jj(arr, arr2, kk, index+1)
      }
    }

    var final_ =jj(names,results_func3,cache,0)

    return  final_.toMap
  }


  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {


    def help_cmp(model: Map[String, String], index:Int,vector: Vector[(String,Int)]): Vector[(String,Int)] ={

        if(index == model.size){
          return vector
        }
        val vals = test.getValues(model.keys.toList(index)).get
        var z_score_cmp = model(model.keys.toList(index)).toDouble
        if(z_score_cmp < 0) z_score_cmp = z_score_cmp*(-1)

        def help_cmp2(values : Vector[Double], z_score_cmp :Double, i:Int, index:Int,vector: Vector[(String,Int)]): Vector[(String,Int)] ={
            if(i == values.size){
               return vector
            }
            else {
              var zscore_tmp = Util.zscore(values.toArray,values.toList(i))
              if(zscore_tmp <0) zscore_tmp = zscore_tmp*(-1)
              if(zscore_tmp > z_score_cmp){
                // append
                val b = vector :+ (model.keys.toList(index),i)
                help_cmp2(values,z_score_cmp,i+1,index,b)
              }
              else help_cmp2(values,z_score_cmp,i+1,index,vector)
            }
        }
      val tmp :Vector[(String,Int)] = Vector[(String,Int)]()
      val harigot :Vector[(String,Int)] = help_cmp2(vals,z_score_cmp,0,index,tmp)
      if(harigot.size !=0){
        val b :Vector[(String,Int)] = vector ++ harigot
        help_cmp(model, index+1, b)
      }
      else help_cmp(model,index+1,vector)
    }

    val vec : Vector[(String,Int)]= Vector[(String,Int)]()
    val final_vec = help_cmp(model,0,vec)

    return  final_vec
  }


}
