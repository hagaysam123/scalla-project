
import scala.collection.mutable

object HybridAnomalyDetector extends  AnomalyDetector {

  var al = ZAnomalyDetector
  var bl = LinearRegAnomalyDetector
  var cl = SumSqrAnomalyDetector

  var map_al :Vector[String] = Vector[String]()
  var map_bl :Map[String,String] = Map[String,String]()
  var map_cl :Map[String,String] = Map[String,String]()
  var real_map_al :Map[String,String] = Map[String,String]()

  override def learn(normal: TimeSeries): Map[String, String] = {

    var a = normal.getValues(normal.features(0)).get.toArray
    var b = normal.getValues(normal.features(1)).get.toArray
    var p = Util.pearson(a,b)
    if(p<0) p = p*(-1)
    // We do not need max because its for every pair of features
    if(p>=0.9){
      // LinearRegAnomalyDetector
      map_bl += (normal.features(0) -> normal.features(1))
    }
    else{
      if(p>0.5 && p<0.9){
        // SumSqrAnomalyDetector
        map_cl += (normal.features(0) -> normal.features(1))
      }
      else{
        // ZAnomalyDetector
        if(!map_al.contains(normal.features(0))) map_al = map_al :+ (normal.features(0))
        if(!map_al.contains(normal.features(1))) map_al = map_al :+ (normal.features(1))
      }
    }

    def help_learn(features:Array[String],index: Int, i:Int): Unit = {
      if (index == features.size - 1) return
      else {
        if (i == features.size) {
          help_learn(features, index + 1, index + 2)
        }
        else{
          var a = normal.getValues(normal.features(index)).get.toArray
          var b = normal.getValues(normal.features(i)).get.toArray
          var p = Util.pearson(a,b)
          if(p<0) p = p*(-1)

          if(p>=0.9){
            // LinearRegAnomalyDetector
            map_bl += (normal.features(index) -> normal.features(i))
            help_learn(features,index,i+1)
          }
          else{
            if(p>0.5 && p<0.9){
              // SumSqrAnomalyDetector
              map_cl += (normal.features(index) -> normal.features(i))
              help_learn(features,index,i+1)
            }
            else{
              // ZAnomalyDetector
              if(!map_al.contains(normal.features(index))) map_al = map_al :+ (normal.features(index))
              if(!map_al.contains(normal.features(i))) map_al = map_al :+ (normal.features(i))
              help_learn(features,index,i+1)
            }
          }
        }
      }
    }

    help_learn(normal.features,0,2)
    //  We need to add to the sumsqr new learn function for zscore we dont need (V)
    //  we will not need to use the map, only the detector.detect
    //  in second thoguht we will create vars for the maps like the detectors (V)

    bl.learn2(map_bl,normal)
    cl.learn2(map_cl,normal)
    real_map_al = al.learn2(normal,map_al.toArray)


    return Map[String, String]()
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {

    var hagis = Vector[(String, Int)]()

    var d1 = al.detect(real_map_al,test)
    var d2 = bl.detect(map_bl,test)
    var d3 = cl.detect2(map_cl,test)

    hagis = hagis ++ d1
    hagis = hagis ++ d2
    hagis = hagis ++ d3

    return hagis
  }

}
