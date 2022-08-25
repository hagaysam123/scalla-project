import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector{



  override def map(ts: TimeSeries):Reports={
    var l= ts.features
    def run_on_features(l:Array[String],index_feat:Int,list:Reports): Reports ={
      if(index_feat == l.size){
        return list
      }
      else{
        var vec = ts.getValues(l(index_feat)).get
        if(ts.original_ts != null)
          vec = ts.original_ts.getValues(l(index_feat)).get

        var hx = Util.entropy(vec.toArray)
        def max_feature_entropy(vec: Vector[Double],index:Int, max:Double,max_report:Report):Report ={
          if(index ==vec.size)
            return max_report
          else{
            def vec_without_element(vecd:Vector[Double],vec2:Vector[Double],i:Int,index:Int):Vector[Double] = {
              if(index==vecd.size)
                return vec2
              else{
                if(index==i)
                  vec_without_element(vecd,vec2,i,index+1)
                else{
                  var vec_2 = vec2
                  vec_2 = vec_2 :+ vecd(index)
                  vec_without_element(vecd,vec_2,i,index+1)
                }
              }
            }
            var vec_without = vec_without_element(vec,Vector[Double](),index,0)
            var h_X_minux_xi = Util.entropy(vec_without.toArray)
            var hefresh = hx- h_X_minux_xi
            if(hefresh>max){
              var max_2 = hefresh
              var rep = new Report(l(index_feat),index+ts.i_futures*ts.chunks,hefresh) // maybe well change the index in the future
              max_feature_entropy(vec,index+1,max_2,rep)
            }
            else{
              max_feature_entropy(vec,index+1,max, max_report)
            }
          }
        }
        var add = max_feature_entropy(vec,0,-9999999,Report(" ",0,0))
        var list_new = list
        list_new = list_new :+ add
        run_on_features(l,index_feat+1,list_new)
      }
    }
    var list :Reports = new Reports()
    var ret_val = run_on_features(l,0,list)
    return ret_val
  }

  override def reduce(r1:Reports,r2:Reports):Reports= {

    def reduce2(r1:Reports,r2:Reports,index:Int,r_new :Reports):Reports = {
      if(index == r1.size)
        return r_new
      else{
        if(r1(index).anomalyScore>r2(index).anomalyScore)
          {
            var r_new_2 = r_new
            var r_report : Report = r1(index)
            r_new_2 = r_new_2 :+ r_report
            reduce2(r1,r2,index+1,r_new_2)
          }
        else
        {
          var r_new_2 = r_new
          var r_report : Report = r2(index)
          r_new_2 = r_new_2 :+ r_report
          reduce2(r1,r2,index+1,r_new_2)
        }
      }
    }
    var list :Reports = new Reports()
    var ret_val = reduce2(r1,r2,0,list)
    return ret_val
  }
}
