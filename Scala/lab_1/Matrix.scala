class Matrix(mat: List[List[Int]]){
    val m = mat

    def == (M: Matrix) = {
        m.equals(M.m)
    }

    def + (M: Matrix) = { new Matrix(
        (for(i<-0 until Integer.min(m.length, M.m.length)) 
            yield (for(j<-0 until Integer.min(m(i).length, M.m(i).length))
                yield m(i)(j) + M.m(i)(j)).toList).toList)
    }

    def T() = { new Matrix(
        (for(j<-0 until m(0).length)
            yield (for(i<-0 until m.length)
                yield m(i)(j)).toList).toList)
    }


    def * (M: Matrix) = { 
        val M_T = M.T
        def aux (MT: Matrix) = {
            f
        }
    }

}