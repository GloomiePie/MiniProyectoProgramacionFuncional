//Simpson 1/3 simple
def SimpsonSimple(f:Double=>Double, a:Int, b:Int) : Double = {

    (b - a) * ((f(a) +(4 * (f((a + b)/2))) + f(b))) / 6

}

//Simpson 1/3 Compuesta
def SimpsonCompuesta(f:Double=>Double, a:Int, b:Double, n:Int) : Double = {

    val h: Double = (b-a)/n

    val calcX = (a:Double, h:Double, j:Double) => a + j * h

    val res : Double = (h/3) *  (1 to n/2).toList.map(x => f(calcX(a, h, 2*x-2))  + 
    4*f(calcX(a, h, 2*x-1))  +  f(calcX(a, h, 2*x))).sum

    res
}

//simpson 1/3 Extendida
def SimpsonExtendida(f:Double=>Double, a:Double, b:Double) : Double = {

    val n = 2 * (b - a)

    val h: Double = (b-a)/n

    val esPar=  (1 to n.toInt-2).toList

    val esImpar = (1 to n.toInt-1).toList

    val res : Double = (h/3) * (f(a) + 4 * esImpar.map(x =>  f(a + x * h)).sum
     + 2 * esPar.map(x =>  f(a + x * h)).sum + f(b))
    res
}

