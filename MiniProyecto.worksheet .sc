//Simpson 1/3 simple
def SimpsonSimple(f:Double=>Double, a:Int, b:Int) : Double = {

    (b - a) * ((f(a) +(4 * (f((a + b)/2))) + f(b))) / 6

}
