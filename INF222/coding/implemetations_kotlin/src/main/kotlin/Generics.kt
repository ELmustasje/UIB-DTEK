package src.main.kotlin

class Generics<T>(thing: T) {
  var thing = thing

  fun get_thing(): T {
    return thing
  }

  companion object {
    fun <T> getFirst(arr: Array<T>): T {
      return arr[0]
    }
    fun <T : Number> add_generics(a: T, b: T): Double {
      return a.toDouble() + b.toDouble()
    }
  }
}

