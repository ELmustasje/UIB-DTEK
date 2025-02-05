package src.main.kotlin

class Varience<out T: Number>(private val num: T) {
    //Covariant = out = output = Producer Class = assign Class of subtype to class of supertype
    //covarience means readonly; you can only use it as a producer (output)
    //covarience can be more spesific

    fun giveValue(): T = num
    //contravarience = in = input
    //contravarience means a type is write-only; you can use it as a consumer (input)
    //contravarience can be more general
}

class Varience_in<in T> {
    fun print_value(value: T){
        println(value)
    }
}
