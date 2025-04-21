package src.main.kotlin

class Varience<out T: Number>(private val num: T) {
    //Covariant = out = output = Producer Class = assign Class of subtype to class of supertype
    //Covarience means readonly; you can only use it as a producer (output)
    //Covarience can be more spesific

    fun giveValue(): T = num
    //Contravarience = in = input
    //Contravarience means a type is write-only; you can use it as a consumer (input)
    //Contravarience can be more general
}

class Varience_in<in T> {
    fun print_value(value: T){
        println(value)
    }
}
