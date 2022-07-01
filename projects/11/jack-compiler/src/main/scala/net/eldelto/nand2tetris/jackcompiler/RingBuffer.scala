package net.eldelto.nand2tetris.jackcompiler

import scala.reflect.ClassTag

class RingBuffer[A](val size: Int)(implicit ct: ClassTag[A]) {
    private val array: Array[A] = new Array(size)
    private var arrayIndex = 0
    
    def append(elem: A): Unit = {
        array.update(arrayIndex, elem)
        arrayIndex = (arrayIndex + 1) % size
    }

    def get(index: Int): Option[A] = Option(array(actualIndex(index)))

    private def actualIndex(index: Int): Int = (size + (arrayIndex-1) - index) % size
}
