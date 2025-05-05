import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._

package object Matrices {
  val random = new Random()

  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    // Crea una matriz de enteros cuadrada de long x long,
    // con valores aleatorios entre 0 y vals
    Vector.fill(long, long)(random.nextInt(vals))
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2T = transpuesta(m2) // Transponemos m2 para facilitar el producto punto
    m1.map { fila =>
      m2T.map { columna =>
        prodPunto(fila, columna) // Producto punto clásico
      }
    }
  }

  // Ejercicio 1.1.2
  def multMatrizParD(m1: Matriz, m2: Matriz): Matriz = {
    val m2T = transpuesta(m2)
    m1.par.map { fila => // Paralelizamos sobre las filas
      m2T.map { columna =>
        prodPunto(fila, columna)
      }
    }.toVector // Convertimos de vuelta a colección secuencial
  }

  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    // Extrae las filas desde i hasta i + l
    m.slice(i, i + l).map(fila => fila.slice(j, j + l))
  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    m1.zip(m2).map { case (fila1, fila2) =>
      fila1.zip(fila2).map { case (x, y) => x + y }
    }
  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      // Caso base: matrices de 1x1
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mid = n / 2

      require(mid >= 1, "El tamaño de la matriz no es potencia de 2")

      // Dividir m1 y m2 en submatrices
      val (a, b, c, d) = (
        subMatriz(m1, 0, 0, mid),
        subMatriz(m1, 0, mid, mid),
        subMatriz(m1, mid, 0, mid),
        subMatriz(m1, mid, mid, mid)
      )
      val (e, f, g, h) = (
        subMatriz(m2, 0, 0, mid),
        subMatriz(m2, 0, mid, mid),
        subMatriz(m2, mid, 0, mid),
        subMatriz(m2, mid, mid, mid)
      )

      // Multiplicaciones recursivas
      val p1 = multMatrizRec(a, e)
      val p2 = multMatrizRec(b, g)
      val p3 = multMatrizRec(a, f)
      val p4 = multMatrizRec(b, h)
      val p5 = multMatrizRec(c, e)
      val p6 = multMatrizRec(d, g)
      val p7 = multMatrizRec(c, f)
      val p8 = multMatrizRec(d, h)

      // Sumar los resultados parciales
      val r1 = sumMatriz(p1, p2)
      val r2 = sumMatriz(p3, p4)
      val r3 = sumMatriz(p5, p6)
      val r4 = sumMatriz(p7, p8)

      // Combinar en una sola matriz
      unirMatrices(r1, r2, r3, r4)
    }
  }

  def unirMatrices(
                    a: Matriz, b: Matriz,
                    c: Matriz, d: Matriz
                  ): Matriz = {
    val arriba = a.zip(b).map { case (filaA, filaB) => filaA ++ filaB }
    val abajo = c.zip(d).map { case (filaC, filaD) => filaC ++ filaD }
    arriba ++ abajo
  }


  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mid = n / 2

      val (a, b, c, d) = (
        subMatriz(m1, 0, 0, mid),
        subMatriz(m1, 0, mid, mid),
        subMatriz(m1, mid, 0, mid),
        subMatriz(m1, mid, mid, mid)
      )
      val (e, f, g, h) = (
        subMatriz(m2, 0, 0, mid),
        subMatriz(m2, 0, mid, mid),
        subMatriz(m2, mid, 0, mid),
        subMatriz(m2, mid, mid, mid)
      )

      // Lanzamos las tareas en paralelo
      val t1 = task {
        multMatrizRecPar(a, e)
      }
      val t2 = task {
        multMatrizRecPar(b, g)
      }
      val t3 = task {
        multMatrizRecPar(a, f)
      }
      val t4 = task {
        multMatrizRecPar(b, h)
      }
      val t5 = task {
        multMatrizRecPar(c, e)
      }
      val t6 = task {
        multMatrizRecPar(d, g)
      }
      val t7 = task {
        multMatrizRecPar(c, f)
      }
      val t8 = task {
        multMatrizRecPar(d, h)
      }

      // Esperamos y sumamos los resultados parciales
      val r1 = sumMatriz(t1.join(), t2.join())
      val r2 = sumMatriz(t3.join(), t4.join())
      val r3 = sumMatriz(t5.join(), t6.join())
      val r4 = sumMatriz(t7.join(), t8.join())

      unirMatrices(r1, r2, r3, r4)
    }
  }

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // Resta elemento a elemento de dos matrices del mismo tamaño
    m1.zip(m2).map { case (fila1, fila2) =>
      fila1.zip(fila2).map { case (a, b) => a - b }
    }
  }

  // Ejercicio 1.3.2
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mid = n / 2

      val (a, b, c, d) = (
        subMatriz(m1, 0, 0, mid),
        subMatriz(m1, 0, mid, mid),
        subMatriz(m1, mid, 0, mid),
        subMatriz(m1, mid, mid, mid)
      )
      val (e, f, g, h) = (
        subMatriz(m2, 0, 0, mid),
        subMatriz(m2, 0, mid, mid),
        subMatriz(m2, mid, 0, mid),
        subMatriz(m2, mid, mid, mid)
      )

      // 7 productos intermedios
      val p1 = multStrassen(a, restaMatriz(f, h))
      val p2 = multStrassen(sumMatriz(a, b), h)
      val p3 = multStrassen(sumMatriz(c, d), e)
      val p4 = multStrassen(d, restaMatriz(g, e))
      val p5 = multStrassen(sumMatriz(a, d), sumMatriz(e, h))
      val p6 = multStrassen(restaMatriz(b, d), sumMatriz(g, h))
      val p7 = multStrassen(restaMatriz(a, c), sumMatriz(e, f))

      // Combinación final
      val r1 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val r2 = sumMatriz(p1, p2)
      val r3 = sumMatriz(p3, p4)
      val r4 = restaMatriz(restaMatriz(sumMatriz(p1, p5), p3), p7)

      unirMatrices(r1, r2, r3, r4)
    }
  }

  // Ejercicio 1.3.3
  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val mid = n / 2

      // Subdividir m1 y m2 en bloques
      val (a, b, c, d) = (
        subMatriz(m1, 0, 0, mid),
        subMatriz(m1, 0, mid, mid),
        subMatriz(m1, mid, 0, mid),
        subMatriz(m1, mid, mid, mid)
      )
      val (e, f, g, h) = (
        subMatriz(m2, 0, 0, mid),
        subMatriz(m2, 0, mid, mid),
        subMatriz(m2, mid, 0, mid),
        subMatriz(m2, mid, mid, mid)
      )

      // Computar productos intermedios en paralelo
      val (p1, p2, p3, p4) = parallel(
        multStrassenPar(a, restaMatriz(f, h)),
        multStrassenPar(sumMatriz(a, b), h),
        multStrassenPar(sumMatriz(c, d), e),
        multStrassenPar(d, restaMatriz(g, e))
      )

      // Usar tareas individuales para p5, p6, p7
      val t5 = task { multStrassenPar(sumMatriz(a, d), sumMatriz(e, h)) }
      val t6 = task { multStrassenPar(restaMatriz(b, d), sumMatriz(g, h)) }
      val t7 = task { multStrassenPar(restaMatriz(a, c), sumMatriz(e, f)) }
      val p5 = t5.join()
      val p6 = t6.join()
      val p7 = t7.join()

      // Combinar resultados en los bloques de la matriz resultante
      val r1 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val r2 = sumMatriz(p1, p2)
      val r3 = sumMatriz(p3, p4)
      val r4 = restaMatriz(restaMatriz(sumMatriz(p1, p5), p3), p7)

      unirMatrices(r1, r2, r3, r4)
    }
  }
}