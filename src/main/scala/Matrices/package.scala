import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector

package object Matrices {
  val random = new Random()
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long, long)(random.nextInt(vals))
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    Vector.fill(long)(random.nextInt(vals))
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => i * j }).sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => i * j }).sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(n, n)((i, j) => prodPunto(m1(i), m2T(j)))
  }

  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val m2T = transpuesta(m2)
    Vector.tabulate(n, n)((i, j) => {
      val (v1, v2) = parallel(m1(i), m2T(j))
      prodPunto(v1, v2)
    })
  }

  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l)((x, y) => m(i + x)(j + y))
  }

  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n)((i, j) => m1(i)(j) + m2(i)(j))
  }

  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 2) {
      multMatriz(m1, m2)
    } else {
      val l = n / 2

      // Submatrices de m1
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      // Submatrices de m2
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Cálculo de las submatrices de C
      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      // Combinar las submatrices en una sola
      Vector.tabulate(n, n)((i, j) => {
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      })
    }
  }

  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 2) {
      multMatriz(m1, m2)
    } else {
      val l = n / 2

      // Submatrices de m1
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      // Submatrices de m2
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Cálculo paralelo de las submatrices de C
      val ((c11, c12), (c21, c22)) = parallel(
        parallel(
          sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21)),
          sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22))
        ),
        parallel(
          sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21)),
          sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22))
        )
      )

      // Combinar las submatrices en una sola
      Vector.tabulate(n, n)((i, j) => {
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      })
    }
  }

  // Ejercicio 1.3.1
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n)((i, j) => m1(i)(j) - m2(i)(j))
  }

  // Ejercicio 1.3.2
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 2) {
      multMatriz(m1, m2)
    } else {
      val l = n / 2

      // Submatrices de m1
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      // Submatrices de m2
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Paso 1: Calcular las matrices S
      val s1 = restaMatriz(b12, b22)
      val s2 = sumMatriz(a11, a12)
      val s3 = sumMatriz(a21, a22)
      val s4 = restaMatriz(b21, b11)
      val s5 = sumMatriz(a11, a22)
      val s6 = sumMatriz(b11, b22)
      val s7 = restaMatriz(a12, a22)
      val s8 = sumMatriz(b21, b22)
      val s9 = restaMatriz(a11, a21)
      val s10 = sumMatriz(b11, b12)

      // Paso 2: Calcular las matrices P
      val p1 = multStrassen(a11, s1)
      val p2 = multStrassen(s2, b22)
      val p3 = multStrassen(s3, b11)
      val p4 = multStrassen(a22, s4)
      val p5 = multStrassen(s5, s6)
      val p6 = multStrassen(s7, s8)
      val p7 = multStrassen(s9, s10)

      // Paso 3: Calcular las submatrices de C
      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)

      // Combinar las submatrices en una sola
      Vector.tabulate(n, n)((i, j) => {
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      })
    }
  }

  // Ejercicio 1.3.3
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n <= 2) {
      multMatriz(m1, m2)
    } else {
      val l = n / 2

      // Submatrices de m1
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)

      // Submatrices de m2
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      // Paso 1: Calcular las matrices S en paralelo (dividido en grupos)
      // Grupo 1 (4 matrices)
      val (s1, s2, s3, s4) = parallel(
        restaMatriz(b12, b22),
        sumMatriz(a11, a12),
        sumMatriz(a21, a22),
        restaMatriz(b21, b11)
      )
      // Grupo 2 (4 matrices)
      val (s5, s6, s7, s8) = parallel(
        sumMatriz(a11, a22),
        sumMatriz(b11, b22),
        restaMatriz(a12, a22),
        sumMatriz(b21, b22)
      )
      // Grupo 3 (2 matrices)
      val (s9, s10) = parallel(
        restaMatriz(a11, a21),
        sumMatriz(b11, b12)
      )

      // Paso 2: Calcular las matrices P en paralelo (dividido en grupos)
      // Grupo 1 (4 matrices)
      val (p1, p2, p3, p4) = parallel(
        multStrassenPar(a11, s1),
        multStrassenPar(s2, b22),
        multStrassenPar(s3, b11),
        multStrassenPar(a22, s4)
      )
      // Grupo 2 (3 matrices) - Solución para el problema específico
      val (p5, p6, p7) = {
        val (p5p6, p7dummy) = parallel(
          parallel(
            multStrassenPar(s5, s6),
            multStrassenPar(s7, s8)
          ),
          multStrassenPar(s9, s10)
        )
        (p5p6._1, p5p6._2, p7dummy)
      }

      // Paso 3: Calcular las submatrices de C en paralelo (4 matrices)
      val (c11, c12, c21, c22) = parallel(
        sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6),
        sumMatriz(p1, p2),
        sumMatriz(p3, p4),
        restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)
      )

      // Combinar las submatrices en una sola
      Vector.tabulate(n, n)((i, j) => {
        if (i < l && j < l) c11(i)(j)
        else if (i < l && j >= l) c12(i)(j - l)
        else if (i >= l && j < l) c21(i - l)(j)
        else c22(i - l)(j - l)
      })
    }
  }
}