import Matrices._
import Benchmark._

// Generar matrices de prueba
val m1 = matrizAlAzar(4, 2)
val m2 = matrizAlAzar(4, 2)

// Pruebas para multiplicación estándar
println("Multiplicación estándar secuencial:")
multMatriz(m1, m2)

println("\nMultiplicación estándar paralela:")
multMatrizPar(m1, m2)

// Pruebas para multiplicación recursiva
println("\nMultiplicación recursiva secuencial:")
multMatrizRec(m1, m2)

println("\nMultiplicación recursiva paralela:")
multMatrizRecPar(m1, m2)

// Pruebas para algoritmo de Strassen
println("\nMultiplicación Strassen secuencial:")
multStrassen(m1, m2)

println("\nMultiplicación Strassen paralela:")
multStrassenPar(m1, m2)

// Benchmarking para diferentes tamaños de matrices
println("\nBenchmarking para matrices de diferentes tamaños:")
for {
  i <- 1 to 7 // 2^7 = 128
  n = math.pow(2, i).toInt
  m1 = matrizAlAzar(n, 2)
  m2 = matrizAlAzar(n, 2)
} yield {
  println(s"\nTamaño: ${n}x${n}")
  println("Comparando multMatriz vs multMatrizPar:")
  println(compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))

  println("Comparando multMatrizRec vs multMatrizRecPar:")
  println(compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))

  println("Comparando multStrassen vs multStrassenPar:")
  println(compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))
}

// Benchmarking para producto punto
println("\nBenchmarking para producto punto:")
for {
  i <- 1 to 6
  n = math.pow(10, i).toInt
  v1 = vectorAlAzar(n, 2)
  v2 = vectorAlAzar(n, 2)
} yield {
  println(s"\nTamaño del vector: $n")
  println("Comparando prodPunto vs prodPuntoParD:")
  println(compararProdPunto(n))
}