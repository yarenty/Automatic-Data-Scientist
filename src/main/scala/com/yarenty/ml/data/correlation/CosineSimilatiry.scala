package com.yarenty.ml.data.correlation

/**
  * Cosine similarity is a measure of similarity between two non-zero vectors of an inner product space
  * that measures the cosine of the angle between them. The cosine of 0° is 1, and it is less than 1
  * for any other angle in the interval [0,2π). It is thus a judgment of orientation and not magnitude:
  * two vectors with the same orientation have a cosine similarity of 1, two vectors at 90° have a similarity of 0,
  * and two vectors diametrically opposed have a similarity of -1, independent of their magnitude.
  *
  * Cosine similarity is particularly used in positive space, where the outcome is neatly bounded in [0,1].
  * The name derives from the term "direction cosine": in this case, note that unit vectors are maximally "similar"
  * if they're parallel and maximally "dissimilar" if they're orthogonal (perpendicular).
  * This is analogous to the cosine, which is unity (maximum value) when the segments subtend a zero angle
  * and zero (uncorrelated) when the segments are perpendicular.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Cosine_similarity">Cosine similarity</a>
  *
  *      (C)2018 by yarenty
  */

case object CosineSimilatiry extends Correlation {

  override def correlate(expected: Array[Double], observed: Array[Double]): Double = {
    checks(expected, observed)
    var dotProduct = 0.0
    var normA = 0.0
    var normB = 0.0
    for (i <- expected.indices) {
      dotProduct += expected(i) * observed(i)
      normA += Math.pow(expected(i), 2)
      normB += Math.pow(observed(i), 2)
    }
    dotProduct / (Math.sqrt(normA) * Math.sqrt(normB))
  }
}