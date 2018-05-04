package com.yarenty.ml.data.transformations.smoothing

import org.junit.Assert.assertEquals
import org.junit.Test

class HoltWinterSmoothingTest {

  @Test
  def forecastNISTData(): Unit = {
    val y = List(362.0, 385.0, 432.0, 341.0, 382.0, 409.0, 498.0, 387.0, 473.0, 513.0, 582.0, 474.0, 544.0, 582.0, 681.0, 557.0, 628.0, 707.0, 773.0, 592.0, 627.0, 725.0, 854.0, 661.0)
    val period = 4
    val m = 4
    val alpha = 0.5
    val beta = 0.4
    val gamma = 0.6
    val debug = true
    val prediction = new HoltWinterSmoothing(alpha, beta, gamma, period, m).forecast(y).toList
    // These are the expected results
    val expected = List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 594.8043646513713, 357.12171044215734, 410.9203094983815, 444.67743912921156, 550.9296957593741, 421.1681718160631, 565.905732450577, 639.2910221068818, 688.8541669002238, 532.7122406111591, 620.5492369959037, 668.5662327429854, 773.5946568453546, 629.0602103529998, 717.0290609530134, 836.4643466657625, 884.1797655866865, 617.6686414831381, 599.1184450128665, 733.227872348479, 949.0708357438998, 748.6618488792186)

    assertEquals(expected, prediction)
  }
}