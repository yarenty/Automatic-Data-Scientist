package com.yarenty.ml.data.transformations.smoothing

import org.junit.Assert.assertEquals
import org.junit.Test

class SingleExpSmoothingTest {


  @Test
  def forecastNISTData(): Unit = {
    val y = List(362.0, 385.0, 432.0, 341.0, 382.0, 409.0, 498.0, 387.0, 473.0, 513.0, 582.0, 474.0, 544.0, 582.0, 681.0, 557.0, 628.0, 707.0, 773.0, 592.0, 627.0, 725.0, 854.0, 661.0)
    val m = 4
    val alpha = 0.5
    val prediction = new SingleExpSmoothing(alpha,m).forecast(y).toList
    // These are the expected results
    val expected = List( 0.0, 362.0, 373.5, 402.75, 371.875, 376.9375, 392.96875, 445.484375, 416.2421875, 444.62109375, 478.810546875, 530.4052734375, 502.20263671875, 523.101318359375, 552.5506591796875, 616.7753295898438, 586.8876647949219, 607.4438323974609, 657.2219161987305, 715.1109580993652, 653.5554790496826, 640.2777395248413, 682.6388697624207, 768.3194348812103, 714.6597174406052, 687.8298587203026, 674.4149293601513, 667.7074646800756)
    assertEquals(expected, prediction)
  }
}
