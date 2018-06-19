package com.yarenty.math.correlation

import java.util.Random

import org.junit.{Assert, Test}

/**
  * Created by yarenty on 17/11/2016.
  */
// scalastyle:off
class ChiSquareCorrelationTest {
  val expected = Array(501.0, 488.0, 504.0, 578.0, 545.0, 632.0, 728.0, 725.0, 585.0, 542.0, 480.0, 530.0, 518.0, 489.0, 528.0, 599.0, 572.0, 659.0, 739.0, 758.0, 602.0, 587.0, 497.0, 558.0, 555.0, 523.0, 532.0, 623.0, 598.0, 683.0, 774.0, 780.0, 609.0, 604.0, 531.0, 592.0, 578.0, 543.0, 565.0, 648.0, 615.0, 697.0, 785.0, 830.0, 645.0, 643.0, 551.0, 606.0, 585.0, 553.0, 576.0, 665.0, 656.0, 720.0, 826.0, 838.0, 652.0, 661.0, 584.0, 644.0, 623.0, 553.0, 599.0, 657.0, 680.0, 759.0, 878.0, 881.0, 705.0, 684.0, 577.0, 656.0, 645.0, 593.0, 617.0, 686.0, 679.0, 773.0, 906.0, 934.0, 713.0, 710.0, 600.0, 676.0, 645.0, 602.0, 601.0, 709.0, 706.0, 817.0, 930.0, 983.0, 745.0, 735.0, 620.0, 698.0, 665.0, 626.0, 649.0, 740.0, 729.0, 824.0, 937.0, 994.0, 781.0, 759.0, 643.0, 728.0, 691.0, 649.0, 656.0, 735.0, 748.0, 837.0, 995.0, 1040.0, 809.0, 793.0, 692.0, 763.0, 723.0, 655.0, 658.0, 761.0, 768.0, 885.0, 1067.0, 1038.0, 812.0, 790.0, 692.0, 782.0, 758.0, 709.0, 715.0, 788.0, 794.0, 893.0, 1046.0, 1075.0, 812.0, 822.0, 714.0, 802.0, 748.0, 731.0, 748.0, 827.0, 788.0, 937.0, 1076.0, 1125.0, 840.0, 864.0, 717.0, 813.0, 811.0, 732.0, 745.0, 844.0, 833.0, 935.0, 1110.0, 1124.0, 868.0, 860.0, 762.0, 877.0)
  val correlation = ChiSquareCorrelation
  val rnd = new Random()

  @Test
  def testStandardizeMinMax() {
    val observed = expected.map(x => (x-expected.min) / (expected.max- expected.min))
    Assert.assertEquals(0.85,correlation.correlate(expected, observed),  1e-2) //it is the same !!!
    Assert.assertTrue(correlation.correlated(expected,observed))
  }

  @Test
  def testStandardizedWithSomeSmallishNoise() {
    val observed = expected.map(x => ((x-expected.min) / (expected.max- expected.min) + rnd.nextDouble()/20.0))
    Assert.assertTrue("Should be bigger than 0",correlation.correlate(expected, observed)>0.0) //with noise but still "the sameish"
    Assert.assertTrue(correlation.correlated(expected,observed))
  }

  @Test
  def testStandardizedWithNoise() {
    val observed = expected.map(x => ((x-expected.min) / (expected.max- expected.min) + rnd.nextDouble())/2)
    Assert.assertFalse(correlation.correlated(expected,observed))
  }

  @Test
  def testRandom() {
    val observed = rnd.doubles(expected.length-2).toArray ++ Array(0.0, 1.0)
    Assert.assertEquals(0.0, correlation.correlate(expected, observed), 1e-2)
    Assert.assertFalse(correlation.correlated(expected,observed))
  }


  @Test
  def testWithNoise() {
    val observed = expected.map(x => x + rnd.nextDouble()*40 - 20)
    Assert.assertTrue(correlation.correlate(expected, observed)>0.05) //with noise but still "the sameish"
    Assert.assertTrue(correlation.correlated(expected,observed))
  }





}

// scalastyle:on