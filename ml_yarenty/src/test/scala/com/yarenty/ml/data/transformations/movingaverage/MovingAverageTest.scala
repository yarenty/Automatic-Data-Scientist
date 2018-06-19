package com.yarenty.ml.data.transformations.movingaverage

import com.yarenty.ml.visualisation.html.TwoPeriodicCharts
import com.yarenty.testhelpers.{TestHelper, TimeSeriesData}
import org.junit.Assert.assertArrayEquals
import org.junit.Test

class MovingAverageTest {


  @Test
  def testSimple(): Unit = {
    val test = new SimpleMovingAverage(2).average(TimeSeriesData.data)
    val expected = Array(501.0, 494.5, 496.0, 541.0, 561.5, 588.5, 680.0, 726.5, 655.0, 563.5, 511.0, 505.0, 524.0, 503.5, 508.5, 563.5, 585.5, 615.5, 699.0, 748.5, 680.0, 594.5, 542.0, 527.5, 556.5, 539.0, 527.5, 577.5, 610.5, 640.5, 728.5, 777.0, 694.5, 606.5, 567.5, 561.5, 585.0, 560.5, 554.0, 606.5, 631.5, 656.0, 741.0, 807.5, 737.5, 644.0, 597.0, 578.5, 595.5, 569.0, 564.5, 620.5, 660.5, 688.0, 773.0, 832.0, 745.0, 656.5, 622.5, 614.0, 633.5, 588.0, 576.0, 628.0, 668.5, 719.5, 818.5, 879.5, 793.0, 694.5, 630.5, 616.5, 650.5, 619.0, 605.0, 651.5, 682.5, 726.0, 839.5, 920.0, 823.5, 711.5, 655.0, 638.0, 660.5, 623.5, 601.5, 655.0, 707.5, 761.5, 873.5, 956.5, 864.0, 740.0, 677.5, 659.0, 681.5, 645.5, 637.5, 694.5, 734.5, 776.5, 880.5, 965.5, 887.5, 770.0, 701.0, 685.5, 709.5, 670.0, 652.5, 695.5, 741.5, 792.5, 916.0, 1017.5, 924.5, 801.0, 742.5, 727.5, 743.0, 689.0, 656.5, 709.5, 764.5, 826.5, 976.0, 1052.5, 925.0, 801.0, 741.0, 737.0, 770.0, 733.5, 712.0, 751.5, 791.0, 843.5, 969.5, 1060.5, 943.5, 817.0, 768.0, 758.0, 775.0, 739.5, 739.5, 787.5, 807.5, 862.5, 1006.5, 1100.5, 982.5, 852.0, 790.5, 765.0, 812.0, 771.5, 738.5, 794.5, 838.5, 884.0, 1022.5, 1117.0, 996.0, 864.0, 811.0, 819.5)
    assertArrayEquals(expected, test.toArray, 0.01)
  }

  @Test
  def testSimple8(): Unit = {
    val test = new SimpleMovingAverage(8).average(TimeSeriesData.data)
    val expected = Array(501.0, 494.5, 497.6666666666667, 517.75, 523.2, 541.3333333333334, 568.0, 587.625, 598.125, 604.875, 601.875, 595.875, 592.5, 574.625, 549.625, 533.875, 532.25, 546.875, 579.25, 607.75, 618.25, 630.5, 626.625, 621.5, 619.375, 602.375, 576.5, 559.625, 559.125, 571.125, 605.75, 633.5, 640.25, 650.375, 650.25, 646.375, 643.875, 626.375, 600.25, 583.75, 584.5, 596.125, 627.875, 657.625, 666.0, 678.5, 676.75, 671.5, 667.75, 649.75, 623.625, 603.0, 604.375, 614.0, 648.375, 677.375, 685.75, 699.25, 700.25, 697.625, 693.5, 672.625, 644.25, 621.625, 625.125, 637.375, 674.125, 703.75, 714.0, 730.375, 727.625, 727.5, 723.125, 702.375, 669.75, 645.375, 642.125, 653.25, 694.375, 729.125, 737.625, 752.25, 750.125, 748.875, 744.625, 723.25, 685.125, 657.0, 656.125, 669.5, 710.75, 749.125, 761.625, 778.25, 780.625, 779.25, 774.125, 750.25, 715.125, 684.75, 682.75, 693.875, 733.5, 770.5, 785.0, 801.625, 800.875, 799.375, 794.625, 772.75, 737.625, 705.25, 701.125, 710.875, 754.875, 793.875, 808.625, 826.625, 831.125, 834.625, 831.5, 808.75, 766.625, 731.75, 726.625, 738.125, 785.0, 819.375, 830.5, 847.375, 851.625, 854.25, 853.0, 831.0, 787.0, 755.75, 753.5, 766.375, 810.625, 847.25, 854.0, 868.125, 868.0, 869.75, 864.0, 843.75, 806.5, 775.5, 772.5, 786.875, 832.125, 872.5, 884.0, 900.625, 896.75, 895.0, 897.875, 872.25, 830.875, 795.75, 794.875, 803.75, 852.875, 891.75, 898.875, 914.875, 917.0, 921.125)
    assertArrayEquals(expected, test.toArray, 0.01)
  }


  @Test
  def testExpotential(): Unit = {
    val test = new ExponentialMovingAverage(0.5).average(TimeSeriesData.data)
    val expected = Array(501.0, 494.5, 499.25, 538.625, 541.8125, 586.90625, 657.453125, 691.2265625, 638.11328125, 590.056640625, 535.0283203125, 532.51416015625, 525.257080078125, 507.1285400390625, 517.5642700195312, 558.2821350097656, 565.1410675048828, 612.0705337524414, 675.5352668762207, 716.7676334381104, 659.3838167190552, 623.1919083595276, 560.0959541797638, 559.0479770898819, 557.023988544941, 540.0119942724705, 536.0059971362352, 579.5029985681176, 588.7514992840588, 635.8757496420294, 704.9378748210147, 742.4689374105074, 675.7344687052537, 639.8672343526268, 585.4336171763134, 588.7168085881567, 583.3584042940784, 563.1792021470392, 564.0896010735196, 606.0448005367598, 610.5224002683799, 653.76120013419, 719.380600067095, 774.6903000335475, 709.8451500167737, 676.4225750083868, 613.7112875041935, 609.8556437520967, 597.4278218760484, 575.2139109380241, 575.6069554690121, 620.303477734506, 638.151738867253, 679.0758694336265, 752.5379347168132, 795.2689673584066, 723.6344836792033, 692.3172418396016, 638.1586209198008, 641.0793104599004, 632.0396552299502, 592.5198276149752, 595.7599138074876, 626.3799569037437, 653.1899784518719, 706.0949892259359, 792.047494612968, 836.523747306484, 770.761873653242, 727.380936826621, 652.1904684133106, 654.0952342066553, 649.5476171033276, 621.2738085516638, 619.136904275832, 652.568452137916, 665.784226068958, 719.392113034479, 812.6960565172395, 873.3480282586197, 793.1740141293099, 751.5870070646549, 675.7935035323275, 675.8967517661638, 660.4483758830819, 631.224187941541, 616.1120939707705, 662.5560469853853, 684.2780234926927, 750.6390117463463, 840.3195058731732, 911.6597529365865, 828.3298764682933, 781.6649382341466, 700.8324691170733, 699.4162345585366, 682.2081172792683, 654.1040586396341, 651.552029319817, 695.7760146599085, 712.3880073299542, 768.1940036649771, 852.5970018324886, 923.2985009162443, 852.1492504581222, 805.5746252290611, 724.2873126145305, 726.1436563072652, 708.5718281536326, 678.7859140768163, 667.3929570384082, 701.1964785192041, 724.598239259602, 780.799119629801, 887.8995598149005, 963.9497799074502, 886.4748899537251, 839.7374449768625, 765.8687224884313, 764.4343612442157, 743.7171806221079, 699.358590311054, 678.6792951555269, 719.8396475777635, 743.9198237888818, 814.4599118944409, 940.7299559472204, 989.3649779736102, 900.6824889868051, 845.3412444934025, 768.6706222467012, 775.3353111233506, 766.6676555616752, 737.8338277808376, 726.4169138904188, 757.2084569452094, 775.6042284726047, 834.3021142363024, 940.1510571181511, 1007.5755285590756, 909.7877642795378, 865.893882139769, 789.9469410698845, 795.9734705349422, 771.9867352674711, 751.4933676337355, 749.7466838168677, 788.3733419084339, 788.1866709542169, 862.5933354771084, 969.2966677385542, 1047.1483338692772, 943.5741669346386, 903.7870834673192, 810.3935417336596, 811.6967708668299, 811.3483854334149, 771.6741927167075, 758.3370963583537, 801.1685481791769, 817.0842740895885, 876.0421370447942, 993.0210685223972, 1058.5105342611987, 963.2552671305993, 911.6276335652997, 836.8138167826498, 856.9069083913249)
    assertArrayEquals(expected, test.toArray, 0.01)
  }


  @Test
  def testExpotential2(): Unit = {
    val test = new ExponentialMovingAverage(1.2).average(TimeSeriesData.data)
    val expected = Array(501.0, 485.4, 507.72, 592.056, 535.5888, 651.28224, 743.343552, 721.3312896, 557.73374208, 538.853251584, 468.2293496832, 542.35413006336, 513.129173987328, 484.17416520253437, 536.7651669594932, 611.4469666081013, 564.1106066783798, 677.9778786643241, 751.2044242671352, 759.359115146573, 570.5281769706854, 590.2943646058629, 478.3411270788274, 573.9317745842345, 551.2136450831531, 517.3572709833694, 534.9285458033261, 640.6142908393348, 589.477141832133, 701.7045716335734, 788.4590856732854, 778.3081828653429, 575.1383634269314, 609.7723273146137, 515.2455345370772, 607.3508930925846, 572.1298213814831, 537.1740357237034, 570.5651928552593, 663.4869614289481, 605.3026077142104, 715.339478457158, 798.9321043085685, 836.2135791382863, 606.7572841723428, 650.2485431655315, 531.1502913668937, 620.9699417266213, 577.8060116546758, 548.0387976690648, 581.592240466187, 681.6815519067626, 650.8636896186475, 733.8272620762705, 844.4345475847459, 836.7130904830508, 615.0573819033898, 670.188523619322, 566.7622952761355, 659.4475409447729, 615.7104918110455, 540.4579016377909, 610.7084196724418, 666.2583160655116, 682.7483367868977, 774.2503326426205, 898.7499334714759, 877.4500133057048, 670.5099973388591, 686.6980005322282, 555.0603998935544, 676.1879200212891, 638.7624159957422, 583.8475168008515, 623.6304966398297, 698.473900672034, 675.1052198655932, 792.5789560268813, 928.6842087946237, 935.0631582410753, 668.587368351785, 718.282526329643, 576.3434947340714, 695.9313010531857, 634.8137397893629, 595.4372520421274, 602.1125495915745, 730.3774900816851, 701.124501983663, 840.1750996032674, 947.9649800793466, 990.0070039841307, 695.9985992031739, 742.8002801593652, 595.439943968127, 718.5120112063746, 654.297597758725, 620.340480448255, 654.731903910349, 757.0536192179302, 723.389276156414, 844.1221447687171, 955.5755710462565, 1001.6848857907487, 736.8630228418504, 763.42739543163, 618.914520913674, 749.8170958172652, 679.236580836547, 642.9526838326906, 658.6094632334618, 750.2781073533076, 747.5443785293385, 854.8911242941323, 1023.0217751411735, 1043.3956449717653, 762.1208710056469, 799.1758257988706, 670.5648348402259, 781.4870330319549, 711.3025933936091, 643.7394813212782, 660.8521037357443, 781.0295792528511, 765.3940841494298, 908.9211831701141, 1098.6157633659773, 1025.8768473268046, 769.2246305346391, 794.1550738930722, 671.5689852213856, 804.0862029557229, 748.7827594088554, 701.0434481182289, 717.7913103763542, 802.0417379247292, 792.3916524150542, 913.1216695169892, 1072.5756660966022, 1075.4848667806796, 759.3030266438641, 834.5393946712272, 689.8921210657545, 824.4215757868491, 732.7156848426301, 730.656863031474, 751.4686273937052, 842.1062745212589, 777.1787450957482, 968.9642509808504, 1097.40714980383, 1130.518570039234, 781.8962859921533, 880.4207428015693, 684.3158514396862, 838.7368297120628, 805.4526340575875, 717.3094731884825, 750.5381053623036, 862.6923789275393, 827.0615242144921, 956.5876951571015, 1140.6824609685798, 1120.663507806284, 817.4672984387432, 868.5065403122513, 740.6986919375497, 904.26026161249)
    assertArrayEquals(expected, test.toArray, 0.01)
  }


  @Test
  def testCumulative(): Unit = {
    val test = new CumulativeMovingAverage().average(TimeSeriesData.data)
    val expected = Array(501.0, 494.5, 497.6666666666667, 517.75, 523.2, 541.3333333333334, 568.0, 587.625, 587.3333333333334, 582.8000000000001, 573.4545454545455, 569.8333333333334, 565.8461538461539, 560.3571428571429, 558.2, 560.75, 561.4117647058823, 566.8333333333333, 575.8947368421052, 585.0, 585.8095238095239, 585.8636363636364, 582.0, 581.0, 579.96, 577.7692307692308, 576.0740740740741, 577.75, 578.448275862069, 581.9333333333333, 588.1290322580645, 594.125, 594.5757575757576, 594.8529411764706, 593.0285714285715, 593.0, 592.5945945945946, 591.2894736842105, 590.6153846153846, 592.0500000000001, 592.609756097561, 595.0952380952381, 599.5116279069767, 604.75, 605.6444444444444, 606.4565217391304, 605.2765957446808, 605.2916666666666, 604.8775510204081, 603.8399999999999, 603.2941176470588, 604.4807692307692, 605.4528301886792, 607.574074074074, 611.5454545454545, 615.5892857142857, 616.2280701754386, 617.0, 616.4406779661017, 616.9, 617.0, 615.9677419354839, 615.6984126984128, 616.3437500000001, 617.323076923077, 619.469696969697, 623.3283582089553, 627.1176470588235, 628.2463768115942, 629.0428571428572, 628.3098591549297, 628.6944444444446, 628.9178082191783, 628.4324324324326, 628.2800000000002, 629.0394736842107, 629.688311688312, 631.5256410256413, 635.0000000000003, 638.7375000000003, 639.6543209876546, 640.5121951219515, 640.0240963855425, 640.4523809523813, 640.5058823529415, 640.058139534884, 639.6091954022992, 640.3977272727276, 641.1348314606745, 643.0888888888892, 646.2417582417586, 649.9021739130438, 650.9247311827961, 651.8191489361706, 651.4842105263161, 651.9687500000003, 652.1030927835055, 651.8367346938779, 651.8080808080812, 652.6900000000004, 653.4455445544559, 655.117647058824, 657.8543689320393, 661.0865384615389, 662.2285714285719, 663.1415094339627, 662.9532710280379, 663.555555555556, 663.8073394495417, 663.6727272727277, 663.603603603604, 664.2410714285719, 664.9823008849562, 666.4912280701759, 669.347826086957, 672.5431034482763, 673.7094017094021, 674.7203389830512, 674.8655462184878, 675.6000000000004, 675.9917355371905, 675.819672131148, 675.6747967479679, 676.3629032258068, 677.0960000000003, 678.746031746032, 681.8031496062995, 684.5859375000002, 685.5736434108529, 686.3769230769233, 686.4198473282445, 687.1439393939396, 687.6766917293235, 687.8358208955225, 688.0370370370372, 688.7720588235295, 689.5401459854015, 691.0144927536232, 693.568345323741, 696.2928571428572, 697.1134751773051, 697.992957746479, 698.1048951048951, 698.8263888888889, 699.1655172413793, 699.3835616438356, 699.7142857142857, 700.5743243243243, 701.1610738255033, 702.7333333333332, 705.2052980132449, 707.9671052631578, 708.8300653594771, 709.8376623376623, 709.8838709677419, 710.5448717948718, 711.1847133757963, 711.3164556962026, 711.5283018867925, 712.35625, 713.1055900621119, 714.4753086419754, 716.9018404907977, 719.3841463414635, 720.2848484848486, 721.1265060240964, 721.37125748503, 722.2976190476192)
    assertArrayEquals(expected, test.toArray, 0.01)
  }

}

object MovingAverageTest {

  //scalastyle:off
  //just to create charts for documentation
  def main(args: Array[String]): Unit = {

    val t = new MovingAverageTest()
    val timestamp = TestHelper.dummyTimestamp(TimeSeriesData.data)

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/CMA"), timestamp,
      TimeSeriesData.data, "input data",
      new CumulativeMovingAverage().average(TimeSeriesData.data), "Cumulative averaging")
    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/CMAScalled"), timestamp,
      TimeSeriesData.data, "input data",
      new CumulativeMovingAverage().average(TimeSeriesData.data), "Cumulative averaging", true)

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(0.2).average(TimeSeriesData.data), " Exponential averaging (alpha=0.2)")
    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/ExpScalled"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(0.2).average(TimeSeriesData.data), " Exponential averaging (alpha=0.2)", true)

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp1"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(0.5).average(TimeSeriesData.data), " Exponential averaging (alpha=0.5)")
    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp1Scalled"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(0.5).average(TimeSeriesData.data), " Exponential averaging (alpha=0.5)", true)

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp2"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(1.2).average(TimeSeriesData.data), " Exponential averaging (alpha=1.2)")

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp3"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(1.6).average(TimeSeriesData.data), " Exponential averaging (alpha=1.6)")

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Exp4"), timestamp,
      TimeSeriesData.data, "input data",
      new ExponentialMovingAverage(2.4).average(TimeSeriesData.data), " Exponential averaging (alpha=2.4)")

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Sim"), timestamp,
      TimeSeriesData.data, "input data",
      new SimpleMovingAverage(2).average(TimeSeriesData.data), " Simple averaging (period=2)")

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Sim1"), timestamp,
      TimeSeriesData.data, "input data",
      new SimpleMovingAverage(8).average(TimeSeriesData.data), " Simple averaging (period=8)")
    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Sim1Scalled"), timestamp,
      TimeSeriesData.data, "input data",
      new SimpleMovingAverage(8).average(TimeSeriesData.data), " Simple averaging (period=8)", true)

    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Sim2"), timestamp,
      TimeSeriesData.data, "input data",
      new SimpleMovingAverage(24).average(TimeSeriesData.data), " Simple averaging (period=24)")
    TwoPeriodicCharts.plot(TestHelper.generateFileName("movingaverages/Sim2Scalled"), timestamp,
      TimeSeriesData.data, "input data",
      new SimpleMovingAverage(24).average(TimeSeriesData.data), " Simple averaging (period=24)", true)
  }
 //scalastyle:on
}
