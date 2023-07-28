import util.Pixel
import util.Util
import util.Util.getNeighbors
import util.Util.toGrayScale

import scala.annotation.tailrec



// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val lines = image.mkString.split("\n").toList
    val P3 = lines.head
    val Array(latime, lungime) = lines.tail.head.mkString.split("\\s").map(_.toInt)
    val max = lines.tail.tail.head

    def make_pixel_list(n: Int, list: List[String]): List[Pixel] = {
      @tailrec
      def pixel_list_aux(n: Int, list2: List[String], acc: List[Pixel]): List[Pixel] = {
        if (n == 0) {
          acc
        } else {
          val temp = Pixel(list2.head.mkString.split(" ").toList.head.toInt,
            list2.head.mkString.split(" ").toList.tail.head.toInt,
            list2.head.mkString.split(" ").toList.tail.tail.head.toInt)
          pixel_list_aux(n - 1, list2.tail, temp :: acc)
        }
      }

      pixel_list_aux(n, list, List.empty[Pixel]);
    }

    val idk1 = make_pixel_list(lines.tail.tail.tail.length, lines.tail.tail.tail);
    idk1.reverse.grouped(latime).toList

  }

  def toStringPPM(image: Image): List[Char] = {
    (List("P3", s"${image.head.length} ${image.length}", "255").mkString("\n")
      ++"\n"
      ++image.flatMap(row => row.map(p => s"${p.red.toInt} ${p.green.toInt} ${p.blue.toInt}\n"))
      .mkString)
      .toList

  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2;
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    (image1.transpose ++ image2.transpose).transpose
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    val Degrees = degrees / 90

    @tailrec
    def rotate_aux(image: Image, degreez: Int): Image = {
      if (degreez == 0)
        image
      else {
        rotate_aux(image.map(_.reverse).transpose, degreez - 1)
      }
    }

    rotate_aux(image, Degrees);
  }

  //ex 4

  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def edgeDetection(image: Image, threshold: Double): Image = {
    //pasul 1
    val gray: GrayscaleImage = image.map(_.map(toGrayScale(_: Pixel)))
    //pasul 2
    val gray_after_first: GrayscaleImage = applyConvolution(gray, gaussianBlurKernel)
    //pasul 3
    val Mx: GrayscaleImage = applyConvolution(gray_after_first, Gx)
    val My: GrayscaleImage = applyConvolution(gray_after_first, Gy)
    //pasul 4
    val almost: GrayscaleImage = Mx.zip(My).map(t => t._1.zip(t._2)).map(_.map(t => t._1.abs + t._2.abs))
    //pasul 5
    almost.map(_.map(t => if (t <= threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))
  }


  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    getNeighbors(image, (kernel.size - 1) / 2)
      .map(_.map(t => t.zip(kernel)
        .map(t => t._1.zip(t._2))
        .flatMap(_.map(t => t._1 * t._2)).sum))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def crate_matrix(x: Int, procent: Int): List[List[Int]] = {
      @tailrec
      def create_matrix_aux(n: Int, x: Int, procent: Int, acc: List[Int]): List[Int] = {
        if (n >= x * x)
          acc
        else {
          if ((n % x == n/x) || (n%x == 0))
            create_matrix_aux(n + 1, x, procent, 1 :: acc)
          else if (n % x > n / x)
            create_matrix_aux(n + 1, x, procent, -1 :: acc)
          else {
            val a = (acc.reverse(n - x ) + acc.reverse(n - x - 1))%procent
            create_matrix_aux(n + 1, x, procent, a :: (acc))
          }
        }
      }
      create_matrix_aux(0, x, procent, List.empty[Int]).reverse.grouped(x).toList
    }
    crate_matrix(size, m).map(_.map(funct(_)))
  }

}
