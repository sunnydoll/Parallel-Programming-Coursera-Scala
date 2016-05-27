package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
    val yLow = clamp(from, 0, dst.height)
    val yHigh = clamp(end, 0, dst.height)
    for(x <- 0 to dst.width - 1; y <- yLow to yHigh - 1) {
      dst.update(x, y, boxBlurKernel(src, x, y, radius))
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
    if(numTasks <= 1) {
      blur(src, dst, 0, src.height, radius)
    }
    else {
      //Number of tasks should be smaller than the height of dst pic
      val validNumTasks = clamp(numTasks, 1, dst.height)
      val tasksPerRound = dst.height / validNumTasks
      if(dst.height % validNumTasks == 0) {
        val range = 0 to dst.height by tasksPerRound
        val strips = range.zip(range.tail)
        for(strip <- strips) {
          task(blur(src, dst, strip._1, strip._2, radius)).join()
        }
      }
      else {
        val mod = dst.height % validNumTasks
        val extraStep = (0, mod + tasksPerRound)
        val range = (mod + tasksPerRound) to dst.height by tasksPerRound
        val strips = range.zip(range.tail)
        val extraStrips = extraStep +: strips
        for(strip <- extraStrips) {
          task(blur(src, dst, strip._1, strip._2, radius)).join()
        }
      }
    }
  }

}
