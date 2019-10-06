package dataset

import java.text.SimpleDateFormat
import java.util.TimeZone

import dataset.util.Commit.Commit

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {

  /** Q16 (5p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val p = input.filter(x => x.stats.isDefined)
      .map(x => x.stats.get.additions)
    p.sum / p.size
  }

  /** Q17 (8p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {

    def sumSecondElems(xs: List[(Int, Int)]): Int = {
      val list = xs.unzip match {
        case (l1, l2) => (l1.sum, l2.sum)
      }
      val result = list._2
      result
    }

    val formatter = new SimpleDateFormat("hh")
    formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
    val p = input.map(commit => (formatter.format(commit.commit.committer.date).toInt, commit.files))
      .map(x => (x._1, x._2.map(p => p.filename.get)))
      .map(x => (x._1, x._2.filter(p => p.endsWith(".js"))))
      .map(x => (x._1, x._2.size))
      .groupBy(x => x._1)
      .map(x => (x._1, sumSecondElems(x._2)))
      .toList
      .sortBy(-_._2)
    print(p)
    p(0)

  }

  /** Q18 (9p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = {
    val o = input.filter(commit => {
      val splitArray: Array[String] = commit.url.split("/")
      val param1 = splitArray(4)
      val slash = "/"
      val k = param1.concat(slash)
      val param2 = splitArray(5)
      val repoName = k.concat(param2)
      repoName == repo
    })
      .map(comm => comm.commit.author.name)
      .map(name => (name, 1))
      .groupBy(pair => pair._1)
      .map(list => (list._1, list._2.size))
      .toList
      .sortBy(_._2)
    print(o)
    o(0)
  }

  /** Q19 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    val p = input.filter(commit => commit.commit.committer.date.toString().contains("2019"))
      .map(commit => {
        val splitArray: Array[String] = commit.url.split("/")
        val param1 = splitArray(4)
        val slash = "/"
        val k = param1.concat(slash)
        val param2 = splitArray(5)
        val repoName = k.concat(param2)
        repoName
      })
      .map(repoName => (repoName, 1))
      .groupBy(repoName => repoName._1)
      .toList
      .map(repoList => (repoList._1, repoList._2.size))
      .toMap
    print(p)
    p
  }

  /** Q20 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   *
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    input.map(commit => commit.files)
      .map(ListFiles => ListFiles.map(file => file.filename))
      .flatMap(List => List)
      .map(some => some.get)
      .map(str => {
        val k: Array[String] = str.split("\\.")
        k(k.size - 1)
      })
      .map(str => (str, 1))
      .groupBy(str => str._1)
      .toList
      .map(x => (x._1, x._2.size))
      .sortBy(-_._2)
      .take(5)

  }
}
