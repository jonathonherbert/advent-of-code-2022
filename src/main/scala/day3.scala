package advent

object day3:
    def run() =
        val backpackLines = readFile("resources/day3.txt")
        val priorities = backpackLines
            .map { line => line.splitAt(line.length / 2) }
            .map { case (a, b) => a.intersect(b) }
            .map { getPriority }

        println(priorities.sum)

        val groupPriorities = backpackLines
            .grouped(3)
            .toList
            .map { case a :: b :: c :: tail => a.intersect(b).intersect(c) }
            .map { getPriority }

        println(groupPriorities.sum)

    def getPriority(item: String) = priorityMap(item.toCharArray()(0))

    val priorityMap =
        ('a' to 'z').zip(1 to 26).toMap ++
        ('A' to 'Z').zip(27 to 52).toMap


