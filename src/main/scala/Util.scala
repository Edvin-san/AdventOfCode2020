object Util {
	def lowerBound[V <% Ordered[V]](first: BigInt, last: BigInt, value: V, valueOf: BigInt => V): BigInt = {
		// Adjusted from https://en.cppreference.com/w/cpp/algorithm/lower_bound
		var f = first
		var count: BigInt = last - first

		while (count > 0) {
			val step = count / 2
			val i = f + step
			if (valueOf(i) < value) {
				f = i + 1
				count -= step + 1
			} else count = step
		}

		f
	}

	def time[R](block: => R): R = {
    	val t0 = System.currentTimeMillis()
    	val result = block    // call-by-name
    	val t1 = System.currentTimeMillis()
    	println("Elapsed time: " + (t1 - t0) + "ms")
    	result
	}

	def posMod(a: BigInt, m: BigInt): BigInt = (a%m + m) % m

	def modularInverse(a: BigInt, m: BigInt): BigInt = {
		val (_, x, _) = gcdExtended(a, m)
		(x%m + m) % m
	}

	// Return gcd(a, b), x, y such that a*x + b*y = gcd(a, b)
	def gcdExtended(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
		if (a == 0) (b, 0, 1)
		else {
			val (gcd, x1, y1) = gcdExtended(b%a, a)
			val x = y1 - (b/a) * x1
			val y = x1

			(gcd, x, y)
		}
	}

	def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
	def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)
	def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

	object Vector {
		case class Pos(x: Int, y: Int) {
			def +(direction: Dir): Pos = Pos(x + direction.x, y + direction.y)
		}
		case class Dir(x: Int, y: Int) {
			def unary_- = Dir(-x, -y)
		}

		object Dir {
			val all8Dirs = List(
        Dir(1, 0),
        Dir(1, 1),
        Dir(0, 1),
        Dir(-1, 1),
        Dir(-1, 0),
        Dir(-1, -1),
        Dir(0, -1),
        Dir(1, -1))
		}

	}

	def normalizeNewLine(s: String) = s.replace("\r\n", "\n")
}