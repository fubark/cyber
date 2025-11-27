var start = System.clock

class Fib {
  static get(n) {
    if (n < 2) return n
    return get(n - 1) + get(n - 2)
  }
}

var res = Fib.get(30)
System.print("time: %((System.clock - start) * 1000)")
System.print(res)