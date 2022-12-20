class Fib {
  static get(n) {
    if (n < 2) return n
    return get(n - 1) + get(n - 2)
  }
}
System.print(Fib.get(30))
