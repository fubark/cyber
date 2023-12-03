class Fib {
    static int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }
  
    public static void main(String args[]) {
        long start = System.currentTimeMillis();
        int res = fib(30);
        System.out.println("time: " + (System.currentTimeMillis() - start));
        System.out.println(res);
    }
}
