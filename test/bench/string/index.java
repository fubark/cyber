class Index {
    public static void main(String[] args) {
        String base = "abcdefghijklmnopqrstuvwxyz123456";
        StringBuilder sb = new StringBuilder(base.length() * 1000000 + "waldo".length());
        for (int i = 0; i < 1000000; i += 1) {
            sb.append(base);
        }
        sb.append("waldo");

        String str = sb.toString();

        long start = System.currentTimeMillis();
        int idx = 0;
        for (int i = 0; i < 100; i += 1) {
            idx = str.indexOf("waldo");
        }
        System.out.println(String.format("idx: %s ms: %s", idx, System.currentTimeMillis() - start));
    }
}