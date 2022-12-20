import java.util.ArrayList;
import java.util.Iterator;

class For {
    public static void main(String[] args) {
        ArrayList<Integer> list = new ArrayList<Integer>();
        for (int i = 0; i < 1000000; i += 1) {
            list.add(i);
        }

        long sum = 0;
        Iterator<Integer> iter = list.iterator();
        while (iter.hasNext()) {
            sum += iter.next();
        }
        System.out.println(sum);
    }
}