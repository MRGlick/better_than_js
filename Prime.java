public class Prime {
    public static void main(String[] args) {

        final int three = 3;
        final int one = 1;

        int num = 78953803;
        boolean isPrime = true;

        long startTime = System.nanoTime();

        int i = three - one;
        while (i < num / 2 && isPrime) {
            if (num % i == 0) 
                isPrime = false;
            
            i++;
        }
        

        long endTime = System.nanoTime();
        double durationSeconds = (endTime - startTime) / 1_000_000_000.0;

        System.out.println(num + " is " + (isPrime ? "prime." : "not prime."));
        System.out.println("Checked in " + durationSeconds + " seconds.");
    }
}