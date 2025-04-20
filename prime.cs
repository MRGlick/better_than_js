using System;
using System.Diagnostics;

public class HelloWorld
{
    public static void Main(string[] args)
    {
        int number = 78953803;

        Stopwatch stopwatch = Stopwatch.StartNew();
        
        int i = 2;
        bool isPrime = true;
        while (i < number / 2 && isPrime) {
            if (number % i == 0) isPrime = false;
            i++;
        }
        
        stopwatch.Stop();

        Console.WriteLine($"Is {number} prime? {isPrime}");
        Console.WriteLine($"Time taken: {stopwatch.ElapsedMilliseconds} ms");
    }
}