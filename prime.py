
import time


avg_time = 0

laps = 0
while laps < 100:
    start = time.process_time()
    num = 2000003
    is_prime = True

    i = 2

    while i < num / 2 and is_prime:
        if num % i == 0:
            is_prime = False
        i = i + 1
    end = time.process_time()

    avg_time += end - start
    if laps % 10 == 0: print("Time: ", end - start)
    laps += 1




print("Time: ", avg_time / 100)