
import time

start = time.process_time()

num = 78953803

is_prime = True

i = 2

while i < num / 2 and is_prime:
    if num % i == 0:
        is_prime = False
    i = i + 1

print(is_prime)
end = time.process_time()

print("Time: ", end - start)