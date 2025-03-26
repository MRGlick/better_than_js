
import time

start = time.time()

num = 78953803

is_prime = True

i = 2

while i < num / 2 and is_prime:
    if num % i == 0:
        is_prime = False
    i += 1

end = time.time()

print(is_prime)
print("Time: ", end - start)