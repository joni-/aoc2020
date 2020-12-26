loop_size = 14775052
door_pk = 4707356
value = 1

for _ in range(loop_size):
  value = (value * door_pk) % 20201227

print(value)
