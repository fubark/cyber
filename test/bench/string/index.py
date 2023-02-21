import time

str = 'abcdefghijklmnopqrstuvwxyz123456' * 1000000 + 'waldo'

start = time.process_time()
for _ in range(100):
    idx = str.find('waldo')
print('idx: {} ms: {}'.format(idx, (time.process_time() - start) * 1000))