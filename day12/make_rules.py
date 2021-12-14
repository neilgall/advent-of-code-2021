import sys

caves = set()
edges = set()

for line in sys.stdin:
  start, end = line.strip().split('-')
  caves.add(start)
  caves.add(end)
  edges.add((start, end))

for cave in caves:
  size = 'big' if cave.upper() == cave else 'small'
  print(f'cave({cave.lower()}, {size}).')

for start, end in edges:
  print(f'edge({start.lower()}, {end.lower()}).')

