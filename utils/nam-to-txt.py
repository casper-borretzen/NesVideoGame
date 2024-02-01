# $ python3 nam-to-txt.py > output.txt
with open(background.nam', 'rb') as f:
  i = 0
  while True:
    hexdata = f.read(1).hex()
    if len(hexdata) == 0:
      break
    if i % 32 == 0:
      print()
    print('$' + hexdata.upper(), end = ',’)
    i = i + 1
