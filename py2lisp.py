Set = ["k", "q", "br", "bl", "nr", "nl", "cr", "cl", 
		"p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8"]
File = "./chess-set-7.snapshot.1"
Voc = ['a','b','c','d','e','f','g','h']
Ipos = ['e','d','f','c','g','b','h','a']

print("(setq BOARD '(",end='')
space = 22
for i in range(8):
	for j in range(8):
		print('("{:s}{:d}" ({:d} {:d} {:d}))'.format(Voc[j], 8-i, (-7 + j * 2) * space, (7 - i * 2) * space, 0),end=' ')

print("))")

#print position array
print("(setq white_pos '(", end='')
for i in range(16):
	if i < 8:
		print('("{:s}" "{:s}1")'.format(Set[i], Ipos[i]),end=' ')
	else:
		print('("{:s}" "{:s}2")'.format(Set[i], Voc[i-8]),end=' ')
print("))",)

#print position array
print("(setq black_pos '(", end='')
for i in range(16):
	if i < 8:
		print('("{:s}" "{:s}8")'.format(Set[i], Ipos[i]),end=' ')
	else:
		print('("{:s}" "{:s}7")'.format(Set[i], Voc[i-8]),end=' ')
print("))",)

#print ent array
print("(setq white_ents '(", end='')
for i in range(16):
	print('("{:s}" . "tmp")'.format(Set[i]),end=' ')
print("))",)

#print ent array
print("(setq black_ents '(", end='')
for i in range(16):
	print('("{:s}" . "tmp")'.format(Set[i]),end=' ')
print("))",)

