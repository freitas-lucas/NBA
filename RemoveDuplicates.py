# Run as python RemoveDuplicates.py > /path/to/NBA20mpg_OK.tab
arquivo = open("/path/to/NBA20mpg.tab")
count = 0
player_x = []
for line in arquivo:
	count += 1
	line_tab = line.rstrip().split("\t")
	if "TOT" in line_tab[5]:
		count = 1
		player_x = [(line_tab[2])]
		print(line.rstrip())
	else:
		if count == 2:
			if line_tab[2] in player_x:
				pass
		elif count == 3:
			if line_tab[2] in player_x:
				pass
		else:
			pass
			print(line.rstrip())