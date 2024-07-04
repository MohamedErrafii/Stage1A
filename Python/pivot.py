def pivot(tab):
	line, col = len(tab), len(tab[0]) ## on considere que tab a deux dimensions et est non vide
	i, j = 0, 0
	
	while j < col and i < line:
		## tri inverse de tab
		tab.sort(reverse=1)
		#print tab, 'tri',
		## recherche d'un pivot
		k = i+1
		max = i ## Pivot
		while k < line:
			if ( abs( tab[k][j] ) > abs( tab[max][j] ) ):
				max = k
			k = k+1
		#print max
		
		
		## l'algo en lui-meme
		if tab[max][j] != 0:
			## reduction du coef pivot a 1
			k = j
			piv = tab[max][j]
			while k < col:
				tab[max][k] = tab[max][k] * 1. / piv
				k = k+1
			#print tab
			
			## operations du pivot de Gauss
			k = 0
			while k < line:
				if k != max:
					t = 0
					ca = tab[k][j] ## coef de la colonne pivot j de la ligne k modifiee
					while t < col:
						tab[k][t] = tab[k][t] - ca * tab[max][t]
						#print tab[k][t], ca, tab[max][t]
						t = t+1
				k = k+1
			i = i+1
		j = j+1
		#print tab, 'fin'
	tab.sort(reverse=1) ## peu important et ajoute un peu de complexite
	return tab

def transpose(tab):
	col = len(tab[0])
	t = []
	li = 0
	while li < col:
		t.append([])
		li = li+1
	for l in tab:
		k = 0
		while k < col:
			t[k].append(l[k])
			k = k+1
	return t

def rang(tab):
	tab = transpose(tab)
	tab = pivot(tab)
	line, col = len(tab), len(tab[0])
	rg = 0
	for k in tab:
		if k != [0]*col:
			rg += 1
	return rg

if __name__ == "__main__":
	a = [[1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0], [1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0], [1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0], [1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1], [1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0], [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]] ## Test
	b = [[0, 0, 0], [-2, 1, -1], [2, 0, 2]] ## Test
	print(pivot(b)) ## Test
