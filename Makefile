.PHONY: dochody_wydatki_1 dochody_wydatki_2 dochody_przyrost

dochody_wydatki_1:
	gnuplot dochody_wydatki_1.plg

dochody_wydatki_2:
	gnuplot dochody_wydatki_2.plg

dochody_przyrost:
	gnuplot dochody_przyrost.plg

budzet_stopa_bezrobocia:
	gnuplot budzet_stopa_bezrobocia.plg
