% Prueba rdf-owl

carga(T) :- load_rdf('hotel.owl',T).

imprime_rdf([]). 
imprime_rdf([F|R]) :- write(F), nl, imprime_rdf(R). 

writelista([]).
writelista([H|R]) :- write(H), writelista(R). 