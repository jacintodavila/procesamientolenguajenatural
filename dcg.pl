%
% Sample parser in DCG notation

s  --> np, vp.
np --> n. 
np --> d, n.
vp --> v, np. 
vp --> v, pro, np.
pro --> [a].
d  --> [el];[un];[la].
n  --> [perro];[gato];[romeo];[julieta];[yeli].
v  --> [siguio];[ama].

% To test:
%  ?- s([the,gardener,saw,a,policeman],[]).
%  yes
