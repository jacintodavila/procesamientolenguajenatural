% ecuaciones.pl
% un programa lógico para interpretar oraciones
% que tienen que ver con un niño resolviendo un
% sistema de ecuaciones con ayuda del computador
%

% Formas linguísticas: 
% 1 anota [esta ecuación]: x + y = a
% 2 anota [esta ecuación como] la uno: a*w + b*y = c
% 3 anota esa [ultima ecuación como] la dos
% 4 despeja la [variable] x [en la última ecuación]
% 5 sustituye la [variable] x en [la ecuación] uno por ..

% Más formas linguisticas:
% tomado de http://www.vitutor.com/ecuaciones/sistemas/reso.html

% 1 Se despeja una incógnita en una de las ecuaciones.
% 2 Se sustituye la expresión de esta incógnita en la otra ecuación, obteniendo un ecuación con una sola incógnita.
% 3 Se resuelve la ecuación.
% 4 El valor obtenido se sustituye en la ecuación en la que aparecía la incógnita despejada.
% 5 Los dos valores obtenidos constituyen la solución del sistema.

% por ejemplo:
% > anota 3*x-4*y = -6 como uno
% > anota 2*x+4*y = 16 como dos
% > despeja la x en dos
% < x = 8 - 2*y
% > anota esa como tres
% > sustituye la x en uno por su valor en tres
% < 3*(8-2*y)-4*y = -6
% > anota esa como cuatro
% > despeja la y en cuatro, paso a paso
% < 24-6*y-4*y = -6
% < -10*y = -30
% < y = 3
% > anota esa última como cinco
% > sustituye la y en dos por su valor en cinco
% < x = 8 - 2*3
% > anota esa como seis
% > reduce seis
% < x = 2
% > Solución?
% < x = 2, y = 3

