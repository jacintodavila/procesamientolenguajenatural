% tokenizadorutf.pl basado en 
% resumidor.pl
%
% modificado por ultima vez el 5 de Abril de 2012 por Jacinto Dávila para adaptarlo a UTF 
% Copyright (C) H. Yelitza Contreras <hyelitza@ula.ve> and Jacinto Dávila <jacinto@ula.ve>
%
%This program is free software; you can redistribute it
%and/or modify it under the terms of the GNU General Public License
%as published by the Free Software Foundation; either version 2 of
%the License, or any later version.
%
%This program is distributed in the hope that it will be
%useful, but WITHOUT ANY WARRANTY; without even the implied
%warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%See the GNU General Public License for more details.
%
%You should have received a copy of the GNU General Public
%License along with this program; if not, write to the Free
%Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%02111-1307 USA or see http://www.opensource.org/licenses/gpl-license.php
% 
% Autor: H. Yelitza Contreras <hyelitza@ula.ve> and Jacinto Dávila <jacinto@ula.ve>
% Adaptación: Jacinto Dávila Marilú Parra <mmarilu@ula.ve>
% Direccion: Universidad de Los Andes. Mérida, Venezuela. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tokenizer progresivo
% Nueva interfaz para el tokenizador.
%

leer_bloque(Bloque, Tokens) :-
    name(Bloque, Lista),
    leer_parrafo(Lista, _, Tokens, _, _). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Paso 1 : Leer, Tokenizer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% archivo entrada/salida, tokens
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Esto corresponde al primer paso para escribir resumenes.
%% Consiste en dividir el texto en fases del pensamiento: párrafos, 
%% oraciones y palabras. 

%% Este tokenizador genera lee la entrada estandar y por cada 
%% párrafo genera como salida dos listas de token, la primera 
%% con los tokens en lower-case y la segunda con los tokens 
%% originales del texto (incluye upper-case).
    
% leer_parrafo(-BloqueEntrada, +BloqueSalida, -Atomos,-AtomosUpper,-ProximoC)
% Lee una línea del texto, separándola en una lista de átomos.
% Atomos = párrafo lower-case, delimitado por el caracter especial 
% de fin de línea [10]. AtomosUpper = párrafo original del texto 
% (incluye upper-case). BloqueEntrada es una lista con todo los digitos ascci 
% de un bloque del texto original (ver tesis Marilu). BloqueSalida contiene la
% lista de esos digitos aun sin procesar. 

leer_parrafo(BE, BS, Atomos,AtomosUpper,ProximoC) :-
    leer_caracter(BE, BN, PrimerC, PrimerCUpper, PrimerT),
    leer_resto_p(BN, BS, PrimerC, PrimerCUpper, PrimerT, Atomos, AtomosUpper, ProximoC).
    
leer_resto_p(BE, BS, 46,46,especial,Parrafo, ParrafoUpper,ProximoC) :- 
    !,
    leer_caracter(BE, BN, Caracter, CaracterUpper, TipoC),
    leer_resto_p(BN, BS, Caracter,CaracterUpper,TipoC, Parrafo, ParrafoUpper, ProximoC).

leer_resto_p(BE, BS, 32,32,blanco,Parrafo,ParrafoUpper,ProximoC) :- 
    !,
    leer_caracter(BE, BN, Caracter,CaracterUpper,TipoC),
    leer_resto_p(BN, BS, Caracter,CaracterUpper,TipoC,Parrafo,ParrafoUpper,ProximoC).

leer_resto_p(B, B, Caracter,Caracter,fin,[],[],Caracter) :- !.  
        
%% tipo alfanumérico

leer_resto_p(BE, BS, PrimerC,PrimerCUpper,PrimerT,[Oracion|Atomos],[OracionUpper|AtomosUpper],ProximoCaracter) :- 
    leer_oracion(BE, BN, PrimerC,PrimerCUpper,PrimerT,Oracion,OracionUpper,ProximoC),
    tipo_caracter(ProximoC,ProximoT,PC),
    leer_resto_p(BN, BS, ProximoC,PC,ProximoT,Atomos,AtomosUpper,ProximoCaracter).

% leer_atomos(-Atomos,-AtomosUpper,-ProximoC)
% Lee una línea del texto, separándola en una lista de átomos lower-case 
% y upper-case respectivamente.

leer_atomos(BE, BS, Atomos, AtomosUpper, ProximoC) :-
    leer_caracter(BE, BN, PrimerC, PrimerCUpper, PrimerT),
    leer_oracion(BN, BS, PrimerC, PrimerCUpper, PrimerT, Atomos, AtomosUpper, ProximoC).
    
% leer_oracion(+PrimerC,+PrimerCUpper,+PrimerT,-Lista,-ListaUpper,-ProximoC)
% Dado el primer caracter lower y upper case, respectivamente, además 
% del tipo de caracter correspondiente retorna la lista de palabras de 
% la oración. La oración esta delimitada por cualquier caracter de fin, 
% en especial el punto [46].

leer_oracion(B, B, Caracter,Caracter,fin,[],[],Caracter) :- !.

leer_oracion(B, B, 46,46,especial,[],[],46) :- !.

leer_oracion(BE, BS, _,_,blanco,Atomos,AtomosUpper,ProximoC) :- 
    !,
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).

leer_oracion(BE, BS, PrimerC,PrimerCUpper,especial,[A|Atomos],[AUpper|AtomosUpper],ProximoC) :-
    !,
    name(A,[PrimerC]),
    name(AUpper,[PrimerCUpper]),
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).
        
%% tipo alfanumérico

leer_oracion(BE, BS, PrimerC,PrimerCUpper,PrimerT,[A|Atomos],[AUpper|AtomosUpper],ProximoCaracter) :- 
    palabra_completa(BE, BN, PrimerC,PrimerCUpper,PrimerT,ProximoC,ProximoT,A,AUpper),
    leer_oracion(BN, BS, ProximoC,ProximoC,ProximoT,Atomos,AtomosUpper,ProximoCaracter).

% leer_caracter(+Input, +RestInput, -Caracter,-Tipo)
% Lee un caracter de la lista de entrada y obtiene el tipo de caracter
% de la función tipo_caracter

leer_caracter([], [], -1, fin) :- !. 

leer_caracter([C|RestoC], RestoC, Caracter,Tipo) :- 
    % get0(C), 
    char_type_char(C, Tipo, Caracter).
    % tipo_caracter(C,Tipo,Caracter).

% leer_caracter(+Input, +RestInput,-Caracter,-CaracterUpper,-Tipo)
% Lee un caracter de la entrada , devuelve el caracter en lower-case
% y el caracter original del texto (contiene upper-case), retorna también el 
% tipo de caracter con el predicado tipo_caracter

leer_caracter([], [], -1, -1, fin) :- !. 

leer_caracter([C|RestoC], RestoC, CaracterLower,C,Tipo) :-  
    % get0(C), 
    % tipo_caracter(C,Tipo,CaracterLower).
    char_type_char(C, Tipo, CaracterLower). 
    %, name(Ca, [C]), 
    %write('->'), 
    %write(Ca), write(' '), 
    %write(C), write(' '). 
    %write(Tipo), write(' '), 
    %write(CaracterLower).

% palabra_completa(+PrimerC,+PrimerCUpper,+PrimerT,-Lista,-ListaUpper)
% Dado el primer caracter y el primer tipo de caracter lee el resto de
% la palabra, colocándola en la lista lower-case y obtiene también la 
% lista en upper-case.

%% para token alfabéticos (primer caracter alfabético)

%palabra_completa(PrimerC,PrimerCUpper,alfa,ProximoC,ProximoT,Palabra,PalabraUpper) :-
palabra_completa(BE, BS, PrimerC,PrimerCUpper,alfa,ProximoC,ProximoT,Palabra,StringUpper) :-
    !,
    leer_caracter(BE, BN, Caracter,CaracterUpper,TipoC),
    palabra_completa_alfa(BN, BS, Caracter,CaracterUpper,TipoC,Lista,ListaUpper,ProximoC,ProximoT),
    name(Palabra,[PrimerC|Lista]),
    name(PalabraUpper,[PrimerCUpper|ListaUpper]),
    string_to_atom(StringUpper,PalabraUpper).
    
%% para tokens numéricos (primer caracter numérico)

%palabra_completa(PrimerC,_,num,ProximoC,ProximoT,Palabra,Palabra) :-
palabra_completa(BE, BS, PrimerC,_,num,ProximoC,ProximoT,Palabra,StringUpper) :-
    !,
    leer_caracter(BE, BN, Caracter,_,TipoC),
    palabra_numerica_completa(BN, BS, Caracter,TipoC,Lista,ProximoC,ProximoT),
    append([PrimerC|Lista],[44],ListaP), 
    name(A,ListaP),
    atom_chars(A,L), 
    append(L2,[(',')],L), 
    concat_atom(L2,Palabra),
    string_to_atom(StringUpper,Palabra).

%% NOTA: el "name" de los número redondos por ejemplos 3.000 genera 
%% como resultado un atomo con el valor de 3. Debe hacerse un 
%% procesamiento adicional para tratar estos casos (concatenar una 
%% coma al final, realizar el name, dividir en tokens, extraer la 
%% última coma y concatenar para obtener la Palabra).

% palabra_completa_alfa(+PrimerC,+PrimerCUpper,+alfa,-Palabra,-PalabraUpper,-ProximoC,-ProximoT)
% Obtiene un token completo cuando el primer caracter del token es alfabético.
% Genera dos tokens uno en lower-case y el segundo segun el texto original 
% (incluye upper-case).

palabra_completa_alfa(BE, BS, PrimerC,PrimerCUpper,alfa,[PrimerC|Lista],[PrimerCUpper|ListaUpper],ProximoC,ProximoT) :-
    !,
    leer_caracter(BE, BN, Caracter,CaracterUpper,TipoC),
    palabra_completa_alfa(BN, BS, Caracter,CaracterUpper,TipoC,Lista,ListaUpper,ProximoC,ProximoT).

palabra_completa_alfa(BE, BS, PrimerC,PrimerCUpper,num,[PrimerC|Lista],[PrimerCUpper|ListaUpper],ProximoC,ProximoT) :-
    !,
    leer_caracter(BE, BN, Caracter,CaracterUpper,TipoC),
    palabra_completa_alfa(BN, BS, Caracter,CaracterUpper,TipoC,Lista,ListaUpper,ProximoC,ProximoT).

palabra_completa_alfa(B, B, PrimerC,_,PrimerT,[],[],PrimerC,PrimerT).

% palabra_numerica_completa(+PrimerC,+PrimerT,-AtomoNumerico,-ProximoC,-ProximoT)
% Obtiene un token completo (palabra) con caracteres numericos, cuando el primer 
% caracter no es alfabético. Comtempla los casos de números decimales, 
% porcentajes, años y último token de una oración. 

palabra_numerica_completa(BE, BS, PrimerC,PrimerT,[PrimerC|Lista],ProximoC,ProximoT) :-
    member(PrimerT,[num,alfa]),
    !,
    leer_caracter(BE, BN, Caracter,TipoC),
    palabra_numerica_completa(BN, BS, Caracter,TipoC,Lista,ProximoC,ProximoT).

palabra_numerica_completa(BE, BS, PrimerC,_,[PrimerC|Lista],ProximoC,ProximoT) :-
    member(PrimerC,[46,44]),
    leer_caracter(BE, BN, Caracter,TipoC),
    member(TipoC,[num]),
    palabra_numerica_completa(BN, BS, Caracter,TipoC,Lista,ProximoC,ProximoT).

palabra_numerica_completa(B, B, PrimerC,PrimerT,[],PrimerC,PrimerT).


% tipo_caracter(+Codigo,?Type,-NuevoCodigo)
% tipo_caracter(+Codigo,?Type,-NuevoCodigo)

% Dado un código ASCII, clasifica el caracter en "fin" (de linea/archivo
% /palabra), "alfa" (alfabético y numéricos), "especiales" al resto de 
% los caracteres y "blanco"

% Modificamos este segmento para impedir que el fin de linea marque el fin del parrafo. 
% tipo_caracter(10,fin,10) :- !. % fin de línea en DOS
% tipo_caracter(13,fin,13) :- !. % fin de línea en UNIX
tipo_caracter(10,blanco,10) :- !. % fin de línea en DOS
tipo_caracter(13,blanco,13) :- !. % fin de línea en UNIX
tipo_caracter(-1,fin,-1) :- !. % fin de archivo

%% blanco y otros caracteres de control

tipo_caracter(Codigo,blanco,Codigo) :- 
    Codigo =< 32,
    !.

%% dígitos numéricos    

tipo_caracter(Codigo,num,Codigo) :- 
    48 =< Codigo, Codigo =< 57,
    !.

%% letras lower-case, alfabéticos

tipo_caracter(Codigo,alfa,Codigo) :- 
    97 =< Codigo, Codigo =< 122,
    !.

%% letras upper-case, alfabéticos

tipo_caracter(Codigo,alfa,NuevoCodigo) :- 
    65 =< Codigo, Codigo =< 90,
    !,
    %%NuevoCodigo is Codigo. % NO trasladar a lower-case
    NuevoCodigo is Codigo + 32. % trasladar a lower-case

%% vocales acentuadas y tilde en minúsculas
%% la lista representa respectivamente L = [á,é,í,ó,ú,ñ]

tipo_caracter(Codigo,alfa,Codigo) :- 
    127 =< Codigo, Codigo =< 254,
    %!,
    %member(Codigo,[225,233,237,243,250,241]), 
    !.

%% vocales acentuadas y tilde en mayúsculas
%% la lista representa respectivamente L = [Á,É,Í,Ó,Ú,Ñ]
%% L = [[193,225],[201,233],[205,237],[211,243],[218,250],[209,241]]
%% L = [[Á,á],[É,é],[Í,í],[Ó,ó],[Ú,ú],[Ñ,ñ]]

tipo_caracter(Codigo,alfa,NuevoCodigo) :- 
    member(Codigo,[161,169,173,177, 179,186, 193,195,201,205,211,218,209]), 
    !,
    %%NuevoCodigo is Codigo. % NO trasladar a lower-case
    NuevoCodigo is Codigo + 32. % trasladar a lower-case

%% caracteres especiales tratados como alfabéticos
%% la lista representa respectivamente L = [%,$,/,°]

tipo_caracter(Codigo,alfa,Codigo) :- 
    member(Codigo,[37,36,47,176]),
    !.

%% todos los demas son especiales 

tipo_caracter(Codigo,especial,Codigo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5 fin del tokenizadorutf.pl

%% modificado por Jacinto Dávila - 2012 Junio 21

%% tomado de 
% et.pl - M. Covington      2003 February 12

% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer

%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.
% modified to handle a code as input directly :JD
char_type_char(Code,Type,Tr) :-
   atom_chars(Char, [Code]), 
   char_table(Char,Type,Tr),
   !.

% Donald changed this from special to letter.
% Using downcase_atom saves having an enormous table
% and should handle all languages.
% letter -> alfa
char_type_char(Char,alfa,Char2) :-
   atom_chars(L2,[Char]),
   downcase_atom(L2,L3),
   atom_chars(L3,[Char2]).  

% End of line marks
% eol -> fin
char_table(end_of_file, fin, end_of_file).
char_table('\n',        fin, '\n'       ).

% Whitespace characters
% whitespace -> blanco
char_table(' ',     blanco,  ' ').     % blank
char_table('\t',    blanco,  ' ').     % tab
char_table('\r',    blanco,  ' ').     % return
char_table('''',    blanco, '''').     % apostrophe does not translate to blank

% Donald removed the letter characters and replaced them by special characters.
% There are too many Unicode letters to put them all in a table.
% The third parameter may be useless, but maybe someone will want to convert
% some of the special characters.
% There may be other Unicode characters that need to be added.
% special -> especial
char_table('~',     especial,    '~' ).
char_table('`',     especial,    '`' ).
char_table('!',     especial,    '!' ).
char_table('@',     especial,    '@' ).
char_table('#',     especial,    '#' ).
char_table('$',     especial,    '$' ).
char_table('\u0025',especial,    '\u0025' ). %
char_table('^',     especial,    '^' ).
char_table('&',     especial,    '&' ).
char_table('*',     especial,    '*' ).
char_table('(',     especial,    '(' ).
char_table(')',     especial,    ')' ).
char_table('_',     especial,    '_' ).
char_table('-',     especial,    '-' ).
char_table('+',     especial,    '+' ).
char_table('=',     especial,    '=' ).
char_table('{',     especial,    '{' ).
char_table('[',     especial,    '[' ).
char_table('}',     especial,    '}' ).
char_table(']',     especial,    ']' ).
char_table('|',     especial,    '|' ).
char_table('\\',    especial,    '\\' ).
char_table(':',     especial,    ':' ).
char_table(';',     especial,    ';' ).
char_table('"',     especial,    '"' ).
char_table('<',     especial,    '<' ).
char_table(',',     especial,    ',' ).
char_table('>',     especial,    '>' ).
char_table('.',     especial,    '.' ).
char_table('?',     especial,    '?' ).
char_table('/',     especial,    '/' ).

% Digits
% digit -> alfa ; Ojo, corregir
char_table('0',   alfa,     '0' ).
char_table('1',   alfa,     '1' ).
char_table('2',   alfa,     '2' ).
char_table('3',   alfa,     '3' ).
char_table('4',   alfa,     '4' ).
char_table('5',   alfa,     '5' ).
char_table('6',   alfa,     '6' ).
char_table('7',   alfa,     '7' ).
char_table('8',   alfa,     '8' ).
char_table('9',   alfa,     '9' ).

% Everything else is a letter character.