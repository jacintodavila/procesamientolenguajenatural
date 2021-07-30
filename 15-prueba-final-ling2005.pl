% -------------------------------------------
% Gramática para el curso
% -------------------------------------------
% Este programa está basado premisas expresadas en un lenguaje de programación lógica (Prolog) 
% y presenta  unas reglas gramaticales y un diccionario particular reducidos.Las reglas
% gramaticales tienen la estructura de preguntas y codifican cada uno de los constituyentes de la
% oración. Prolog trata de probar dichas reglas en base a las premisas establecidas.La estrategia
% del intérprete se trata de constatar, en base a las condiciones lógicas de verdad, si las
% preguntas introducidas son verdaderas o falsas. Al hacer el análisis el interprete responde “yes
% or no” dependiendo de las premisas dadas: This means that if a statement cannot be shown to be
% true, it is assumed to be false.

% Copyright (C) This program is free software; you can redistribute it and/or modify it under the % terms of the GNU General Public License  as published by the Free Software Foundation; either
% version 2 of the License, or any later version. This program is distributed in the hope that 
% it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
% for more details. You should have received a copy of the GNU General Public License 
% along with this program. if not, write to the Free Software Foundation, Inc., 59 Temple 
% Place, Suite 330,  Boston, MA 02111-1307 USA or 
% see http://www.opensource.org/licenses/gpl-license.php. 
% Please visit http://www.swi-prolog.org for details.

% Authors: Hernán  Martínez  <hejmart@gmail.com> , Giuseppina Nicotra <gnicotral@gmail.com>  , 
% Pamela Palm <pampalm@gmail.com> , Lino Urdaneta <lino.urdaneta@gmail.com>. 
% Address: Universidad de Los Andes. Mérida, Venezuela.

% Corregido y modificado por JAcinto Dávila <jacinto@ula.ve>

% -------------------------------------------
% Reglas gramaticales
% -------------------------------------------

pregunta(S)  --> pro_interrog, v_atributivo, s_atributivo(S).
pregunta(S) --> adv_interrog, s_verbal, s_atributivo(S).
pregunta(S)    --> s_verbal, s_atributivo(S).

s_verbal --> v_modal, v_infinitivo.
s_verbal --> v_infinitivo.
s_verbal --> v_conjugado.

s_atributivo(S) --> especificador, atributo(S).
s_atributivo(S) --> atributo(S).

% -------------------------------------------
% Reglas de inserción léxica
% -------------------------------------------

pro_interrog --> [que];[cual];[cuales];[cuantos];[cuantas].
especificador --> [el];[la];[lo];[los];[las];[un];[una];[unos];[unas];[mi];[mis].
v_atributivo --> [es];[son];[significa].
adv_interrog --> [como];[cuando];[donde];[por, que].
v_modal --> [puedo];[puede];[podemos].
v_infinitivo --> [utilizar];[abrir];[encontrar];[minimizar];[ordenar];[cambiar];[configurar];[usar];[proteger];[establecer];[importar];[migrar];[actualizar];[tener];[preparar].
v_conjugado --> [guarda];[desinstalo];[existe];[uso];[cambio];[instalo];[creo];[está];[actualizo].
prep --> [a];[como];[con];[de];[desde];[durante];[en];[entre];[hacia];[mediante];[para];[por];[sin];[sobre].

atributo(respuesta_de(T),T,T2).
