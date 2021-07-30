% yelinizador.pl
% Autor: H. Yelitza Contreras CopyRight 2002. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Paso 1 : Leer, Tokennizer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% archivo entrada/salida, tokens
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Esto corresponde al primer paso para escribir resumenes.
% Consiste en dividir el texto en fases del pensamiento: párrafos, 
% oraciones y palabras. 
        
% leer_parrafo(-Atomos,-ProximoC)
% lee una línea del texto, separándola en una lista de átomos.
% átomo = párrafo, delimitado por el caracter especial de fin de 
% línea [10].

leer_parrafo(Atomos,ProximoC) :-
        leer_caracter(PrimerC, PrimerT),
        leer_resto_p(PrimerC, PrimerT, Atomos, ProximoC).
        
leer_resto_p(46,especial,Parrafo,ProximoC) :- 
        !,
        leer_caracter(Caracter,TipoC),
        leer_resto_p(Caracter,TipoC,Parrafo,ProximoC).

leer_resto_p(32,blanco,Parrafo,ProximoC) :- 
        !,
        leer_caracter(Caracter,TipoC),
        leer_resto_p(Caracter,TipoC,Parrafo,ProximoC).

leer_resto_p(10,fin,[],ProximoC) :- 
        !,
        leer_caracter(ProximoC,_).
        
leer_resto_p(Caracter,fin,[],Caracter) :- !.    
                
leer_resto_p(PrimerC,PrimerT,[Oracion|Atomos],ProximoCaracter) :- 
        % tipo alfa
        leer_oracion(PrimerC,PrimerT,Oracion,ProximoC),
        tipo_caracter(ProximoC,ProximoT,PC),
        leer_resto_p(PC,ProximoT,Atomos,ProximoCaracter).

% leer_atomos(-Atomos,-ProximoC)
% lee una línea del texto, separándola en una lista de átomos

leer_atomos(Atomos,ProximoC) :-
        leer_caracter(PrimerC, PrimerT),
        leer_oracion(PrimerC, PrimerT, Atomos,ProximoC).
        
% leer_oracion(+PrimerC,+PrimerT,-Lista,-ProximoC)
% Dado el primer caracter y tipo de caracter retorna la lista de 
% palabras de la oración. La oración esta delimitada por cualquier
% caracter de fin, en especial el punto [46].

leer_oracion(Caracter,fin,[],Caracter) :- !.

leer_oracion(46,especial,[],46) :- !.

leer_oracion(_,blanco,Atomos,ProximoC) :- 
        !,
        leer_atomos(Atomos,ProximoC).

leer_oracion(PrimerC,especial,[A|Atomos],ProximoC) :-
        !,
        name(A,[PrimerC]),
        leer_atomos(Atomos,ProximoC).
                
leer_oracion(PrimerC,PrimerT,[A|Atomos],ProximoCaracter) :- % tipo alfa
        palabra_completa(PrimerC,PrimerT,Palabra,ProximoC,ProximoT),
        name(A,Palabra),
        leer_oracion(ProximoC,ProximoT,Atomos,ProximoCaracter).

% leer_caracter(-Caracter,-Tipo)
% lee un caracter de la entrada estándar y obtiene el tipo de caracter
% de la funcion tipo_caracter

leer_caracter(Caracter,Tipo) :- 
        get0(C), %% lee un caracter de la entrada estándar
        tipo_caracter(C,Tipo,Caracter).

% palabra_completa(+PrimerC,+PrimerT,-Lista,-ProximoC,-ProximoT)
% dado el primer caracter y el primer tipo de caracter lee el resto de
% la palabra, colocándola en la lista.
        
palabra_completa(PrimerC,alfa,[PrimerC|Lista],ProximoC,ProximoT) :-
        !,
        leer_caracter(Caracter,TipoC),
        palabra_completa(Caracter,TipoC,Lista,ProximoC,ProximoT).

palabra_completa(PrimerC,num,[PrimerC|Lista],ProximoC,ProximoT) :-
        !,
        leer_caracter(Caracter,TipoC),
        palabra_numerica_completa(Caracter,TipoC,Lista,ProximoC,ProximoT).

%% cuando el primer caracter no es alfanumérico 

palabra_completa(PrimerC,PrimerT,[],PrimerC,PrimerT).

palabra_numerica_completa(PrimerC,PrimerT,[PrimerC|Lista],ProximoC,ProximoT) :-
        member(PrimerT,[num,alfa]),
        !,
        leer_caracter(Caracter,TipoC),
        palabra_numerica_completa(Caracter,TipoC,Lista,ProximoC,ProximoT).

palabra_numerica_completa(PrimerC,especial,[PrimerC|Lista],ProximoC,ProximoT) :-
        member(PrimerC,[46]),
        !,
        leer_caracter(Caracter,TipoC),
        palabra_numerica_completa(Caracter,TipoC,Lista,ProximoC,ProximoT).
        
palabra_numerica_completa(PrimerC,PrimerT,[],PrimerC,PrimerT).

% tipo_caracter(+Codigo,?Type,-NuevoCodigo)
% Dado un código ASCII, clasifica el caracter en "fin" (de linea/archivo
% /palabra), "alfa" (alfabético y numéricos), "especiales" al resto de 
% los caracteres y "blanco"

tipo_caracter(10,fin,10) :- !. % fin de línea en DOS
tipo_caracter(13,fin,13) :- !. % fin de línea en UNIX
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
        %% NuevoCodigo is Codigo. % NO trasladar a lower-case
        NuevoCodigo is Codigo + 32. % trasladar a lower-case

%% vocales acentuadas y tilde en minúsculas
%% la lista representa respectivamente L = [á,é,í,ó,ú,ñ]

tipo_caracter(Codigo,alfa,Codigo) :- 
        member(Codigo,[225,233,237,243,250,241]), 
        !.

%% vocales acentuadas y tilde en mayúsculas
%% la lista representa respectivamente L = [Á,É,Í,Ó,Ú,Ñ]
% L = [[193,225],[201,233],[205,237],[211,243],[218,250],[209,241]]
% L = [[Á,á],[É,é],[Í,í],[Ó,ó],[Ú,ú],[Ñ,ñ]]

tipo_caracter(Codigo,alfa,NuevoCodigo) :- 
        member(Codigo,[193,201,205,211,218,209]), 
        !,
        % NuevoCodigo is Codigo. % NO trasladar a lower-case
        NuevoCodigo is Codigo + 32. % trasladar a lower-case

%% caracteres especiales tratados como alfabéticos
%% la lista representa respectivamente L = [%,$,/,°]

tipo_caracter(Codigo,alfa,Codigo) :- 
        member(Codigo,[37,36,47,176]),
        !.

%% todos los especiales 

tipo_caracter(Codigo,especial,Codigo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Diccionarios %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% diccionario de expresiones (Williams y texto)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Conectores lógicos

expresion([(,)]) --> [(,)].
expresion([además]) --> [además].
expresion([como, resultado]) --> [como, resultado].
expresion([pero]) --> [pero].
expresion([no, obstante]) --> [no, obstante].
expresion([de, esta, forma]) --> [de, esta, forma].
expresion([sin, embargo]) --> [sin, embargo].
expresion([en, vista, de]) --> [en, vista, de].
expresion([también]) --> [también].
expresion([que]) --> [que].
expresion([y]) --> [y].

%% Expresiones de tiempo y espacio

expresion([más, tarde]) --> [más, tarde].
expresion([en, américa]) --> [en, américa].
expresion([en, europa]) --> [en, europa].
expresion([en, los, últimos, años]) --> [en, los, últimos, años].
expresion([en, el, siglo, X]) --> [en, el, siglo, X].
expresion([del, siglo, X]) --> [del, siglo, X].
expresion([siglo, X]) --> [siglo, X].
expresion([a, principios, de]) --> [a, principios, de].
expresion([a, principios, del]) --> [a, principios, del].
expresion([de, esa, época]) --> [de, esa, época].
expresion([en, X]) --> [en, X].
expresion([actualmente]) --> [actualmente].
expresion([desde, X]) --> [desde, X].
expresion([hasta, X]) --> [hasta, X].
expresion([luego]) --> [luego].
expresion([eventualmente]) --> [eventualmente].
expresion([anteriormente]) --> [anteriormente].
expresion([cuando]) --> [cuando].


%% Expresiones para evaluar


expresion([no]) --> [no].
expresion([entonces]) --> [entonces].
expresion([quizás]) --> [quizás].
expresion([afirmativamente]) --> [afirmativamente].
expresion([bajo, estas, circunstancias]) --> [bajo, estas, circunstancias].
expresion([a, partir]) --> [a, partir].
expresion([en, consecuencia]) --> [en, consecuencia].
expresion([como, consecuencia]) --> [como, consecuencia].
expresion([se, concluye]) --> [se, concluye].
expresion([con, base, en, el, marco, antes, descrito]) --> [con, base, en, el, marco, antes, descrito].
expresion([es, importante, señalar]) --> [es, importante, señalar].
expresion([adicionalmente]) --> [adicionalmente].
expresion([debido, a]) --> [debido, a].
expresion([por, tanto]) --> [por, tanto].
expresion([por, lo, tanto]) --> [por, lo, tanto].
expresion([por, ejemplo]) --> [por, ejemplo].
expresion([se, sabe]) --> [se, sabe].
expresion([el, presente]) --> [el, presente].
expresion([a, través, de]) --> [a, través, de].
expresion([a, través, del]) --> [a, través, del].
expresion([de, acuerdo, con]) --> [de, acuerdo, con].
expresion([por, otra, parte]) --> [por, otra, parte].
expresion([por, otro, lado]) --> [por, otro, lado].
expresion([por, su, parte]) --> [por, su, parte].
expresion([por, esta, razón]) --> [por, esta, razón].
expresion([por, último]) --> [por, último].
expresion([por]) --> [por].
expresion([más, específicamente]) --> [más, específicamente].
expresion([según]) --> [según].
expresion([aún]) --> [aún].
expresion([a, su, vez]) --> [a, su, vez].
expresion([teóricamente]) --> [teóricamente].
expresion([solo]) --> [solo].
expresion([cómo]) --> [cómo].
expresion([a]) --> [a].
expresion([de]) --> [de].
expresion([de, ellas]) --> [de, ellas].
expresion([como, objetivo]) --> [como, objetivo].
expresion([dentro, del, mencionado, sector]) --> [dentro, del, mencionado, sector].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% diccionario de verbos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% verbos obtenidos con el tact

verb(verbo([partir])) --> [partir].
verb(verbo([ser])) --> [ser].
verb(verbo([mejorar])) --> [mejorar].
verb(verbo([aumentar])) --> [aumentar].
verb(verbo([obtener])) --> [obtener].
verb(verbo([logrando])) --> [logrando].
verb(verbo([cumplir])) --> [cumplir].
verb(verbo([disminuir])) --> [disminuir].
verb(verbo([fortalecer])) --> [fortalecer].
verb(verbo([pesar])) --> [pesar].
verb(verbo([poder])) --> [poder].
verb(verbo([realizar])) --> [realizar].
verb(verbo([acopiar])) --> [acopiar].
verb(verbo([alcanzando])) --> [alcanzando].
verb(verbo([analizar])) --> [analizar].
verb(verbo([clasificar])) --> [clasificar].
verb(verbo([eliminar])) --> [eliminar].
verb(verbo([estimular])) --> [estimular].
verb(verbo([fijar])) --> [fijar].
verb(verbo([pagar])) --> [pagar].
verb(verbo([proteger])) --> [proteger].
verb(verbo([tomando])) --> [tomando].
verb(verbo([asegurar])) --> [asegurar].
verb(verbo([asumir])) --> [asumir].
verb(verbo([competir])) --> [competir].
verb(verbo([conocer])) --> [conocer].
verb(verbo([consolidar])) --> [consolidar].
verb(verbo([controlar])) --> [controlar].
verb(verbo([coordinar])) --> [coordinar].
verb(verbo([dependiendo])) --> [dependiendo].
verb(verbo([describir])) --> [describir].
verb(verbo([disminuyendo])) --> [disminuyendo].
verb(verbo([distribuir])) --> [distribuir].
verb(verbo([ejercer])) --> [ejercer].
verb(verbo([enfrentar])) --> [enfrentar].
verb(verbo([estabilizar])) --> [estabilizar].
verb(verbo([estimar])) --> [estimar].
verb(verbo([fermentar])) --> [fermentar].
verb(verbo([incursionar])) --> [incursionar].
verb(verbo([jatar])) --> [jatar].
verb(verbo([liderando])) --> [liderando].
verb(verbo([mantener])) --> [mantener].
verb(verbo([manteniendo])) --> [manteniendo].
verb(verbo([ofrecer])) --> [ofrecer].
verb(verbo([presionar])) --> [presionar].
verb(verbo([promover])) --> [promover].
verb(verbo([recuperar])) --> [recuperar].
verb(verbo([regular])) --> [regular].
verb(verbo([señalar])) --> [señalar].
verb(verbo([señalarse])) --> [señalarse].
verb(verbo([superar])) --> [superar].
verb(verbo([abastecer])) --> [abastecer].
verb(verbo([abastecerse])) --> [abastecerse].
verb(verbo([acondicionar])) --> [acondicionar].
verb(verbo([adaptar])) --> [adaptar].
verb(verbo([adecuar])) --> [adecuar].
verb(verbo([adquirir])) --> [adquirir].
verb(verbo([agrupando])) --> [agrupando].
verb(verbo([agruparse])) --> [agruparse].
verb(verbo([ahorrar])) --> [ahorrar].
verb(verbo([alcanzar])) --> [alcanzar].
verb(verbo([ampliar])) --> [ampliar].
verb(verbo([apoyar])) --> [apoyar].
verb(verbo([apreciarse])) --> [apreciarse].
verb(verbo([armonizar])) --> [armonizar].
verb(verbo([asegurando])) --> [asegurando].
verb(verbo([beneficiar])) --> [beneficiar].
verb(verbo([cerrando])) --> [cerrando].
verb(verbo([comprometiendo])) --> [comprometiendo].
verb(verbo([conformar])) --> [conformar].
verb(verbo([considerando])) --> [considerando].
verb(verbo([considerarse])) --> [considerarse].
verb(verbo([contrabando])) --> [contrabando].
verb(verbo([convertirse])) --> [convertirse].
verb(verbo([decir])) --> [decir].
verb(verbo([decirse])) --> [decirse].
verb(verbo([declinando])) --> [declinando].
verb(verbo([dedicarse])) --> [dedicarse].
verb(verbo([defender])) --> [defender].
verb(verbo([denominarse])) --> [denominarse].
verb(verbo([deprimirse])) --> [deprimirse].
verb(verbo([desarrollar])) --> [desarrollar].
verb(verbo([desarrollarse])) --> [desarrollarse].
verb(verbo([desplazar])) --> [desplazar].
verb(verbo([diseñar])) --> [diseñar].
verb(verbo([distinguir])) --> [distinguir].
verb(verbo([diversificar])) --> [diversificar].
verb(verbo([elevar])) --> [elevar].
verb(verbo([eliminando])) --> [eliminando].
verb(verbo([encargarse])) --> [encargarse].
verb(verbo([entregar])) --> [entregar].
verb(verbo([equilibrar])) --> [equilibrar].
verb(verbo([establecer])) --> [establecer].
verb(verbo([excluyendo])) --> [excluyendo].
verb(verbo([experimentar])) --> [experimentar].
verb(verbo([exportar])) --> [exportar].
verb(verbo([formar])) --> [formar].
verb(verbo([fortalecerse])) --> [fortalecerse].
verb(verbo([ganar])) --> [ganar].
verb(verbo([garantizar])) --> [garantizar].
verb(verbo([generando])) --> [generando].
verb(verbo([haber])) --> [haber].
verb(verbo([incentivar])) --> [incentivar].
verb(verbo([incluyendo])) --> [incluyendo].
verb(verbo([incorporando])) --> [incorporando].
verb(verbo([incorporar])) --> [incorporar].
verb(verbo([incursionando])) --> [incursionando].
verb(verbo([indicar])) --> [indicar].
verb(verbo([interviniendo])) --> [interviniendo].
verb(verbo([involucrarse])) --> [involucrarse].
verb(verbo([limitar])) --> [limitar].
verb(verbo([llegando])) --> [llegando].
verb(verbo([llegar])) --> [llegar].
verb(verbo([lograr])) --> [lograr].
verb(verbo([manejar])) --> [manejar].
verb(verbo([manifestando])) --> [manifestando].
verb(verbo([mantenerse])) --> [mantenerse].
verb(verbo([organizarse])) --> [organizarse].
verb(verbo([participar])) --> [participar].
verb(verbo([particular])) --> [particular].
verb(verbo([pasar])) --> [pasar].
verb(verbo([perder])) --> [perder].
verb(verbo([permitiendo])) --> [permitiendo].
verb(verbo([permitir])) --> [permitir].
verb(verbo([perseguir])) --> [perseguir].
verb(verbo([plantear])) --> [plantear].
verb(verbo([posicionar])) --> [posicionar].
verb(verbo([producir])) --> [producir].
verb(verbo([prohibiendo])) --> [prohibiendo].
verb(verbo([prohibir])) --> [prohibir].
verb(verbo([prolongar])) --> [prolongar].
verb(verbo([reactivar])) --> [reactivar].
verb(verbo([recibir])) --> [recibir].
verb(verbo([recobrar])) --> [recobrar].
verb(verbo([reconocer])) --> [reconocer].
verb(verbo([reconvertirse])) --> [reconvertirse].
verb(verbo([recuperarse])) --> [recuperarse].
verb(verbo([redefinir])) --> [redefinir].
verb(verbo([redituar])) --> [redituar].
verb(verbo([reemplazar])) --> [reemplazar].
verb(verbo([referir])) --> [referir].
verb(verbo([repercutiendo])) --> [repercutiendo].
verb(verbo([represar])) --> [represar].
verb(verbo([rescatar])) --> [rescatar].
verb(verbo([seguir])) --> [seguir].
verb(verbo([siendo])) --> [siendo].
verb(verbo([siguiendo])) --> [siguiendo].
verb(verbo([sostener])) --> [sostener].
verb(verbo([tomar])) --> [tomar].
verb(verbo([tomarse])) --> [tomarse].
verb(verbo([trabajar])) --> [trabajar].
verb(verbo([transmitir])) --> [transmitir].
verb(verbo([trasladar])) --> [trasladar].
verb(verbo([ubicar])) --> [ubicar].
verb(verbo([utilizando])) --> [utilizando].
verb(verbo([utilizar])) --> [utilizar].
verb(verbo([vendiendo])) --> [vendiendo].

%% verbos obtenidos manualmente

verb(verbo([reveló])) --> [reveló].
verb(verbo([contraer])) --> [contraer].
verb(verbo([ocurrido])) --> [ocurrido].
verb(verbo([comenzó])) --> [comenzó].
verb(verbo([identifica])) --> [identifica].
verb(verbo([origina])) --> [origina].
verb(verbo([aparece])) --> [aparece].
verb(verbo([crea])) --> [crea].
verb(verbo([formó])) --> [formó].
verb(verbo([provocó])) --> [provocó].
verb(verbo([realizó])) --> [realizó].
verb(verbo([concedió])) --> [concedió].
verb(verbo([permite])) --> [permite].
verb(verbo([consolidó])) --> [consolidó].
verb(verbo([transforma])) --> [transforma].
verb(verbo([ejecutó])) --> [ejecutó].
verb(verbo([extendió])) --> [extendió].
verb(verbo([consistía])) --> [consistía].
verb(verbo([desarrollaron])) --> [desarrollaron].
verb(verbo([diseñó])) --> [diseñó].
verb(verbo([promovía])) --> [promovía].
verb(verbo([orientó])) --> [orientó].
verb(verbo([realizaba])) --> [realizaba].
verb(verbo([realizaban])) --> [realizaban].
verb(verbo([efectuó])) --> [efectuó].
verb(verbo([transportaba])) --> [transportaba].
verb(verbo([establecía])) --> [establecía].
verb(verbo([efectuaban])) --> [efectuaban].
verb(verbo([ofrecía])) --> [ofrecía].
verb(verbo([destinaba])) --> [destinaba].
verb(verbo([controlaba])) --> [controlaba].
verb(verbo([mantuvo])) --> [mantuvo].
verb(verbo([logró])) --> [logró].
verb(verbo([localizaron])) --> [localizaron].
verb(verbo([consiste])) --> [consiste].
verb(verbo([rompió])) --> [rompió].
verb(verbo([fijaba])) --> [fijaba].
verb(verbo([lograban])) --> [lograban].
verb(verbo([obtenía])) --> [obtenía].
verb(verbo([superó])) --> [superó].
verb(verbo([trasladaron])) --> [trasladaron].
verb(verbo([ocasionó])) --> [ocasionó].
verb(verbo([manifestó])) --> [manifestó].
verb(verbo([sostuvo])) --> [sostuvo].
verb(verbo([dependían])) --> [dependían].
verb(verbo([correspondía])) --> [correspondía].
verb(verbo([correspondían])) --> [correspondían].
verb(verbo([dependía])) --> [dependía].
verb(verbo([existía])) --> [existía].
verb(verbo([existían])) --> [existían].
verb(verbo([desconocían])) --> [desconocían].
verb(verbo([dinamizaría])) --> [dinamizaría].
verb(verbo([varían])) --> [varían].
verb(verbo([varía])) --> [varía].
verb(verbo([manejaba])) --> [manejaba].
verb(verbo([permitía])) --> [permitía].
verb(verbo([comienza])) --> [comienza].
verb(verbo([funciona])) --> [funciona].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verbos binario - auxiliares 
% conjugacion de los verbos tener, deber, poder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb(verbo([deber])) --> [deber].
verb(verbo([debiendo])) --> [debiendo].
verb(verbo([debo])) --> [debo].
verb(verbo([debes])) --> [debes].
verb(verbo([debe])) --> [debe].
verb(verbo([debemos])) --> [debemos].
verb(verbo([debéis])) --> [debéis].
verb(verbo([deben])) --> [deben].
verb(verbo([debía])) --> [debía].
verb(verbo([debías])) --> [debías].
verb(verbo([debía])) --> [debía].
verb(verbo([debíamos])) --> [debíamos].
verb(verbo([debíais])) --> [debíais].
verb(verbo([debían])) --> [debían].
verb(verbo([debí])) --> [debí].
verb(verbo([debiste])) --> [debiste].
verb(verbo([debió])) --> [debió].
verb(verbo([debimos])) --> [debimos].
verb(verbo([debisteis])) --> [debisteis].
verb(verbo([debieron])) --> [debieron].
verb(verbo([deberé])) --> [deberé].
verb(verbo([deberás])) --> [deberás].
verb(verbo([deberá])) --> [deberá].
verb(verbo([deberemos])) --> [deberemos].
verb(verbo([deberéis])) --> [deberéis].
verb(verbo([deberán])) --> [deberán].
verb(verbo([debería])) --> [debería].
verb(verbo([deberías])) --> [deberías].
verb(verbo([debería])) --> [debería].
verb(verbo([deberíamos])) --> [deberíamos].
verb(verbo([deberíais])) --> [deberíais].
verb(verbo([deberían])) --> [deberían].
verb(verbo([deba])) --> [deba].
verb(verbo([debas])) --> [debas].
verb(verbo([debamos])) --> [debamos].
verb(verbo([debáis])) --> [debáis].
verb(verbo([deban])) --> [deban].
verb(verbo([debiera])) --> [debiera].
verb(verbo([debiese])) --> [debiese].
verb(verbo([debieras])) --> [debieras].
verb(verbo([debieses])) --> [debieses].
verb(verbo([debiera])) --> [debiera].
verb(verbo([debiese])) --> [debiese].
verb(verbo([debiéramos])) --> [debiéramos].
verb(verbo([debiésemos])) --> [debiésemos].
verb(verbo([debierais])) --> [debierais].
verb(verbo([debieseis])) --> [debieseis].
verb(verbo([debieran])) --> [debieran].
verb(verbo([debiesen])) --> [debiesen].
verb(verbo([debiere])) --> [debiere].
verb(verbo([debieres])) --> [debieres].
verb(verbo([debiere])) --> [debiere].
verb(verbo([debiéremos])) --> [debiéremos].
verb(verbo([debiereis])) --> [debiereis].
verb(verbo([debieren])) --> [debieren].
verb(verbo([tener])) --> [tener].
verb(verbo([teniendo])) --> [teniendo].
verb(verbo([tengo])) --> [tengo].
verb(verbo([tienes])) --> [tienes].
verb(verbo([tiene])) --> [tiene].
verb(verbo([tenemos])) --> [tenemos].
verb(verbo([tenéis])) --> [tenéis].
verb(verbo([tienen])) --> [tienen].
verb(verbo([tenía])) --> [tenía].
verb(verbo([tenías])) --> [tenías].
verb(verbo([tenía])) --> [tenía].
verb(verbo([teníamos])) --> [teníamos].
verb(verbo([teníais])) --> [teníais].
verb(verbo([tenían])) --> [tenían].
verb(verbo([tuve])) --> [tuve].
verb(verbo([tuviste])) --> [tuviste].
verb(verbo([tuvo])) --> [tuvo].
verb(verbo([tuvimos])) --> [tuvimos].
verb(verbo([tuvisteis])) --> [tuvisteis].
verb(verbo([tuvieron])) --> [tuvieron].
verb(verbo([tendré])) --> [tendré].
verb(verbo([tendrás])) --> [tendrás].
verb(verbo([tendrá])) --> [tendrá].
verb(verbo([tendremos])) --> [tendremos].
verb(verbo([tendréis])) --> [tendréis].
verb(verbo([tendrán])) --> [tendrán].
verb(verbo([tendría])) --> [tendría].
verb(verbo([tendrías])) --> [tendrías].
verb(verbo([tendría])) --> [tendría].
verb(verbo([tendríamos])) --> [tendríamos].
verb(verbo([tendríais])) --> [tendríais].
verb(verbo([tendrían])) --> [tendrían].
verb(verbo([tenga])) --> [tenga].
verb(verbo([tengas])) --> [tengas].
verb(verbo([tenga])) --> [tenga].
verb(verbo([tengamos])) --> [tengamos].
verb(verbo([tengáis])) --> [tengáis].
verb(verbo([tengan])) --> [tengan].
verb(verbo([tuviera])) --> [tuviera].
verb(verbo([tuviese])) --> [tuviese].
verb(verbo([tuvieras])) --> [tuvieras].
verb(verbo([tuvieses])) --> [tuvieses].
verb(verbo([tuviera])) --> [tuviera].
verb(verbo([tuviese])) --> [tuviese].
verb(verbo([tuviéramos])) --> [tuviéramos].
verb(verbo([tuviésemos])) --> [tuviésemos].
verb(verbo([tuvierais])) --> [tuvierais].
verb(verbo([tuvieseis])) --> [tuvieseis].
verb(verbo([tuvieran])) --> [tuvieran].
verb(verbo([tuviesen])) --> [tuviesen].
verb(verbo([tuviere])) --> [tuviere].
verb(verbo([tuvieres])) --> [tuvieres].
verb(verbo([tuviere])) --> [tuviere].
verb(verbo([tuviéremos])) --> [tuviéremos].
verb(verbo([tuviereis])) --> [tuviereis].
verb(verbo([tuvieren])) --> [tuvieren].
verb(verbo([poder])) --> [poder].
verb(verbo([pudiendo])) --> [pudiendo].
verb(verbo([puedo])) --> [puedo].
verb(verbo([puedes])) --> [puedes].
verb(verbo([puede])) --> [puede].
verb(verbo([podemos])) --> [podemos].
verb(verbo([podéis])) --> [podéis].
verb(verbo([pueden])) --> [pueden].
verb(verbo([podía])) --> [podía].
verb(verbo([podías])) --> [podías].
verb(verbo([podía])) --> [podía].
verb(verbo([podíamos])) --> [podíamos].
verb(verbo([podíais])) --> [podíais].
verb(verbo([podían])) --> [podían].
verb(verbo([pude])) --> [pude].
verb(verbo([pudiste])) --> [pudiste].
verb(verbo([pudo])) --> [pudo].
verb(verbo([pudimos])) --> [pudimos].
verb(verbo([pudisteis])) --> [pudisteis].
verb(verbo([pudieron])) --> [pudieron].
verb(verbo([podré])) --> [podré].
verb(verbo([podrás])) --> [podrás].
verb(verbo([podrá])) --> [podrá].
verb(verbo([podremos])) --> [podremos].
verb(verbo([podréis])) --> [podréis].
verb(verbo([podrán])) --> [podrán].
verb(verbo([podría])) --> [podría].
verb(verbo([podrías])) --> [podrías].
verb(verbo([podría])) --> [podría].
verb(verbo([podríamos])) --> [podríamos].
verb(verbo([podríais])) --> [podríais].
verb(verbo([podrían])) --> [podrían].
verb(verbo([pueda])) --> [pueda].
verb(verbo([puedas])) --> [puedas].
verb(verbo([pueda])) --> [pueda].
verb(verbo([podamos])) --> [podamos].
verb(verbo([podáis])) --> [podáis].
verb(verbo([puedan])) --> [puedan].
verb(verbo([pudiera])) --> [pudiera].
verb(verbo([pudiese])) --> [pudiese].
verb(verbo([pudieras])) --> [pudieras].
verb(verbo([pudieses])) --> [pudieses].
verb(verbo([pudiera])) --> [pudiera].
verb(verbo([pudiese])) --> [pudiese].
verb(verbo([pudiéramos])) --> [pudiéramos].
verb(verbo([pudiésemos])) --> [pudiésemos].
verb(verbo([pudierais])) --> [pudierais].
verb(verbo([pudieseis])) --> [pudieseis].
verb(verbo([pudieran])) --> [pudieran].
verb(verbo([pudiesen])) --> [pudiesen].
verb(verbo([pudiere])) --> [pudiere].
verb(verbo([pudieres])) --> [pudieres].
verb(verbo([pudiere])) --> [pudiere].
verb(verbo([pudiéremos])) --> [pudiéremos].
verb(verbo([pudiereis])) --> [pudiereis].
verb(verbo([pudieren])) --> [pudieren].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verbos participios regulares 
% llamados participios pasivos terminados en 
% los sufijos ido y ado
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb_part(verbo([asistido])) --> [asistido].
verb_part(verbo([fijado])) --> [fijado].
verb_part(verbo([habido])) --> [habido].
verb_part(verbo([sido])) --> [sido].
verb_part(verbo([estado])) --> [estado].
verb_part(verbo([debido])) --> [debido].
verb_part(verbo([tenido])) --> [tenido].
verb_part(verbo([podido])) --> [podido].
verb_part(verbo([acompañado])) --> [acompañado].
verb_part(verbo([adecuado])) --> [adecuado].
verb_part(verbo([adelantado])) --> [adelantado].
verb_part(verbo([afectado])) --> [afectado].
verb_part(verbo([agregado])) --> [agregado].
verb_part(verbo([agrupado])) --> [agrupado].
verb_part(verbo([alcanzado])) --> [alcanzado].
verb_part(verbo([aplicado])) --> [aplicado].
verb_part(verbo([arraigado])) --> [arraigado].
verb_part(verbo([aumentado])) --> [aumentado].
verb_part(verbo([aunado])) --> [aunado].
verb_part(verbo([cancelado])) --> [cancelado].
verb_part(verbo([caracterizado])) --> [caracterizado].
verb_part(verbo([citado])) --> [citado].
verb_part(verbo([clasificado])) --> [clasificado].
verb_part(verbo([comercializado])) --> [comercializado].
verb_part(verbo([conformado])) --> [conformado].
verb_part(verbo([considerado])) --> [considerado].
verb_part(verbo([consolidado])) --> [consolidado].
verb_part(verbo([contratado])) --> [contratado].
verb_part(verbo([creado])) --> [creado].
verb_part(verbo([desarrollado])) --> [desarrollado].
verb_part(verbo([designado])) --> [designado].
verb_part(verbo([desmonopolizado])) --> [desmonopolizado].
verb_part(verbo([desplazado])) --> [desplazado].
verb_part(verbo([destinado])) --> [destinado].
verb_part(verbo([elevado])) --> [elevado].
verb_part(verbo([estado])) --> [estado].
verb_part(verbo([exportado])) --> [exportado].
verb_part(verbo([fermentado])) --> [fermentado].
verb_part(verbo([fijado])) --> [fijado].
verb_part(verbo([fundamentado])) --> [fundamentado].
verb_part(verbo([garantizado])) --> [garantizado].
verb_part(verbo([consolidado])) --> [consolidado].
verb_part(verbo([importado])) --> [importado].
verb_part(verbo([incorporado])) --> [incorporado].
verb_part(verbo([incrementado])) --> [incrementado].
verb_part(verbo([liberalizado])) --> [liberalizado].
verb_part(verbo([ligado])) --> [ligado].
verb_part(verbo([llevado])) --> [llevado].
verb_part(verbo([logrado])) --> [logrado].
verb_part(verbo([mencionado])) --> [mencionado].
verb_part(verbo([monopolizado])) --> [monopolizado].
verb_part(verbo([negociado])) --> [negociado].
verb_part(verbo([ocasionado])) --> [ocasionado].
verb_part(verbo([oscilado])) --> [oscilado].
verb_part(verbo([otorgado])) --> [otorgado].
verb_part(verbo([pagado])) --> [pagado].
verb_part(verbo([pasado])) --> [pasado].
verb_part(verbo([posesionado])) --> [posesionado].
verb_part(verbo([privado])) --> [privado].
verb_part(verbo([publicado])) --> [publicado].
verb_part(verbo([quedado])) --> [quedado].
verb_part(verbo([realizado])) --> [realizado].
verb_part(verbo([represado])) --> [represado].
verb_part(verbo([representado])) --> [representado].
verb_part(verbo([resultado])) --> [resultado].
verb_part(verbo([revisado])) --> [revisado].
verb_part(verbo([rodeado])) --> [rodeado].
verb_part(verbo([señalado])) --> [señalado].
verb_part(verbo([subsidiado])) --> [subsidiado].
verb_part(verbo([sustentado])) --> [sustentado].
verb_part(verbo([usado])) --> [usado].
verb_part(verbo([utilizado])) --> [utilizado].
verb_part(verbo([variado])) --> [variado].
verb_part(verbo([asistido])) --> [asistido].
verb_part(verbo([conocido])) --> [conocido].
verb_part(verbo([consumido])) --> [consumido].
verb_part(verbo([cumplido])) --> [cumplido].
verb_part(verbo([debido])) --> [debido].
verb_part(verbo([dependido])) --> [dependido].
verb_part(verbo([desaparecido])) --> [desaparecido].
verb_part(verbo([dirigido])) --> [dirigido].
verb_part(verbo([disminuido])) --> [disminuido].
verb_part(verbo([ejercido])) --> [ejercido].
verb_part(verbo([exigido])) --> [exigido].
verb_part(verbo([invertido])) --> [invertido].
verb_part(verbo([mantenido])) --> [mantenido].
verb_part(verbo([obtenido])) --> [obtenido].
verb_part(verbo([permitido])) --> [permitido].
verb_part(verbo([promovido])) --> [promovido].
verb_part(verbo([reconocido])) --> [reconocido].
verb_part(verbo([requerido])) --> [requerido].
verb_part(verbo([sentido])) --> [sentido].
verb_part(verbo([sido])) --> [sido].
verb_part(verbo([sostenido])) --> [sostenido].
verb_part(verbo([tenido])) --> [tenido].
verb_part(verbo([vendido])) --> [vendido].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verbos auxiliares 
% conjugacion de los verbos haber, ser, estar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb_aux(verbo([haber])) --> [haber].
verb_aux(verbo([habiendo])) --> [habiendo].
verb_aux(verbo([he])) --> [he].
verb_aux(verbo([has])) --> [has].
verb_aux(verbo([ha])) --> [ha].
verb_aux(verbo([hay])) --> [hay].
verb_aux(verbo([hemos])) --> [hemos].
verb_aux(verbo([habemos])) --> [habemos].
verb_aux(verbo([habéis])) --> [habéis].
verb_aux(verbo([han])) --> [han].
verb_aux(verbo([había])) --> [había].
verb_aux(verbo([habías])) --> [habías].
verb_aux(verbo([había])) --> [había].
verb_aux(verbo([habíamos])) --> [habíamos].
verb_aux(verbo([habíais])) --> [habíais].
verb_aux(verbo([habían])) --> [habían].
verb_aux(verbo([hube])) --> [hube].
verb_aux(verbo([hubiste])) --> [hubiste].
verb_aux(verbo([hubo])) --> [hubo].
verb_aux(verbo([hubimos])) --> [hubimos].
verb_aux(verbo([hubisteis])) --> [hubisteis].
verb_aux(verbo([hubieron])) --> [hubieron].
verb_aux(verbo([habré])) --> [habré].
verb_aux(verbo([habrás])) --> [habrás].
verb_aux(verbo([habrá])) --> [habrá].
verb_aux(verbo([habremos])) --> [habremos].
verb_aux(verbo([habréis])) --> [habréis].
verb_aux(verbo([habrán])) --> [habrán].
verb_aux(verbo([habría])) --> [habría].
verb_aux(verbo([habrías])) --> [habrías].
verb_aux(verbo([habría])) --> [habría].
verb_aux(verbo([habríamos])) --> [habríamos].
verb_aux(verbo([habríais])) --> [habríais].
verb_aux(verbo([habrían])) --> [habrían].
verb_aux(verbo([haya])) --> [haya].
verb_aux(verbo([hayas])) --> [hayas].
verb_aux(verbo([hayamos])) --> [hayamos].
verb_aux(verbo([hayáis])) --> [hayáis].
verb_aux(verbo([hayan])) --> [hayan].
verb_aux(verbo([hubiera])) --> [hubiera].
verb_aux(verbo([hubiese])) --> [hubiese].
verb_aux(verbo([hubieras])) --> [hubieras].
verb_aux(verbo([hubieses])) --> [hubieses].
verb_aux(verbo([hubiera])) --> [hubiera].
verb_aux(verbo([hubiese])) --> [hubiese].
verb_aux(verbo([hubiéramos])) --> [hubiéramos].
verb_aux(verbo([hubiésemos])) --> [hubiésemos].
verb_aux(verbo([hubierais])) --> [hubierais].
verb_aux(verbo([hubieseis])) --> [hubieseis].
verb_aux(verbo([hubieran])) --> [hubieran].
verb_aux(verbo([hubiesen])) --> [hubiesen].
verb_aux(verbo([hubiere])) --> [hubiere].
verb_aux(verbo([hubieres])) --> [hubieres].
verb_aux(verbo([hubiere])) --> [hubiere].
verb_aux(verbo([hubiéremos])) --> [hubiéremos].
verb_aux(verbo([hubiereis])) --> [hubiereis].
verb_aux(verbo([hubieren])) --> [hubieren].
verb_aux(verbo([ser])) --> [ser].
verb_aux(verbo([siendo])) --> [siendo].
verb_aux(verbo([soy])) --> [soy].
verb_aux(verbo([eres])) --> [eres].
verb_aux(verbo([es])) --> [es].
verb_aux(verbo([somos])) --> [somos].
verb_aux(verbo([sois])) --> [sois].
verb_aux(verbo([son])) --> [son].
verb_aux(verbo([era])) --> [era].
verb_aux(verbo([eras])) --> [eras].
verb_aux(verbo([era])) --> [era].
verb_aux(verbo([éramos])) --> [éramos].
verb_aux(verbo([erais])) --> [erais].
verb_aux(verbo([eran])) --> [eran].
verb_aux(verbo([fui])) --> [fui].
verb_aux(verbo([fuiste])) --> [fuiste].
verb_aux(verbo([fue])) --> [fue].
verb_aux(verbo([fuimos])) --> [fuimos].
verb_aux(verbo([fuisteis])) --> [fuisteis].
verb_aux(verbo([fueron])) --> [fueron].
verb_aux(verbo([seré])) --> [seré].
verb_aux(verbo([serás])) --> [serás].
verb_aux(verbo([será])) --> [será].
verb_aux(verbo([seremos])) --> [seremos].
verb_aux(verbo([seréis])) --> [seréis].
verb_aux(verbo([serán])) --> [serán].
verb_aux(verbo([sería])) --> [sería].
verb_aux(verbo([serías])) --> [serías].
verb_aux(verbo([sería])) --> [sería].
verb_aux(verbo([seríamos])) --> [seríamos].
verb_aux(verbo([seríais])) --> [seríais].
verb_aux(verbo([serían])) --> [serían].
verb_aux(verbo([sea])) --> [sea].
verb_aux(verbo([seas])) --> [seas].
verb_aux(verbo([sea])) --> [sea].
verb_aux(verbo([seamos])) --> [seamos].
verb_aux(verbo([seáis])) --> [seáis].
verb_aux(verbo([sean])) --> [sean].
verb_aux(verbo([fuera])) --> [fuera].
verb_aux(verbo([fuese])) --> [fuese].
verb_aux(verbo([fueras])) --> [fueras].
verb_aux(verbo([fueses])) --> [fueses].
verb_aux(verbo([fuera])) --> [fuera].
verb_aux(verbo([fuese])) --> [fuese].
verb_aux(verbo([fuéramos])) --> [fuéramos].
verb_aux(verbo([fuésemos])) --> [fuésemos].
verb_aux(verbo([fuerais])) --> [fuerais].
verb_aux(verbo([fueseis])) --> [fueseis].
verb_aux(verbo([fueran])) --> [fueran].
verb_aux(verbo([fuesen])) --> [fuesen].
verb_aux(verbo([fuere])) --> [fuere].
verb_aux(verbo([fueres])) --> [fueres].
verb_aux(verbo([fuere])) --> [fuere].
verb_aux(verbo([fuéremos])) --> [fuéremos].
verb_aux(verbo([fuereis])) --> [fuereis].
verb_aux(verbo([fueren])) --> [fueren].
verb_aux(verbo([estar])) --> [estar].
verb_aux(verbo([estando])) --> [estando].
verb_aux(verbo([estoy])) --> [estoy].
verb_aux(verbo([estás])) --> [estás].
verb_aux(verbo([está])) --> [está].
verb_aux(verbo([estamos])) --> [estamos].
verb_aux(verbo([estáis])) --> [estáis].
verb_aux(verbo([están])) --> [están].
verb_aux(verbo([estaba])) --> [estaba].
verb_aux(verbo([estabas])) --> [estabas].
verb_aux(verbo([estaba])) --> [estaba].
verb_aux(verbo([estábamos])) --> [estábamos].
verb_aux(verbo([estabais])) --> [estabais].
verb_aux(verbo([estaban])) --> [estaban].
verb_aux(verbo([estuve])) --> [estuve].
verb_aux(verbo([estuviste])) --> [estuviste].
verb_aux(verbo([estuvo])) --> [estuvo].
verb_aux(verbo([estuvimos])) --> [estuvimos].
verb_aux(verbo([estuvisteis])) --> [estuvisteis].
verb_aux(verbo([estuvieron])) --> [estuvieron].
verb_aux(verbo([estaré])) --> [estaré].
verb_aux(verbo([estarás])) --> [estarás].
verb_aux(verbo([estará])) --> [estará].
verb_aux(verbo([estaremos])) --> [estaremos].
verb_aux(verbo([estaréis])) --> [estaréis].
verb_aux(verbo([estarán])) --> [estarán].
verb_aux(verbo([estaría])) --> [estaría].
verb_aux(verbo([estarías])) --> [estarías].
verb_aux(verbo([estaría])) --> [estaría].
verb_aux(verbo([estaríamos])) --> [estaríamos].
verb_aux(verbo([estaríais])) --> [estaríais].
verb_aux(verbo([estarían])) --> [estarían].
verb_aux(verbo([esté])) --> [esté].
verb_aux(verbo([estés])) --> [estés].
verb_aux(verbo([esté])) --> [esté].
verb_aux(verbo([estemos])) --> [estemos].
verb_aux(verbo([estéis])) --> [estéis].
verb_aux(verbo([estén])) --> [estén].
verb_aux(verbo([estuviera])) --> [estuviera].
verb_aux(verbo([estuviese])) --> [estuviese].
verb_aux(verbo([estuvieras])) --> [estuvieras].
verb_aux(verbo([estuvieses])) --> [estuvieses].
verb_aux(verbo([estuviera])) --> [estuviera].
verb_aux(verbo([estuviese])) --> [estuviese].
verb_aux(verbo([estuviéramos])) --> [estuviéramos].
verb_aux(verbo([estuviésemos])) --> [estuviésemos].
verb_aux(verbo([estuvierais])) --> [estuvierais].
verb_aux(verbo([estuvieseis])) --> [estuvieseis].
verb_aux(verbo([estuvieran])) --> [estuvieran].
verb_aux(verbo([estuviesen])) --> [estuviesen].
verb_aux(verbo([estuviere])) --> [estuviere].
verb_aux(verbo([estuvieres])) --> [estuvieres].
verb_aux(verbo([estuviere])) --> [estuviere].
verb_aux(verbo([estuviéremos])) --> [estuviéremos].
verb_aux(verbo([estuviereis])) --> [estuviereis].
verb_aux(verbo([estuvieren])) --> [estuvieren].

