%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parte 1 - Sombrero Seleccionador
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%nuestras casas y para las que queremos que funcione el programa
casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).

%sangre(Mago, TipoDeSangre)
sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).
sangre(neville, pura).

mago(Mago):-
    sangre(Mago, _).

/* funciona pero queda más largo y lo tendría que cargar por cada mago
mago(harry).
mago(hermione).
mago(draco).*/


%laCasaBusca(Casa, Característica).
laCasaBusca(gryffindor, coraje).
laCasaBusca(slytherin, orgullo).
laCasaBusca(slytherin, inteligencia).
laCasaBusca(ravenclaw, responsabilidad).
laCasaBusca(ravenclaw, inteligencia).
laCasaBusca(hufflepuff, amistad).


%Punto 1
%Opcion antes de tener el predicado sangre
/*laCasalePermiteEntrar(Mago, slytherin):-
    elMagoEs(Mago, Caracteristica),
    Caracteristica \= impura. */ %después ver cómo se haría si tengo muchas características

laCasalePermiteEntrar(Mago, slytherin):-
    sangre(Mago, TipoDeSangre),
    TipoDeSangre \= impura.

laCasalePermiteEntrar(_, Casa):-
    casa(Casa), %para que no permita entrar cualquier cosa sino las casas de mi programa solamente
    Casa \= slytherin.


%Punto2
tieneCaracterApropiadoParaLaCasa(Mago, Casa):-
    casa(Casa),
    mago(Mago),
    forall(laCasaBusca(Casa, Caracteristica), caracteristicaMago(Mago, Caracteristica)).

tieneCaracterApropiadoParaLaCasa(hermione, gryffindor).

%Mis magos
%caracteristicaMago(Mago, Caracteristica).
%Harry
caracteristicaMago(harry, coraje).
caracteristicaMago(harry, amistad).
caracteristicaMago(harry, orgullo).
caracteristicaMago(harry, inteligencia).

%Draco
caracteristicaMago(draco, orgullo).
caracteristicaMago(draco, inteligencia).

%Hermione
caracteristicaMago(hermione, orgullo).
caracteristicaMago(hermione, inteligencia).
caracteristicaMago(hermione, responsabilidad).

caracteristicaMago(neville, amistad).
caracteristicaMago(neville, coraje).
caracteristicaMago(neville, responsabilidad).

caracteristicaMago(luna, amistad).
caracteristicaMago(luna, inteligencia).
caracteristicaMago(luna, responsabilidad).
%Punto 3
%Ojo con la inversibilidad del not


podriaQuedarSeleccionadoEn(Mago, Casa):-
    tieneCaracterApropiadoParaLaCasa(Mago, Casa),
    laCasalePermiteEntrar(Mago, Casa),
    not(odiariaIr(Mago, Casa)).

odiariaIr(harry, slytherin).
odiariaIr(draco, hufflepuff).
%Universo cerrado existe para Hermione ya que el predicado sí existe sólo que no hay ningún caso para ella

%Punto 4
%Caso Base
cadenaDeAmistades([_]).
    %magoAmistoso(Mago).

cadenaDeAmistades([]).
    
%Caso Recursivo
/*cadenaDeAmistades([Mago|DemasMagos]):-
    cumpleCondicionesMago(Mago),
    nth0(0, DemasMagos, MagoSiguiente),
    podriaQuedarSeleccionadoEn(MagoSiguiente, Casa),
    cadenaDeAmistades(DemasMagos).*/

cadenaDeAmistades([Mago|DemasMagos]):-
    magoAmistoso(Mago),
    nth0(0, DemasMagos, MagoSiguiente),
    podrianQuedarJuntos(Mago, MagoSiguiente),
    cadenaDeAmistades([MagoSiguiente|DemasMagos]).

magoAmistoso(Mago):-
    caracteristicaMago(Mago, amistad).


/*cumpleCondicionesMago(Mago):-
    caracteristicaMago(Mago, amistad).*/

podrianQuedarJuntos(Mago1, Mago2):-
    podriaQuedarSeleccionadoEn(Mago1, Casa),
    podriaQuedarSeleccionadoEn(Mago2, Casa).

%%-----------------------------------------------------------------
%cadenaDeAmistades([Mago, MagoSiguiente|DemasMagos]):-
