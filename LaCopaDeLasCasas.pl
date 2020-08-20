%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parte 2 - La copa de las casas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Base de conocimiento
accionMago(harry, andarFueraDeCama).
accionMago(hermione, irA(tercerPiso)).
accionMago(hermione, irA(seccionRestringidaBiblioteca)).
accionMago(harry, irA(bosque)).
accionMago(harry, irA(tercerPiso)).
accionMago(draco, irA(lasMazmorras)).
%buenaAccion(Accion, Puntaje).
accionMago(ron, buenaAccion(ganarPartidaAjedrez, 50)).
accionMago(hermione, buenaAccion(salvarAmigos, 50)).
accionMago(harry, buenaAccion(ganarleAVoldemort, 60)).

%Casas
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

%puntajePorAccion(Accion, Puntaje):-

puntajePorAccion(fueraDeCama, -50).
puntajePorAccion(irA(Lugar), Puntaje):-
    lugarProhibido(Lugar, Puntaje).

lugarProhibido(bosque, -50).
lugarProhibido(seccionRestringidaBiblioteca, -10).
lugarProhibido(tercerPiso, -75).

%esMalaAccion(Accion, Puntaje)
%AccionMago(Mago, Accion).
esMalaAccion(Accion, Puntaje):-
    puntajePorAccion(Accion, Puntaje),
    Puntaje < 0.

%Punto 1
esBuenAlumno(Mago):-
    esDe(Mago,_),
    forall(accionMago(Mago, Accion), not(esMalaAccion(Accion,_))).

/*accionRecurrente(Accion):-
    findall(Mago, accionMago(Mago, Accion), Magos),
    length(Magos, CantidadMagos),
    CantidadMagos > 1.*/

%%No es necesario usar listas
accionRecurrente(Accion):-
    accionMago(Mago, Accion),
    accionMago(OtroMago, Accion),
    Mago \= OtroMago.

puntajeTotalCasa(Casa, PuntajeTotal):-
    esDe(_, Casa),
    findall(Puntos, 
        (esDe(Mago, Casa),accionMago(Mago, Accion), puntajePorAccion(Accion, Puntos)),
    ListaDePuntos),
    sum_list(ListaPuntos, PuntajeTotal).
    
%puntajeTotalCasa(Casa, PuntajeTotal).

%Punto 3
/*
casaGanadora([Casa1, Casa2|Casas]):-
    nth0(0, Casas, CasaSiguiente),
    mayorPuntaje(Casa1, Casa2),
    casaGanadora([Casa1, CasaSiguiente|Casas]).

casaGanadora([Casa1, Casa2|Casas]):-
    nth0(0, Casas, CasaSiguiente),
    mayorPuntaje(Casa2, Casa1),
    casaGanadora([Casa2, CasaSiguiente|Casas]).
    
mayorPuntaje(Casa1, Casa2):-
    puntajeTotalCasa(Casa1, Puntaje1),
    puntajeTotalCasa(Casa2, Puntaje2),
    Puntaje1 > Puntaje2.
*/