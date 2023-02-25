% ist1107245, Patricia Gameiro

:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ["dados.pl"], ["keywords.pl"]. % ficheiros a importar


% -------------------------------------------------------------------------------------
% semestres(+Periodo, -Semestre)
% Auxiliar - estabelece uma relacao entre os periodos e os respetivos semestres
% Semestre eh o semestre que corresponde ao periodo Periodo
% -------------------------------------------------------------------------------------
semestres(Periodo, Semestre):- 
    ( Periodo = p1, Semestre = p1_2 ;
      Periodo = p2, Semestre = p1_2 ;
      Periodo = p3, Semestre = p3_4 ;
      Periodo = p4, Semestre = p3_4 ).

% -------------------------------------------------------------------------------------
% eventosSemSalas(-EventosSemSala)
% EventosSemSala eh uma lista ordenada e sem elementos repetidos de IDs de 
% eventos sem sala
% -------------------------------------------------------------------------------------
eventosSemSalas(EventosSemSala):-
    findall(ID, evento(ID, _, _, _, semSala), EventosSemSala).

% -------------------------------------------------------------------------------------
% eventosSemSalasDiaSemana(+DiaDaSemana, -EventosSemSala)
% EventosSemSala eh uma lista ordenada e sem elementos repetidos de IDs de 
% eventos sem sala que decorrem em DiaDaSemana 
% -------------------------------------------------------------------------------------
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala):-
    findall(ID,(
        evento(ID, _, _, _, semSala), 
        horario(ID, DiaDaSemana, _, _, _, _)
    ), EventosSemSala). 

% -------------------------------------------------------------------------------------
% semestresLista(+ListaPeriodos, -ListaPeriodoseSemestres)
% Auxiliar - utilizada no eventosSemSalasPeriodo 
% ListaPeriodoseSemestres eh uma lista ordenada e sem elementos repetidos dos 
% periodos de ListaPeriodos e dos semestres que lhes correspondem
% -------------------------------------------------------------------------------------
semestresLista([], []).         

semestresLista(ListaPeriodos, ListaPeriodoseSemestres):-
    setof(Periodo_Semestre, Periodo ^ (
        member(Periodo, ListaPeriodos),
        % se semestres for falso, Periodo_Semestre corresponde a Periodo
        (semestres(Periodo, Periodo_Semestre); Periodo_Semestre = Periodo)
    ), ListaPeriodoseSemestres).

% -------------------------------------------------------------------------------------
% eventosSemSalasPeriodo(+ListaPeriodos, -EventosSemSala)
% EventosSemSala eh uma lista ordenada e sem elementos repetidos de IDs de 
% eventos sem sala nos periodos de ListaPeriodos
% -------------------------------------------------------------------------------------
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala):-
    semestresLista(ListaPeriodos, ListaPeriodoseSemestres),
    findall(ID, (
        evento(ID, _, _, _, semSala), 
        horario(ID, _, _, _, _, Periodo), 
        member(Periodo, ListaPeriodoseSemestres)
    ), EventosSemSala).

% -------------------------------------------------------------------------------------
% organizaEventos(+ListaEventos, +Periodo, -EventosNoPeriodo)
% EventosNoPeriodo eh a lista ordenada e sem elementos repetidos de IDs dos 
% eventos de ListaEventos que ocorrem no periodo Periodo
% -------------------------------------------------------------------------------------
organizaEventos([], _, []).

organizaEventos([ID | IDs], Periodo, EventosNoPeriodo):-
    (horario(ID, _, _, _, _, Periodo);   
    (semestres(Periodo, Semestre),       % se o evento nao ocorrer em Periodo
    horario(ID, _, _, _, _, Semestre))), % verifica se o evento ocorre em Semestre
    organizaEventos(IDs, Periodo, EventosNoPeriodoAux),
    sort([ID | EventosNoPeriodoAux], EventosNoPeriodo).

% se o evento nao ocorrer nem em Periodo nem no Semestre correspondente
organizaEventos([_ | IDs], Periodo, EventosNoPeriodo):-  
    organizaEventos(IDs, Periodo, EventosNoPeriodo).

% -------------------------------------------------------------------------------------
% eventosMenoresQue(+Duracao, -ListaEventosMenoresQue)
% ListaEventosMenoresQue eh a lista ordenada e sem elementos repetidos dos
% identificadores dos eventos que tem duracao menor ou igual a Duracao
% -------------------------------------------------------------------------------------
eventosMenoresQue(Duracao, ListaEventosMenoresQue):-
    findall(ID, (
        horario(ID, _, _, _, DuracaoEvento , _), 
        DuracaoEvento =< Duracao
    ), ListaEventosMenoresQue).

% -------------------------------------------------------------------------------------
% eventosMenoresQueBool(+ID, +Duracao)
% eh verdade se o evento identificado por ID tiver duracao igual ou menor a Duracao
% -------------------------------------------------------------------------------------
eventosMenoresQueBool(ID, Duracao):-
    horario(ID, _, _, _, DuracaoEvento , _),
    DuracaoEvento =< Duracao.

% -------------------------------------------------------------------------------------
% procuraDisciplinas(+Curso, -ListaDisciplinas)
% ListaDisciplinas eh a lista ordenada alfabeticamente do nome das disciplinas
% do curso Curso.
% -------------------------------------------------------------------------------------
procuraDisciplinas(Curso, ListaDisciplinas):-
    findall(Disciplina, (
        turno(ID, Curso, _, _), 
        evento(ID, Disciplina, _, _, _)
    ), ListaDisciplinasDupliscadas),
    % ordena as disciplinas alfabeticamente e remove disciplinas duplicadas
    sort(ListaDisciplinasDupliscadas, ListaDisciplinas).

% -------------------------------------------------------------------------------------  
% organizaDisciplinas(+ListaDisciplinas, +Curso, -Semestres)
% Semestres eh uma lista com duas listas ordenadas alfabeticamente e sem
% elementos repetidos. A primeira lista contem as disciplinas de 
% ListaDisciplinas do curso Curso que ocorrem no primeiro semestre e a segunda 
% lista contem as que ocorrem no segundo semestre
% -------------------------------------------------------------------------------------  
organizaDisciplinas(ListaDisciplinas, Curso, Semestres):-
    organizaDisciplinas(ListaDisciplinas, Curso, Semestres, [], []).

% ------------------------------------------------------------------------------------- 
% organizaDisciplinas(+ListaDisciplinas, +Curso, -Semestres, +S1, +S2)
% Auxiliar - processo iterativo de organizaDisciplinas/3
% ------------------------------------------------------------------------------------- 
organizaDisciplinas([], _, Semestres, S1, S2):-
    sort(S1, S1Ordenado),
    sort(S2, S2Ordenado),
    append([S1Ordenado], [S2Ordenado], Semestres).

organizaDisciplinas([Disciplina | R], Curso, Semestres, S1, S2):-
    evento(ID, Disciplina, _, _, _),
    turno(ID, Curso , _, _),
    horario(ID, _, _, _, _, Periodo),   % obtem Periodo em que Disciplina ocorre
    ((member(Periodo, [p1, p2, p1_2]),
    organizaDisciplinas(R, Curso, Semestres, [Disciplina | S1], S2));
    member(Periodo, [p3, p4, p3_4]),
    organizaDisciplinas(R, Curso, Semestres, S1 , [Disciplina | S2]));
    % falha se nao existir no curso Curso uma disciplina de ListaDisciplinas
    !, fail.

% -------------------------------------------------------------------------------------
% horasCurso(+Periodo, +Curso, +Ano, -TotalHoras) 
% TotalHoras eh o numero de horas total dos eventos associados ao curso Curso,
% no ano Ano e periodo Periodo
% -------------------------------------------------------------------------------------
horasCurso(Periodo, Curso, Ano, TotalHoras):-    
    semestres(Periodo, Semestre),
    findall(ID, turno(ID, Curso, Ano, _), ListaIDs),
    sort(ListaIDs, ListaSemIDsDuplicados),
    findall(Duracao, ((
        % obtem duracao de eventos do periodo 
        horario(ID, _, _, _, Duracao, Periodo); 
        % obtem duracao de eventos do semestre 
        horario(ID, _, _, _, Duracao, Semestre)
    ), member(ID, ListaSemIDsDuplicados)), ListaDuracoes),
    sumlist(ListaDuracoes, TotalHoras), !.

% -------------------------------------------------------------------------------------
% evolucaoHorasCurso(+Curso, -Evolucao) 
% Evolucao eh uma lista de tuplos na forma (Ano, Periodo, NumHoras) ordenada por
% ano e periodo. NumHoras eh o total de horas associadas ao curso Curso, no ano Ano
% e periodo Periodo
% -------------------------------------------------------------------------------------
evolucaoHorasCurso(Curso, Evolucao):-
    findall((Ano, Periodo, TotalHoras), (
        member(Ano, [1, 2, 3]),
        member(Periodo, [p1, p2, p3, p4]), 
        horasCurso(Periodo, Curso, Ano, TotalHoras)
    ), Evolucao).

% -------------------------------------------------------------------------------------
% ocupaSlot(+HoraInicioDada, +HoraFimDada, +HoraInicioEvento, +HoraFimEvento, -Horas)
% Horas eh o numero de horas sobrepostas entre o evento que tem inicio em 
% HoraInicioEvento e fim em HoraFimEvento, e o slot que tem inicio em 
% HoraInicioDada e fim em HoraFimDada
% -------------------------------------------------------------------------------------
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
    HoraInicioDada < HoraFimEvento,     % verifica se existe sobreposicao
    HoraFimDada > HoraInicioEvento,
    Horas is min(HoraFimDada, HoraFimEvento) - max(HoraInicioDada, HoraInicioEvento).

% -------------------------------------------------------------------------------------
% numHorasOcupadas(+Periodo, +TipoSala, +DiaSemana, +HoraInicio, +HoraFim, -SomaHoras)
% SomaHoras eh o numero de horas ocupadas nas salas do tipo TipoSala, no intervalo 
% de tempo definido entre HoraInicio e HoraFim, no dia da semana DiaSemana, e no
% periodo Periodo
% -------------------------------------------------------------------------------------
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    salas(TipoSala, ListaSalas),
    semestres(Periodo, Semestre),
    findall([HoraInicioEvento, HoraFimEvento], (
        evento(ID, _, _, _, Sala), (
            % para eventos do periodo
            horario(ID , DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo); 
            % para eventos semestrais
            horario(ID , DiaSemana, HoraInicioEvento, HoraFimEvento, _, Semestre)
        ), member(Sala, ListaSalas)
    ),  ListaHoras),
    numHorasOcupadas(ListaHoras, HoraInicio, HoraFim, SomaHoras), !.

% -------------------------------------------------------------------------------------
% numHorasOcupadas(+ListaHoras, +HoraInicio, +HoraFim, -SomaHoras)
% Auxiliar - processo recursivo de numHorasOcupadas/6
% -------------------------------------------------------------------------------------
numHorasOcupadas([], _, _, 0).

numHorasOcupadas([[HoraInicioEv, HoraFimEv] | R], HoraInicio, HoraFim, SomaHoras):-
    ocupaSlot(HoraInicio, HoraFim, HoraInicioEv, HoraFimEv, Horas),
    numHorasOcupadas(R, HoraInicio, HoraFim, SomaAux),
    SomaHoras is SomaAux + Horas.

numHorasOcupadas([_ | R], HoraInicio, HoraFim, SomaHoras):-
    numHorasOcupadas(R, HoraInicio, HoraFim, SomaHoras).

% -------------------------------------------------------------------------------------
% ocupacaoMax(+TipoSala, +HoraInicio, +HoraFim, -Max) 
% Max eh o numero de horas possiveis de ser ocupadas por salas do tipo TipoSala, 
% no intervalo de tempo definido entre HoraInicio e HoraFim
% -------------------------------------------------------------------------------------
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):-
    salas(TipoSala, ListaSalas),
    length(ListaSalas, NumeroSalas),
    Max is (HoraFim - HoraInicio) * NumeroSalas.

% -------------------------------------------------------------------------------------
% percentagem(+SomaHoras, +Max, -Percentagem)
% Percentagem eh a divisao de SomaHoras por Max, multiplicada por 100
% -------------------------------------------------------------------------------------
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is (SomaHoras / Max) * 100.

% -------------------------------------------------------------------------------------
% ocupacaoCritica(+HoraInicio, +HoraFim, +Threshold, -Resultados) 
% Resultados eh uma lista ordenada de tuplos do tipo casosCriticos(DiaSemana, 
% TipoSala, Percentagem). DiaSemana, TipoSala e Percentagem representam, 
% respetivamente, um dia da semana, um tipo de sala e a sua percentagem 
% de ocupacao, no intervalo de tempo entre HoraInicio e HoraFim. A percentagem de 
% ocupacao relativa a esses elementos dever estar acima de um dado valor
% critico (Threshold).
% -------------------------------------------------------------------------------------
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(casosCriticos(DiaSemana, TipoSala, PercentagemInteira), (
        salas(TipoSala, Salas),
        member(Sala, Salas),
        % liga as variaveis ID, DiaSemana e Periodo
        horario(ID, DiaSemana, _, _, _, Periodo),
        evento(ID, _, _, _, Sala),
        % obtem Max
        ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
        % obtem SomaHoras 
        numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
        percentagem(SomaHoras, Max, Percentagem), 
        PercentagemInteira is ceiling(Percentagem),
        Threshold < Percentagem
    ), ResultadosDuplicados),
    sort(ResultadosDuplicados, Resultados).

% -------------------------------------------------------------------------------------
% cab1(+NomePessoa, +MesaPossivel)
% eh verdade se NomePessoa for a pessoa que fica na cabeceira 1 de MesaPossivel
% -------------------------------------------------------------------------------------
cab1(NomePessoa, MesaPossivel):- 
    nth1(4, MesaPossivel, NomePessoa).

% -------------------------------------------------------------------------------------
% cab2(+NomePessoa, +MesaPossivel)
% eh verdade se NomePessoa for a pessoa que fica na cabeceira 2 de MesaPossivel
% -------------------------------------------------------------------------------------    
cab2(NomePessoa, MesaPossivel):- 
    nth1(5, MesaPossivel, NomePessoa).

% -------------------------------------------------------------------------------------
% honra(+NomePessoa1, +NomePessoa2, +MesaPossivel)
% eh verdade se NomePessoa1 estiver numa das cabeceiras de MesaPossivel e NomePessoa2 
% ficar ah sua direita
% -------------------------------------------------------------------------------------
honra(NomePessoa1, NomePessoa2, MesaPossivel):- 
    (nth1(4, MesaPossivel, NomePessoa1), nth1(6, MesaPossivel, NomePessoa2));
    (nth1(3, MesaPossivel, NomePessoa2), nth1(5, MesaPossivel, NomePessoa1)).

% -------------------------------------------------------------------------------------
% lado(+NomePessoa1, +NomePessoa2, +MesaPossivel)
% eh verdade se NomePessoa1 e NomePessoa2 ficarem lado a lado em MesaPossivel
% -------------------------------------------------------------------------------------
lado(NomePessoa1, NomePessoa2, MesaPossivel):- 
    (nextto(NomePessoa1, NomePessoa2, MesaPossivel);
    nextto(NomePessoa2, NomePessoa1, MesaPossivel)),
    % verifica que NomePessoa1 e NomePessoa2 nao estao nas cabeceiras
    \+nth1(4, MesaPossivel, NomePessoa1),
    \+nth1(4, MesaPossivel, NomePessoa2),
    \+nth1(5, MesaPossivel, NomePessoa1),
    \+nth1(5, MesaPossivel, NomePessoa2).

% -------------------------------------------------------------------------------------
% naoLado(+NomePessoa1, +NomePessoa2, +MesaPossivel)
% eh verdade se NomePessoa1 e NomePessoa2  nao ficarem lado a lado em MesaPossivel
% -------------------------------------------------------------------------------------
naoLado(NomePessoa1, NomePessoa2, MesaPossivel):-
    \+lado(NomePessoa1, NomePessoa2, MesaPossivel).

% -------------------------------------------------------------------------------------
% frente(+NomePessoa1, +NomePessoa2, +MesaPossivel)
% eh verdade se NomePessoa1 e NomePessoa2 ficarem frente a frente em MesaPossivel
% -------------------------------------------------------------------------------------
frente(NomePessoa1, NomePessoa2, MesaPossivel):-
    (nth1(1, MesaPossivel, NomePessoa1), nth1(6, MesaPossivel, NomePessoa2));
    (nth1(1, MesaPossivel, NomePessoa2), nth1(6, MesaPossivel, NomePessoa1));
    (nth1(2, MesaPossivel, NomePessoa1), nth1(7, MesaPossivel, NomePessoa2));
    (nth1(2, MesaPossivel, NomePessoa2), nth1(7, MesaPossivel, NomePessoa1));
    (nth1(3, MesaPossivel, NomePessoa1), nth1(8, MesaPossivel, NomePessoa2));
    (nth1(3, MesaPossivel, NomePessoa2), nth1(8, MesaPossivel, NomePessoa1)).

% -------------------------------------------------------------------------------------
% naoFrente(+NomePessoa1, +NomePessoa2, +MesaPossivel)
% eh verdade se NomePessoa1 e NomePessoa2 nao ficarem frente a frente em MesaPossivel
% -------------------------------------------------------------------------------------
naoFrente(NomePessoa1, NomePessoa2, MesaPossivel):-
    \+frente(NomePessoa1, NomePessoa2, MesaPossivel).

% -------------------------------------------------------------------------------------
% formataMesa(+OcupacaoMesaNaoFormatada, -OcupacaoMesa)
% Auxiliar - coloca a mesa no formato pedido em ocupacaoMesa/3
% OcupacaoMesa eh uma lista com tres listas. A primeira contem os primeiros tres 
% elementos de OcupacaoMesaNaoFormatada, a segunda os dois elementos seguintes
% e a terceira lista contem os restantes elementos
% -------------------------------------------------------------------------------------
formataMesa([X1, X2, X3, X4, X5, X6, X7, X8], [[X1, X2, X3], [X4, X5], [X6, X7, X8]]).

% -------------------------------------------------------------------------------------
% aplicaRestricoes(+ListaRestricoes, +OcupacaoMesa)
% Auxiliar - verifica cada restricao em ListaRestricoes, uma a uma 
% eh verdadeiro se OcupacaoMesa cumprir todas as restricoes em ListaRestricoes
% -------------------------------------------------------------------------------------
aplicaRestricoes([], _).

aplicaRestricoes([Restricao | Restricoes], OcupacaoMesaAux):-
    call(Restricao, OcupacaoMesaAux),
    aplicaRestricoes(Restricoes, OcupacaoMesaAux).

% -------------------------------------------------------------------------------------
% ocupacaoMesa(+ListaPessoas, +ListaRestricoes, -OcupacaoMesa) 
% OcupacaoMesa eh uma lista com tres listas
% A primeira contem as pessoas de um lado da mesa (X1, X2 e X3), a segunda as
% pessoas ah cabeceira (X4 e X5) e a terceira as pessoas do outro lado da
% mesa (X6, X7 e X8), de modo a que essas pessoas sao exactamente as da 
% ListaPessoas e verificam todas as restricoes de ListaRestricoes
% -------------------------------------------------------------------------------------
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa):- 
    permutation(ListaPessoas, OcupacaoMesaAux),
    aplicaRestricoes(ListaRestricoes, OcupacaoMesaAux),
    formataMesa(OcupacaoMesaAux, OcupacaoMesa).

