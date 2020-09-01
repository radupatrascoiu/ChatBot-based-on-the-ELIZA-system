:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :- fail.

list_sublist(List, SubList) :-
    append(_, Rest, List),
    append(SubList, _, Rest).

match_rule(Tokens, _UserMemory, rule(R, _, _, _, _)) :- 
    list_sublist(Tokens, R).

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)

find_matching_rules(Tokens, Rules, _UserMemory, MatchingRules) :- 
    findall(R, (member(R, Rules), match_rule(Tokens, _, R)), MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

% flattent pe un singur nivel
flatten_first_level([], []).
flatten_first_level([H|T], Rez) :-
    flatten_first_level(T, N),
    append(H, N, Rez).

% sterge ultimul element
delete_last_elem([_], []).
delete_last_elem([H, N|T], [H|NT]) :-
    delete_last_elem([N|T], NT). % N - NEXT

% verifica daca ultimul element este 'nu inteleg'
check_last_elem(L, InputKeys) :-
    length(L, Len),
    (   Len > 1,
        delete_last_elem(L, InputKeys)
    ;   InputKeys=L
    ).

% verifica daca este sfarsit de conversatie
check_sfarsit_conversatie(Tokens, Actions) :-
    (   Exit_list=[[pa], [bye], [la, revedere]],
        \+ member(Tokens, Exit_list),
        Actions=[]
    ;   Actions=[exit]
    ).

% helper pentru a obine regulile in get_matched_rules 
get_rules(Tokens, X) :-
    rules(Reply, X),
    ord_subset(Reply, Tokens).

% obtine toate regulile care fac match cu regula de input
get_matched_rules(Tokens, RulesToUse) :-
    findall(X, get_rules(Tokens, X), RulesFound),  % Ia toate regulile care fac match cu replica noastra
    flatten(RulesFound, MatchingRules),         % lista liste cuvinte => lista de cuvinte
    find_matching_rules(Tokens, MatchingRules, _, RulesToUse).       % Ia toate regulile care fac match cu replica noastra

% elimina 'nu inteleg', pentru ca se face match mereu
process_rules(ProcessingRules, InputKeys) :-
    flatten_first_level(ProcessingRules, Y),      % flatten pe un nivel din lista                        
    check_last_elem(Y, InputKeys).

% obtine o lista de cuvinte din cheia memoriei
get_key(InputKeys, KeyHash) :-
    member(X, InputKeys),
    unwords(X, KeyHash).

% transforma memoria lui Gigel intr-o lista de forama (reply, score)
transform_hashTable(InputKeys, BotMemory, HashTable) :-
    findall((KeyHash, ValHash),
            ( get_key(InputKeys, KeyHash),
              get_value(BotMemory, KeyHash, ValHash)
            ),
            HashTable).

% obtine lista de replici din regulile obtinute la pasul get_matched_rules
get_processing_rules(Tokens, RulesToUse, ProcessingRules) :-
    findall(Iterator,
            member(rule(Tokens,
                        Iterator,
                        _,
                        _,
                        _),
                   RulesToUse),
            ProcessingRules).

% obtine replica cu numarul de utilizari minim
process_min_element(HashTable, Answer) :-
    min_element(HashTable, StringAnswer),
    words(StringAnswer, Answer).

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, _UserMemory, BotMemory, Answer, Actions) :-
    check_sfarsit_conversatie(Tokens, Actions),
    get_matched_rules(Tokens, RulesToUse),
    get_processing_rules(Tokens, RulesToUse, ProcessingRules),
    process_rules(ProcessingRules, InputKeys),
    transform_hashTable(InputKeys, BotMemory, HashTable),
    process_min_element(HashTable, Answer).

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :-
    not(member(exit, Actions)).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.

% find_occurrences/2
% find_occurrences(+UserMemory, -Result)
find_occurrences(_UserMemory, _Result) :- fail.

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(_UserMemory, _Score) :- fail.

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(_UserMemory, _Score) :- fail.

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(_UserMemory, _Emotion) :- fail.

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(_Tag, _UserMemory, _Score) :- fail.

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(_UserMemory, _Tag) :- fail.