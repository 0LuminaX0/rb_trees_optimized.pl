/* 
  Оптимізована реалізація червоно-чорних дерев на Prolog
  Початковий автор: Roman Pitak
  Оптимізація та розширення: Andrii Negrub
  
  Структура дерева: t(Колір, Ключ, ЛівеПіддерево, ПравеПіддерево)
  Кольори: b - чорний, r - червоний, bb - подвійний чорний (для балансування)
  Порожнє дерево: t(b, nil, nil, nil)
*/

:- use_module(library(clpfd)).

% RB_EMPTY: Визначає порожнє дерево
% rb_empty(?Tree).
rb_empty(t(b, nil, nil, nil)).

% RB_INSERT: ставляє елемент у дерево
% rb_insert( +Tree, +Key, -Tree ).
rb_insert(T1, K, T3) :-
  rb_ins(T1, K, T2),
  rb_reblackRoot(T2, T3).
% rb_ins( +Tree, +Key, -Tree ).
rb_ins(t(b, nil, nil, nil), K, t(r, K, t(b, nil, nil, nil), t(b, nil, nil, nil))).
rb_ins( t( C, K, Ls, Rs ), K, t( C, K, Ls, Rs ) ).
rb_ins(t(C, K, Ls, Rs), Ki, Tfixed) :-
  ( Ki #< K -> rb_ins(Ls, Ki, Lsi), T = t(C, K, Lsi, Rs)
  ; Ki #> K -> rb_ins(Rs, Ki, Rsi), T = t(C, K, Ls, Rsi)),
  rb_insFixup(T, Tfixed).

% Забезпечення чорного кольору кореня
% rb_reblackRoot( +Tree, -Tree ).
rb_reblackRoot( t( _, K, Ls, Rs ), t( b, K, Ls, Rs ) ).

% Підтримка балансу після вставки
% rb_insFixup( +Tree, -FixedTree ).
% Випадок 1: дядько червоний - перефарбування
rb_insFixup( t( b, Kz, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), t( r, Ku, Lsu, Rsu ) ), t( r, Kz, t( b, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), t( b, Ku, Lsu, Rsu ) ) ).
rb_insFixup( t( b, Kz, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), t( r, Ku, Lsu, Rsu ) ), t( r, Kz, t( b, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), t( b, Ku, Lsu, Rsu ) ) ).
rb_insFixup( t( b, Kz, t( r, Ku, Lsu, Rsu ), t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ), t( r, Kz, t( b, Ku, Lsu, Rsu ), t( b, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ) ).
rb_insFixup( t( b, Kz, t( r, Ku, Lsu, Rsu ), t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ), t( r, Kz, t( b, Ku, Lsu, Rsu ), t( b, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ) ).
% Випадок 2: дядько чорний, "зигзаг" - подвійне обертання
rb_insFixup( t( b, Kz, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ), Uncle ), Tfixed ) :-
  rb_insFixup( t( b, Kz, t( r, Kx, t( r, Ky, Lsy, Lsx ), Rsx ), Uncle ), Tfixed ).
rb_insFixup( t( b, Kz, Uncle, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ) ), Tfixed ) :-
  rb_insFixup( t( b, Kz, Uncle, t( r, Kx, Lsx, t( r, Ky, Rsx, Rsy ) ) ), Tfixed ).
% Випадок 3: дядько чорний, "пряма лінія" - одинарне обертання
rb_insFixup( t( b, Kz, t( r, Ky, t( r, Kx, Lsx, Rsx ), Rsy ), Uncle ), t( b, Ky, t( r, Kx, Lsx, Rsx ), t( r, Kz, Rsy, Uncle ) ) ).
rb_insFixup( t( b, Kz, Uncle, t( r, Ky, Lsy, t( r, Kx, Lsx, Rsx ) ) ), t( b, Ky, t( r, Kz, Uncle, Lsy ), t( r, Kx, Lsx, Rsx ) ) ).
% Випадок 4: стандартний гарний випадок (з деревом все гаразд (ймовірно))
rb_insFixup( T, T ).


% Публічний предикат видалення
% rb_delete( +Tree, +Key, -Tree ).
rb_delete( T, K, Trb ) :-
  rb_del( T, K, Td ), 
  rb_reblackRoot( Td, Trb ).
  
% Ключ не знайдено
rb_del( t( b, nil, nil, nil ),  _, t( b, nil, nil, nil ) ).
% Видалення червоного вузла без дочірніх вузлів
rb_del( t( r, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), K, t( b, nil, nil, nil ) ).
% Видалення червоного вузла з лівим нащадком
rb_del( t( r, K, Ls, t( b, nil, nil, nil ) ), K, Ls ).
% Видалення червоного вузла з правим нащадком
rb_del( t( r, K, t( b, nil, nil, nil ), Rs ), K, Rs ).

% Видалення чорного вузла з лівим червоним нащадком
rb_del( t( b, K, t( r, Kl, Lsl, Rsl ), t( b, nil, nil, nil ) ), K, t( b, Kl, Lsl, Rsl ) ).
% Видалення чорного вузла з правим червоним нащадком
rb_del( t( b, K, t( b, nil, nil, nil ), t( r, Kr, Lsr, Rsr ) ), K, t( b, Kr, Lsr, Rsr ) ).
% оскільки чорна вершина не може мати чорного єдиного нащадка, єдиним проблемним випадком 
% є видалення чорної вершини без нащадків (через зміну ваг дерева). 
% Ми пофарбуємо нову вершину у колір 'bb', що означає подвійний чорний колір, який треба буде розподілити
rb_del( t( b, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), K, t( bb, nil, nil, nil ) ).

% Видалення вузла з двома дітьми
rb_del( t( C, K, Ls, Rs ), K, Tfixed ) :-
  rb_maximum( Ls, Max ), 
  rb_del( Ls, Max, Lsd ),
  rb_delFixup( t( C, Max, Lsd, Rs ), Tfixed ).
% розгалуження видалення
rb_del( t( C, K, Ls, Rs ), Kd, Tfixed ) :-
  K #> Kd, 
  rb_del( Ls, Kd, Lsd ),
  rb_delFixup( t( C, K, Lsd, Rs ), Tfixed ).
rb_del( t( C, K, Ls, Rs ), Kd, Tfixed ) :-
  K #< Kd, 
  rb_del( Rs, Kd, Rsd ),
  rb_delFixup( t( C, K, Ls, Rsd ), Tfixed ).
  
% rb_delFixup( +Tree, -FixedTree ).
% Випадок 1: брат червоний
rb_delFixup( t( b, Kp, N, t( r, Ks, Lss, Rss ) ), t( b, Ks, Tfixed, Rss ) ) :-
  N = t( bb, _, _, _ ),
  rb_delFixup( t( r, Kp, N, Lss ), Tfixed ).
rb_delFixup( t( b, Kp, t( r, Ks, Lss, Rss ), N ), t( b, Ks, Lss, Tfixed ) ) :-
  N = t( bb, _, _, _ ),
  rb_delFixup( t( r, Kp, Rss, N ), Tfixed ).
  
% Випадок 2: брат чорний, діти брата чорні, батько чорний
rb_delFixup( t( b, Kp, t( bb, Kn, Lsn, Rsn ), t( b, Ks, Lss, Rss ) ),
             t( bb, Kp, t( b, Kn, Lsn, Rsn ), t( r, Ks, Lss, Rss ) ) ) :-
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
rb_delFixup( t( b, Kp, t( b, Ks, Lss, Rss ), t( bb, Kn, Lsn, Rsn ) ), 
             t( bb, Kp, t( r, Ks, Lss, Rss ), t( b, Kn, Lsn, Rsn ) ) ) :-
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).

% Випадок 3: брат чорний, діти брата чорні, батько червоний
rb_delFixup( t( r, Kp, N, t( b, Ks, Lss, Rss ) ),
             t( b, Kp, N2, t( r, Ks, Lss, Rss ) ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
rb_delFixup( t( r, Kp, t( b, Ks, Lss, Rss ), N ), 
             t( b, Kp, t( r, Ks, Lss, Rss ), N2 ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  Lss = t( b, _, _, _ ),
  Rss = t( b, _, _, _ ).
  
% Випадок 4: брат чорний, ближчий племінник червоний, дальший племінник чорний
rb_delFixup( t( Cp, Kp, N, S ), Tfixed ) :-
  N = t( bb, _, _, _ ),
  S = t( b, Ks, Lss, Rss ),
  Lss = t( r, Ksl, Lsl, Rsl ),
  Rss = t( b, _, _, _ ),
  rb_delFixup( t( Cp, Kp, N, t( b, Ksl, Lsl, t( r, Ks, Rsl, Rss ) ) ), Tfixed ).
rb_delFixup( t( Cp, Kp, S, N ), Tfixed ) :-
  N = t( bb, _, _, _ ),
  S = t( b, Ks, Lss, Rss ),
  Lss = t( b, _, _, _ ),
  Rss = t( r, Ksr, Lsr, Rsr ),
  rb_delFixup( t( Cp, Kp, t( b, Ksr, t( r, Ks, Lss, Lsr ), Rsr ), N ), Tfixed ).
  
% Випадок 5: брат чорний, дальший племінник червоний
rb_delFixup( t( Cp, Kp, N, S ), 
             t( Cp, Ks, t( b, Kp, N2, Lss ), Rss2 ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  S = t( b, Ks, Lss, Rss ), 
  Rss = t( r, Kr, Lsr, Rsr ),
  Rss2 = t( b, Kr, Lsr, Rsr ).
  
rb_delFixup( t( Cp, Kp, S, N ), 
             t( Cp, Ks, Lss2, t( b, Kp, Rss, N2 ) ) ) :-
  N = t( bb, Kn, Lsn, Rsn ), 
  N2 = t( b, Kn, Lsn, Rsn ),
  S = t( b, Ks, Lss, Rss ), 
  Lss = t( r, Kl, Lsl, Rsl ),
  Lss2 = t( b, Kl, Lsl, Rsl ).
			 
% Випадок 6: стандартний гарний випадок (з деревом все гаразд (ймовірно))
rb_delFixup( T, T ).

% RB_SEARCH: Шукає елемент у дереві
% rb_search(+Tree, +Element).
rb_search(t(b, nil, nil, nil), _) :- !, fail.
rb_search(t(_, K, _, _), K).
rb_search(t(_, K, Ls, _), Ks) :-
  K #> Ks,
  rb_search(Ls, Ks).
rb_search(t(_, K, _, Rs), Ks) :-
  K #< Ks,
  rb_search(Rs, Ks).

% RB_INORDER: Обхід дерева у порядку зростання
% rb_inOrder(+Tree, -List).
rb_inOrder(t(b, nil, nil, nil), []).
rb_inOrder(t(_, K, Ls, Rs), L) :-
  rb_inOrder(Ls, Ll), 
  rb_inOrder(Rs, Lr),
  append(Ll, [K | Lr], L).
  
% RB_PREORDER: Обхід дерева у прямому порядку (корінь, ліве, праве)
% rb_preOrder(+Tree, -List).
rb_preOrder(t(b, nil, nil, nil), []).
rb_preOrder(t(_, K, Ls, Rs), L) :-
  rb_preOrder(Ls, Ll), 
  rb_preOrder(Rs, Lr),
  append([K | Ll], Lr, L).
  
% RB_POSTORDER: Обхід дерева у зворотному порядку (ліве, праве, корінь)
% rb_postOrder(+Tree, -List).
rb_postOrder(t(b, nil, nil, nil), []).
rb_postOrder(t( _, K, Ls, Rs), L) :-
  rb_postOrder(Ls, Ll), 
  rb_postOrder(Rs, Lr),
  append(Ll, Lr, Lp), 
  append(Lp, [K], L).
  
% RB_BUILD: Будує дерево зі списку
% rb_build( +List, -Tree )
rb_build([], T) :- rb_empty(T).
rb_build([K | Rest], Tree) :-
  rb_build(Rest, T1),
  rb_insert(T1, K, Tree).


% RB_CHECK: Перевірка на правиьність виводу
% rb_check( +Tree ).
rb_check( t( b, K, Ls, Rs ) ) :-
  rb_ch( t( b, K, Ls, Rs ), true, _ ),
  write( 'rb_check successfull' ), nl.

% rb_ch( +Tree, -Validity, -NumberOfBlackNodes ).
% Nbn - кількість чорних вузлів
rb_ch( t( b, nil, nil, nil ), true, 1 ).
% чорний вузол без дітей
rb_ch( t( b, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), true, 2 ) :- !, 
  K \= nil.

% чорний вузол з лівим нащадком
rb_ch( t( b, K, t( r, Kl, Lsl, Rsl ), t( b, nil, nil, nil ) ), true, 2 ) :- !, 
  rb_ch( t( r, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  Bl = true,
  Nl = 1,
  K \= nil,
  Kl \= nil,
  K #> Kl.
% чорний вузол з правим нащадком
rb_ch( t( b, K, t( b, nil, nil, nil ), t( r, Kr, Lsr, Rsr ) ), true, 2 ) :- !, 
  rb_ch( t( r, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Br = true,
  Nr = 1,
  K \= nil,
  Kr \= nil,
  K #< Kr.
% чорний вузол з обома нащадками
rb_ch( t( b, K, t( Cl, Kl, Lsl, Rsl ), t( Cr, Kr, Lsr, Rsr ) ), true, N ) :- !, 
  rb_ch( t( Cl, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  rb_ch( t( Cr, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Bl = true, 
  Br = true,
  Nl = Nr,
  N is Nl + 1,
  K \= nil,
  Kl \= nil,
  Kr \= nil,
  K #> Kl, 
  K #< Kr.  
% червоний вузол без нащадків
rb_ch( t( r, K, t( b, nil, nil, nil ), t( b, nil, nil, nil ) ), true, 1 ) :- !, 
  K \= nil.
% червоного вузла з одним нащадком не існує!
% червоний вузол з обома нащадками
rb_ch( t( r, K, t( b, Kl, Lsl, Rsl ), t( b, Kr, Lsr, Rsr ) ), true, N ) :- !, 
  rb_ch( t( b, Kl, Lsl, Rsl ), Bl, Nl ), !, 
  rb_ch( t( b, Kr, Lsr, Rsr ), Br, Nr ), !, 
  Bl = true, 
  Br = true,
  Nl = Nr,
  N is Nl + 0,
  K \= nil,
  Kl \= nil,
  Kr \= nil,
  K #> Kl, 
  K #< Kr.
  
% default FALSE
rb_ch( T, false, 0 ) :- !, 
  rb_print( T ).

% RB_MIN/MAX: Знаходить мінімальний та максимальний елементи
% rb_maximum( +Tree, -Maximum ).
% fails on empty !
rb_minimum(t(_, K, t(b, nil, nil, nil), _), K).
rb_minimum(t(_, _, Ls, _), Min) :- rb_minimum(Ls, Min).

rb_maximum(t(_, K, _, t(b, nil, nil, nil)), K).
rb_maximum(t(_, _, _, Rs), Max) :- rb_maximum(Rs, Max).
  
% RB_PRINT: Видруковує дерево в консоль
% rb_print( +Tree ).
rb_print( T ) :-
  write( '[ ] Black node' ), nl, 
  write( '( ) Red node' ), nl, nl,
  rb_print( T, [] , nodebug ), nl, !.

rb_print( t( b, nil, nil, nil ), _, nodebug ).
rb_print( nil, N , debug ) :- !, 
  rb_spaces( N ), write( 'nil' ), nl.
rb_print( t( b, nil, Ls, Rs ), N , debug ) :- 
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , debug ), !, 
  rb_spaces( N ), write( '[' ), write( nil ), write( ']' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , debug ), !.
rb_print( t( b, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), write( '[' ), write( K ), write( ']' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.
rb_print( t( r, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), write( '(' ), write( K ), write( ')' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.
rb_print( t( bb, K, Ls, Rs ), N , D ) :-
  append( N, [ 'r' ], Nr ), rb_print( Rs, Nr , D ), !, 
  rb_spaces( N ), write( '{' ), write( K ), write( '}' ), nl,
  append( N, [ 'l' ], Nl ), rb_print( Ls, Nl , D ), !.

rb_spaces( [] ).
rb_spaces( [ _ ] ) :- 
  write( ' |---' ).
rb_spaces( [ H, H | T ] ) :-
  write( '     ' ), 
  rb_spaces( [ H | T ] ).
rb_spaces( [ _ | T ] ) :-
  write( ' |   ' ), 
  rb_spaces( T ).

%=================================================================================
% Приклади:

% ?- rb_empty(TreeEmpty),
%    rb_insert(TreeEmpty, 5, Tree1),
%    rb_insert(Tree1, 3, Tree2),
%    rb_insert(Tree2, 4, Tree3),
%    rb_delete(Tree3, 3, Tree4),
%    rb_delete(Tree4, 4, Tree5),
%    rb_delete(Tree5, 15, Tree6).

%    TreeEmpty = t(b, nil, nil, nil),
%    Tree1 = t(b, 5, t(b, nil, nil, nil), t(b, nil, nil, nil)),
%    Tree2 = t(b, 5, t(b, nil, nil, nil), t(r, 3, t(b, nil, nil, nil), t(b, nil, nil, nil))),
%    Tree3 = t(b, 4, t(r, 3, t(b, nil, nil, nil), t(b, nil, nil, nil)), t(r, 5, t(b, nil, nil, nil), t(b, nil, nil, nil)))
%    Tree4 = t(b, 4, t(b, nil, nil, nil), t(r, 5, t(b, nil, nil, nil), t(b, nil, nil, nil))),
%    Tree5 = Tree6,
%    Tree6 = t(b, 5, t(b, nil, nil, nil), t(b, nil, nil, nil)),

% ?- rb_build([1,2,3,4,5], T).

% T = 
%   t(
%     b, 4, t(
%              b, 2,
%              t(r,1,t(b,nil,nil,nil),t(b,nil,nil,nil)),
%              t(r,3,t(b,nil,nil,nil),t(b,nil,nil,nil))
%            ),
%     t(b,5,t(b,nil,nil,nil),t(b,nil,nil,nil))
%   )


%   ?- rb_print(T).

%   [ ] Black node
%   ( ) Red node
  
%    |---[5]
%   [4]
%    |    |---(3)
%    |---[2]
%         |---(1)


% ?- rb_minimum(T, K),
%    rb_maximum(T,KK).

%    K = 1,
%    KK = 5.