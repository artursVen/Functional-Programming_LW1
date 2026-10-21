Funkcionālā programmēšana. Laboratorijas darbs N1 

0. Lejupielādējiet un instalējiet savā datorā Haskell kompilatoru 

1. Izveidojiet funkciju, lai noteiktu maksimālo skaitli no skaitļu saraksta [5, 10, 11, 15, 3, 8]; 

Risinājums: 

   - Kārtot sarakstu dilstošā secībā 

   - Izvadiet (print) pirmo skaitli Requirement: Function should return meaningful output with short description, e.g. “The maximnumber is XXX” 

2. Izveidot funkciju, kas apgriež ievadītā vārda simbolus Risinājums 

Definējiet funkciju ar šādu funkcionalitāti: ar nosaukumu, aprēķiniet un izdrukājiet apgriezto versiju. 

Programmai ir jāpieņem ievadu no lietotāju (virkni formatā). 

   - Sadaliet virkni rakstzīmju masīvā 

   - Sakārtojiet to apgrieztā secībā 

   - Apvienojiet rakstzīmes atpakaļ 

3. Izveidot funkciju, kas Katra Teikuma Vārda Pirmo Burtu Pārveido Par Lielo Burtu. Programmai ir jāpieņem ievadu no lietotāju (virkni formatā) Risinājums: 

   - Sadaliet teikumu vārdos, izmantojot atstarpju rakstzīmi. 

   - Katru vārdu pārveidojiet, lai tā pirmais burts būtu lielais. 

   - Apvienojiet vārdus atpakaļ, izmantojot atstarpi. 

4. Izveidojiet funkciju, kas atgriež vienu pirms pēdējo elementu no saraksta. 

5. Definējiet operatoru (!!), kas atgriež saraksta i-to elementu. 

6. Uzrakstiet funkciju, kas atgriež visas iespējas izņemt vienu elementu no saraksta. Piemēram: gaps [1,2,3,4,5] = [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]] 

7. Apsveriet šādu bināro koku datu tipa definīciju, kuru mēs izmantosim, lai realizētu bināros meklēšanas kokus: 

data Tree a = Branch a (Tree a) (Tree a) | Leaf 

Uzrakstiet funkciju isSearchTree :: Tree a −> Bool , kas pārbauda, vai tās arguments ir binārais meklēšanas koks. 

Pēc tam pārbaudiet īpašību, ka, ja tiek dots binārais meklēšanas koks t, jaunas vērtības ievietošana kokā rada vēl vienu bināro meklēšanas koku. Kods jaunās vērtības ievietošanai kokā ir: 

insertTree :: Ord a => a −> Tree a −> Tree a 
insertTree e Leaf = Branch e Leaf Leaf insertTree 
e (Branch x li re) 
| e <= x = Branch x (insertTree e li) re 
| e > x = Branch x li (insertTree e re) 

Eksperimentējiet ar insertTree implementācijas mainīšanu, lai noskaidrotu, vai jūsu īpašība spēj atklāt, ka modificētā implementācija vairs neatspoguļo bināros meklēšanas kokus kā bināros meklēšanas kokus. 

8. Pierādiet šādu likumu par list funkciju 

map f . concat = concat . map (map f) 