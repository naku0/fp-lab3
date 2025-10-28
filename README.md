# lab3

## Выбранные методы интерполяции
- Линейная
- Ньютон
- Лагранж

## Как пользоваться?

Запуск с флагами:
- `--method` Доступные методы будут ниже во всем объеме
- `--n` Количество начальных точек
- `--step` Шаг

```haskell
parseMethod :: String -> Maybe InterpolationMethod
parseMethod s = case map toLower s of
  "linear" -> Just Linear
  "lin" -> Just Linear
  "newton" -> Just Newton
  "new" -> Just Newton
  "lagrange" -> Just Lagrange
  "lag" -> Just Lagrange
  "newton-lagrange" -> Just NewtonLagrange
  "lagrange-newton" -> Just NewtonLagrange
  "new-lag" -> Just NewtonLagrange
  "newlag" -> Just NewtonLagrange
  "lag-new" -> Just NewtonLagrange
  "lagnew" -> Just NewtonLagrange
  "linear-newton" -> Just LinearNewton
  "newton-linear" -> Just LinearNewton
  "linnew" -> Just LinearNewton
  "lin-new" -> Just LinearNewton
  "newlin" -> Just LinearNewton
  "new-lin" -> Just LinearNewton
  "linear-lagrange" -> Just LinearLagrange
  "lin-lag" -> Just LinearLagrange
  "linlag" -> Just LinearLagrange
  "lagrange-linear" -> Just LinearLagrange
  "lag-lin" -> Just LinearLagrange
  "lin-lag" -> Just LinearLagrange
  _ -> Nothing
  ```

Выбирайте методы из списка выше, написал побольше: все для удобства использования

## Пример запуска
```bash
stack run -- --method new-lag --n 2 --step 0.4
Please enter 2 points, format example x;y / x,y / x y
Point 1> 0 0    #ваши начальные точки
Point 2> 1 1    #ваши начальные точки
Current config is Config {method = NewtonLagrange, startPoints = 2, step = 0.4} #ваш итоговый конфиг
Your points: [(0.0,0.0),(1.0,1.0)] #ваши начальные точки
Type EOF if you want to exit
NewtonLagrange: (0.00; 0.00)
NewtonLagrange: (0.40; 0.40)
NewtonLagrange: (0.80; 0.80)
> #Ввод
```
## Требования

>[!NOTE]
>я отделял требования таким обозначением `__________________` для удобного чтения

 - обязательно должна быть реализована линейная интерполяция (отрезками, link);

**Done ✅**

```haskell
linearInt :: [Point] -> Double -> Double
```
in [`src/Mathblock.hs`](https://github.com/naku0/fp-lab3/blob/main/src/MathBlock.hs)

**__________________**


 - настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:


   - какие алгоритмы использовать (в том числе два сразу);
   - частота дискретизации результирующих данных;
   - и т.п.;

**Done ✅**

```haskell
parseRead :: [String] -> Config -> Config
```
in [`src/Parse.hs`](https://github.com/naku0/fp-lab3/blob/main/src/Parse.hs)

**__________________**

 - входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;

**Done ✅**
Точки задаются в формате *`x;y / x,y / x y`*
 ```haskell
 parsePoint :: String -> Maybe Point
 ```
in [`src/Parse.hs`](https://github.com/naku0/fp-lab3/blob/main/src/Parse.hs)

а также точки сортируются по возрастанию x в методах интерполяции in [`src/Mathblock.hs`](https://github.com/naku0/fp-lab3/blob/main/src/MathBlock.hs)

**__________________**

 - выходные данные должны подаваться на стандартный вывод;

 - программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;


**Done ✅**
```haskell
 consoleRead :: Config -> [Point] -> IO ()
```
in [`src/Console.hs`](https://github.com/naku0/fp-lab3/blob/main/src/Console.hs)

**__________________**

