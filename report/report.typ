#import "@preview/chronos:0.2.0"
#import "@preview/syntree:0.2.0": syntree


#set par(justify: true)
#set text(lang: "es")

#set page(numbering: "1")
#set page(header: [
  _El lenguaje de programación Komodo_
  #h(1fr)
  // _Universidad Nacional de Colombia_
])

#set heading(numbering: "1.1.1.")

#page(
  align(
    center + horizon,
    [
      #text(17pt)[*El lenguaje de programación Komodo*] \
      \
      \
      César Danilo Pedraza Montoya \
      #link("mailto:cpedraza@unal.edu.co")
    ]
  ),
  numbering: none,
)

#page(outline())

= Introducción

Komodo es un lenguaje de programación hecho para probar ideas rápidamente. Es ideal para problemas con estructuras discretas como números y palabras.
Komodo intenta que operar con estas entidades sea tan fácil como sea posible mientras se minimiza la cantidad de código necesario para llegar
a una implementación exitosa. La otra prioridad de Komodo es la sencillez: se busca que el lenguaje sea pequeño y con reglas simples,
con el propósito de que pueda ser aprendido con facilidad.

Komodo está diseñado con la intención de convertirse en una herramienta útil para generar estructuras discretas que puedan depender
de muchas restricciones, para así estudiarlas. Esta es una tarea común en el estudio de areas de las matemáticas como la combinatoria, la teoría de la computación,
la teoría de grafos o la teoría de códigos. Usar el computador como una herramienta de exploración matemática es una práctica conocida como matemática experimental. @experimental[p.~2]

Este documento describe el lenguaje de programación Komodo. No es una guía de uso del lenguaje. También se exploran detalles del
intérprete de Komodo creado por el autor. Sin embargo, no se describe todo el comportamiento esperado de una implementación del lenguaje,
ni se proveen detalles del intérprete más allá de lo estructural.

= Visión general

El propósito de Komodo tiene consecuencias en su diseño. Puesto que Komodo es un lenguaje para _scripting_, no es una prioridad que el lenguaje
pueda procesarse a si mismo, o que la representación de los datos sea similar a la representación de los programas. Asimismo, el nivel de abstracción de Komodo y la etapa en que se encuentra el proyecto hacen preferible implementar un intérprete en lugar de un compilador.

El diseño de Komodo no es deliberado, sino que se ha llegado a él con una construcción iterativa.

En esta sección se explican brevemente algunas características de Komodo, que son descritas con mayor detalle en secciones posteriores.

== Sistema de tipos

Komodo es un lenguaje con tipado débil y dinámico. Lo primero significa que las reglas de tipos son relativamente laxas y se realizan conversiones implicitas de tipos, y lo segundo significa que estas reglas y conversiones son verificadas y realizadas en tiempo de ejecución. Esto es así por varias razones:

- Komodo está pensado para que las anotaciones de tipos sean totalmente opcionales, por lo que en general no es posible inferir los tipos de todas las variables en tiempo de compilación.

- Hace posible la implementación de un intérprete sin añadir análisis semántico, lo que fue útil para llegar rápido a un prototipo funcional.

- Komodo está pensado para realizar algunas conversiones de tipos implicitamente, lo que necesariamente implica que las restricciones de tipos son más ligeras.

Komodo también es de tipado latente, lo que significa que los tipos están asociados a valores y no a variables o símbolos. Esto hace que un símbolo pueda tener valores tipos distintos en momentos distintos. @piercelanguages[p.~2].

Además, Komodo tiene tipado gradual. Se realiza chequeo de tipos en tiempo de ejecución cuando el usuario provee restricciones de tipos en las firmas de funciones y variables.

== Paradigmas

Komodo emplea dos paradigmas de programación: procedural y funcional.

Por un lado, Komodo es un lenguaje procedural porque las formas y el orden importan: las sentencias de un programa son evaluadas en el
orden en que aparecen en el mismo. Esto establece una semántica clara para cambiar el valor de una variable sin que esta sea mutable, por ejemplo. (veáse @shadowing)

Además, en Komodo las funciones tienen un papel protagónico: pueden declararse de forma nombrada o anónima, pueden rastrear patrones y pueden pasarse como argumentos a otras funciones. La búsqueda de patrones de Komodo y su inclusión en las funciones permite describir procedimientos en términos de lo que deben hacer, en lugar de como.

Esto permite que dependiendo del problema, un programa de Komodo pueda ser más imperativo o más declarativo a conveniencia.
La forma en que se restringe la combinación de los dos paradigmas es limitando la mutabilidad de valores.

== La estructura del intérprete

Como suele suceder con múltiples compiladores e intérpretes, el intérprete de Komodo funciona como una cadena de procesamiento. Se comienza procesando texto, y tras cada paso se obtiene una representación del programa más preparada para ser ejecutada. En el caso de Komodo, se tienen las siguientes etapas:

+ Analizador léxico,
+ Analizador sintáctico,
+ Post-analizador sintáctico o _weeder_,
+ Evaluador,
+ Entorno de tiempo de ejecución o _runtime environment_.

Este es un diagrama de secuencia de los componentes del intérprete. Las columnas son los componentes, y las flechas son interfaces. En algunos casos las interfaces son estructuras de datos, y en otros son eventos invocados por el usuario.

#figure(
  chronos.diagram({
    import chronos: *
    _par("Sistema operativo")
    _par("Lexer")
    _par("Parser")
    _par("Weeder")
    _par("Evaluador")
    _par("Runtime")

    _seq("Sistema operativo", "Lexer", comment: "REPL")
    _seq("Sistema operativo", "Lexer", comment: "Archivos con código")
    _seq("Lexer", "Parser", comment: "Token")
    _seq("Parser", "Weeder", comment: "CST")
    _seq("Weeder", "Evaluador", comment: "AST")
    _seq("Evaluador", "Runtime", comment: "Declaraciones")
    _seq("Evaluador", "Runtime", comment: "Valores")

    _seq("Runtime", "Sistema operativo", comment: "Entrada/salida estándar")
    
  }),
  caption: "Componentes del intérprete y sus relaciones más importantes."
)

En este documento se hace una descripción del funcionamiento de cada componente y cada relación entre componentes, al mismo tiempo que se especifican aspectos de Komodo.

= Análisis léxico y sintáctico

== Analizador léxico o _lexer_

  El analizador léxico convierte un programa, una sucesión de caracteres, en una sucesión de _tokens_, que son unidades más complicadas como palabras, números y símbolos. Uno de los propósitos de esta fase es que las demás fases no tengan que lidiar con detalles relacionados al texto que representa el programa: Las fases posteriores no deberían lidiar con aspectos como espacios en blanco, indentación o comentarios en el código. Toda la información necesaria debería estar incluída en los _tokens_ que el analizador emite.

  La entrada del analizador es un _stream_ de caracteres Unicode. Sin embargo, la mayoría de palabras clave y símbolos se componen de caracteres ASCII. La salida es un _stream_ de _tokens_. El _lexer_ pasa una sola vez por el texto de entrada para emitir todos los _tokens_ correspondientes, y el texto es recorrido conforme los _tokens_ son emitidos.

  #figure(
    grid(
      columns: 1,
      gutter: 4mm,
      {
        show regex("let"): set text(fill: red)
        show regex("x"): set text(fill: blue)
        show regex(":="): set text(fill: purple)
        show regex("2"): set text(fill: maroon)

        ```
        let x := 2
        ```
      },
      sym.arrow.b,
      {
        show regex("Let"): set text(fill: red)
        show regex("Ident\(x\)"): set text(fill: blue)
        show regex("Assign"): set text(fill: purple)
        show regex("Integer\(2\)"): set text(fill: maroon)

        ```
        Let, Ident(x), Assign, Integer(2)
        ```
      },
    ),
    caption: [Ejemplo de paso de un texto a una sucesión de _tokens_]
  )

=== Lista de _tokens_
  
  Esta es una lista de los _tokens_ que el analizador léxico emite, y las reglas que hacen que sean emitidos. Se muestran expresiones regulares para algunos _tokens_ con el propósito de ilustrar las reglas rápidamente, pero la implementación del _lexer_ no usa expresiones regulares. Los tokens que están relacionados a los bloques de indentación indentados son casos especiales, cuyo funcionamiento se describe con más detalle en la siguiente sección.

  Las expresiones regulares están escritas con el estilo de Perl. @perlregex

  #{
    show figure: set block(breakable: true)
    figure(
      table(
        columns: 3,
        [Nombre], [Descripción], [Expresión regular],
        [`Ampersand`], [Sígno et: `&`], [`&`],
        [`Arrow`], [Flecha simple: `->`], [`->`],
        [`As`], [Palabra clave: `as`], [`as`],
        [`Assign`], [Símbolo de asignación: `:=`], [`:=`],
        [`Bang`], [Símbolo de exclamación: `!`], [`!`],
        [`Case`], [Palabra clave: `case`], [`case`],
        [`Colon`], [Dos puntos: `:`], [`:`],
        [`Comma`], [Coma: `,`], [`,`],
        [`Dedent`], [El final de un bloque indentado.], [],
        [`Do`], [Palabra clave: `do`], [`do`],
        [`Dot`], [Punto: `.`], [`\.`],
        [`DotDot`], [Punto tras punto: `..`], [`\.\.`],
        [`Else`], [Palabra clave: `else`], [`else`],
        [`Equals`], [Símbolo de igualdad: `=`], [`=`],
        [`False`], [Palabra clave: `false`], [`false`],
        [`FatArrow`], [Flecha gruesa: `=>`], [`=>`],
        [`For`], [Palabra clave: `for`], [`for`],
        [`From`], [Palabra clave: `from`], [`from`],
        [`Greater`], [Símbolo de _mayor que_: `>`], [`>`],
        [`GreaterEqual`], [Símbolo de _mayor o igual que_: `>=`], [`>=`],
        [`Ident`], [Un identificador.], [`\p{Alphabetic}[\p{Alphabetic}\p{GC=Number}]`],
        [`If`], [Palabra clave: `if`], [`if`],
        [`Import`], [Palabra clave: `import`], [`import`],
        [`In`], [Palabra clave: `in`], [`in`],
        [`Indent`], [El inicio de un bloque indentado.], [],
        [`Integer`], [Un entero en base 2, 8, 10 o 16.], [`(0)|(0(b|B)[0-1]+)|(0(o|O)[0-7]+)|([1-9][0-9]*)|(0(x|X)[0-9a-fA-F]+)`],
        [`Lbrace`], [Corchete izquierdo: `{`], [`{`],
        [`Lbrack`], [Paréntesis cuadrado izquierdo: `[`], [`\[`],
        [`LeftShift`], [Dos _menor que_ juntos: `<<`], [`<<`],
        [`Less`], [Símbolo de _menor que_: `<`], [`<`],
        [`LessEqual`], [Símbolo de _menor o igual que_: `<=`], [`<=`],
        [`Let`], [Palabra clave: `let`], [`let`],
        [`LogicAnd`], [Dos sígnos et juntos: `&&`], [`&&`],
        [`LogicOr`], [Dos barras verticales juntas: `||`], [`\|\|`],
        [`Lparen`], [Paréntesis izquierdo: `(`], [`\(`],
        [`Memoize`], [Palabra clave: `memoize`], [`memoize`],
        [`Minus`], [Guión: `-`], [`-`],
        [`Newline`], [Salto de línea.], [],
        [`NotEqual`], [Un _slash_ y un símbolo de igualdad: `/=`], [`\/=`],
        [`Percent`], [Símbolo de porcentaje: `%`], [`%`],
        [`Plus`], [Símbolo de suma: `+`], [`\+`],
        [`Rbrace`], [Corchete derecho: `}`], [`}`],
        [`Rbrack`], [Paréntesis cuadrado derecho: `]`], [`]`],
        [`RightShift`], [Dos _mayor que_ juntos: `>>`], [`>>`],
        [`Rparen`], [Paréntesis derecho: `)`], [`)`],
        [`Slash`], [Una barra inclinada: `/`], [`\/`],
        [`SlashSlash`], [Dos barras inclinadas: `//`], [`\/\/`],
        [`Star`], [Un asterisco: `*`], [`\*`],
        [`StarStar`], [Dos asteriscos: `**`], [`\*\*`],
        [`String`], [Una cadena de caracteres.], [`"[.\w]*"`],
        [`Then`], [Palabra clave: `then`], [`then`],
        [`Tilde`], [Una virgulilla: `~`], [`~`],
        [`True`], [Palabra clave: `true`], [`true`],
        [`Unknown`], [Un caracter no reconocido.], [],
        [`Var`], [Palabra clave: `var`], [`var`],
        [`VerticalBar`], [Barra vertical: `|`], [`\|`],
        [`Wildcard`], [Barra baja: `_`], [`_`],
      ),
      caption: [Tokens del _lexer_ de Komodo y sus reglas.]
    )
  }

Hay algunas particularidades a mencionar:

+ El _lexer_ ignora los segmentos que comienzan con un númeral `#` y terminan con un salto de línea. Estos son los comentarios de Komodo.

+ Los identificadores reciben toda la clase `Alphabetic` de Unicode en su primer caracter, y luego reciben caracteres de la clase `Alphabetic` o `Number`. @unicodepropslist
  
  Estos nombres son propiedades de caracteres Unicode. @unicodeprops

+ Como muestra su expresión regular, los identificadores no incluyen barras bajas en ningún punto. Están exlusivamente compuestos de caracteres alfanuméricos.

+ Los ceros a la izquierda en enteros decimales no están permitidos. Un cero sólo va al principio de un _token_ `Integer` cuando consiste en un solo cero, o cuando se va a escribir un prefijo para una base numérica no decimal (`0b`, `0o` o `0x`).

=== Rastreo de indentación y el alcance del analizador léxico

Los _tokens_ `Indent` y `Dedent` indican el inicio y el final de un bloque de código indentado, respectivamente. Por ejemplo, en el siguiente fragmento de código

#figure(
  ```
  let f(x) := x + 2


  ```,
  caption: "Ejemplo de declaración de una función",
)

el cuerpo de la función `f` está compuesto exactamente por la expresión `x + 2`. Si se requiere que el cuerpo de la función tenga más líneas de código, se puede iniciar un bloque en una nueva línea. Las líneas que pertenecen al bloque están espaciadas a la derecha por 4 espacios:

#figure(
  ```
  let f(x) :=
      let y := 2
      x + y


  ```,
  caption: "Ejemplo de declaración de una función con un bloque de código.",
)

En este caso, la función `f` esta compuesta por un bloque de código de dos líneas.

Para indicar el inicio de este bloque, el _lexer_ emite un token `Indent` antes de emitir los correspondientes al mismo. Tras haber emitido todos los _tokens_ del bloque, se emite un `Dedent` para indicar el fin de este.

Este comportamiento también ocurre cuando hay bloques dentro de bloques, como en el siguiente programa:

#figure(
  ```
  for i in 0..10 do
      # se emite el primer Indent
      for j in 0..10 do
          # se emite el segundo Indent
          mat[i][j] = i + j
  # Se emiten dos Dedent seguidos


  ```,
  caption: "Ejemplo de bloques anidados."
)

En este caso, el cuerpo del primer ciclo `for` es un un bloque de código, cuya única parte es otro ciclo `for`, cuyo cuerpo es otro bloque de una sola línea. Después del primer `do` se emite un `Indent`. Después del segundo `do` se emite otro `Indent`. Cuando se llega al final del texto, se emiten dos `Dedent` seguidos para "cerrar" los dos bloques de código que estaban "abiertos".

La razón para hacer esto es que al emitir estos _tokens_ se pueden entender los bloques de código de la misma forma que se hace con lenguajes donde los bloques están delimitados con caracteres como corchetes. Por ejemplo en JavaScript @ecmascript, este es un programa similar:

#figure(
  ```
  for (let i = 0; i < 10; i++) {
      for (let j = 0; j < 10; j++) {
          mat[i][j] = i + j;
      }
  }


  ```,
  caption: "Bloques anidados en JavaScript."
)

Los corchetes aquí cumplen la misma función que los `Indent` y `Dedent` en Komodo, solo que en este caso tienen una correspondencia directa con caracteres del texto. En el caso de Komodo son un artificio obtenido de contar espacios en blanco.

Esta es una descripción de como el _lexer_ decide emitir estos _tokens_:

+ El _lexer_ cuenta el nivel de indentación en el que se encuentra el programa en el punto en donde el texto está siendo leído. Cuando se está al principio del programa, este nivel es cero.

+ Cuando se llega a una nueva línea, se quiere contar su nivel de indentación. Esto se hace contando el número de espacios al principio de la línea. Cada 4 espacios son un nivel de indentación. Si quedan espacios sobrantes (es decir, el número de espacios no es múltiplo de 4), se ignora el residuo.
  
  Por ejemplo, la línea de código
  ```
          println(x)
  ```

  tiene 8 espacios al principio, por lo que su nivel de indentación es 2.

  Las líneas que estan compuestas exclusivamente de espacios o comentarios son ignoradas.

+ Una vez que se consumen y cuentan los espacios, y que se llega a un caracter que va a componer un _token_, se compara el nivel de indentación de la línea con el nivel de indentación que el _lexer_ guarda.

  - Si son iguales, no se emiten _tokens_ de más: el resto de la línea es consumida.

  - Si el nivel de la línea es mayor, se emiten tantos `Indent` como la diferencia entre el nivel de la línea y el nivel guardado en el _lexer_, y se consume el resto de la línea.

  - Si el nivel de la línea es menor, se emiten tantos `Dedent` como la diferencia entre el nivel guardado en el _lexer_ y el nivel de la línea. Luego se consume el resto de la línea.

Cabe destacar que la razón por la que hay que almacenar el nivel de indentación es por que las reglas con las que estos _tokens_ son emitidos son dependientes del contexto: no basta con conocer el caracter actual o una cantidad fija hacia adelante, sino, en general, es necesario poder recorrer todos los caracteres recorridos antes. Una solución más sensible es almacenar un estado útil (el nivel de indentación) para poder decidir cuando emitir los _tokens_.

Esta estrategia es la misma que usa el intérprete principal de Python, CPython.

== Analizador sintáctico o _parser_

El analizador sintáctico convierte sucesiones de _tokens_ en nodos de un árbol que describe la estructura sintáctica del programa, conocido como CST (del inglés _Concrete Syntax Tree_). Este árbol contiene todos los detalles del programa, y es generado casi en su totalidad de forma independiente del contexto. La mayoría de la estructura del programa se obtiene de este paso.

El _parser_ recibe un _stream_ de _tokens_, y retorna un _stream_ de nodos del CST. En este punto, un programa es una sucesión de nodos del CST.

Komodo es un lenguaje orientado a expresiones, lo que significa que hay una preferencia explicita a que las sentencias del lenguaje retornen un valor.

Komodo tiene algunas sentencias cuya interpretación más natural son como declaraciones, pero aún así retornan un valor, que usualmente es la tupla vacía `()`.

Esto hace que en el análisis sintáctico todo sea considerado una expresión. No se define una distinción entre declaraciones y expresiones.

El análizador sintáctico de Komodo es de descenso recursivo. Esto significa que está compuesto de funciones que se llaman mutuamente, donde (casi siempre) una función se encarga de procesar exclusivamente una de las expresiones del lenguaje.

De forma similar a como ocurre con el analizador léxico, el analizador sintáctico solo pasa por el _stream_ de _tokens_ una vez para analizar todo el programa. No es necesario hacer regresos a partes del _stream_ previamente recorridas.

#figure(
  grid(
    columns: 1,
    gutter: 4mm,
    {
      show regex("let"): set text(fill: red)
      show regex("x"): set text(fill: blue)
      show regex(":="): set text(fill: purple)
      show regex("2"): set text(fill: maroon)

      ```
      let x := 2
      ```
    },
    sym.arrow.b,
    {
      show regex("Let"): set text(fill: red)
      show regex("Ident\(x\)"): set text(fill: blue)
      show regex("Assign"): set text(fill: purple)
      show regex("Integer\(2\)"): set text(fill: maroon)

      ```
      Let, Ident(x), Assign, Integer(2)
      ```
    },
    sym.arrow.b,
    {
      show regex("Let"): set text(fill: red)
      show regex("Symbol\(x\)"): set text(fill: blue)
      show regex("Infix\(Assign\)"): set text(fill: purple)
      show regex("Integer\(2\)"): set text(fill: maroon)

      syntree(
        child-spacing: 2em, // default 1em
        layer-spacing: 3em, // default 2.3em
        "[`Let` [`Infix(Assign)` `Symbol(x)` `Integer(2)`]]"
      )
    },
  ),
  caption: [Ejemplo de paso de una sucesión de _tokens_ a un nodo de CST]
)

Para el análisis de expresiones infijas, el _parser_ usa el algoritmo de escalada de precedencia (_precedence climbing_ en Inglés). Este es un algoritmo iterativo que funciona bien dentro de un analizador de descenso recursivo, siendo más simple que algunas de las alternativas, como el algoritmo _Shunting Yard_. @precedenceclimbing

== Post-analizador sintáctico o _weeder_

El _weeder_ toma un nodo del CST y realiza dos tareas:

- Eliminar detalles innecesarios para la evaluación del código,
- Verificar condiciones del programa que serían más dificiles de verificar en etapas anteriores.

El resultado es un nodo de un árbol de sintaxis abstracto o AST (del inglés _Abstract Syntax Tree_), que no contiene detalles como la precedencia de operadores, espacios o indentación. También convierte ciertos operadores infijos en nodos más restringidos, para facilitar la evaluación y eliminar estados indeseables. El tipo de errores que el _weeder_ captura son de naturaleza sintáctica y en muchas ocasiones, dependientes del contexto.

A diferencia del _lexer_ y del _parser_, cuyas entradas son _streams_, la entrada del _weeder_ es un nodo individual del CST. Cuando un programa es analizado, el _weeder_ pasa por cada uno de los nodos retornados por el _parser_ de forma independiente.

La tarea del _weeder_ es reescribir los nodos del CST para convertirlos en nodos del AST. Este proceso puede fallar cuando el nodo de entrada no cumple características que el _weeder_ verifica. Por lo tanto, el _weeder_ puede retornar un nodo del AST o un error reportando la restricción que la entrada no cumple.

Otra de las razones para añadir el _weeder_ como una fase independiente en lugar de integrar sus funciones al _parser_, es controlar la complejidad del _parser_, que puede empezar a abarcar muchas reglas rápidamente. Al costo de aumentar el número de componentes y hacer al intérprete potencialmente más lento, se conserva la facilidad para entender y modificar el _parser_. Por esta razón, hay transformaciones que se realizan en el _weeder_ a pesar de que podrían realizarse en el _parser_ sin tanta dificultad.

El cambio de nodos del CST a nodos del AST también deja atrás información que ya no es relevante, como la precedencia de operadores. Esto crea barreras más rigidas entre las fases del intérprete, evitando que se acoplen @softwarevocab2010[p.~83] demasiado.

#figure(
  grid(
    columns: 1,
    gutter: 4mm,
    {
      show regex("let"): set text(fill: red)
      show regex("x"): set text(fill: blue)
      show regex(":="): set text(fill: purple)
      show regex("2"): set text(fill: maroon)

      ```
      let x := 2
      ```
    },
    sym.arrow.b,
    {
      show regex("let"): set text(fill: red)
      show regex("Ident\(x\)"): set text(fill: blue)
      show regex("Assign"): set text(fill: purple)
      show regex("Integer\(2\)"): set text(fill: maroon)

      ```
      let, Ident(x), Assign, Integer(2)
      ```
    },
    sym.arrow.b,
    {
      show regex("Let"): set text(fill: red)
      show regex("Symbol\(x\)"): set text(fill: blue)
      show regex("Infix\(Assign\)"): set text(fill: purple)
      show regex("Integer\(2\)"): set text(fill: maroon)

      syntree(
        child-spacing: 2em, // default 1em
        layer-spacing: 3em, // default 2.3em
        "[`Let` [`Infix(Assign)` `Symbol(x)` `Integer(2)`]]"
      )
    },

    sym.arrow.b,
    {
      show regex("Inmutable"): set text(fill: red)
      show regex("Symbol\(x\)"): set text(fill: blue)
      show regex("Declaration"): set text(fill: purple)
      show regex("Integer\(2\)"): set text(fill: maroon)

      syntree(
        child-spacing: 2em, // default 1em
        layer-spacing: 3em, // default 2.3em
        "[`Declaration(Inmutable)` `Symbol(x)` `Integer(2)`]"
      )
    },
  ),
  caption: [Ejemplo de paso de un nodo de CST a uno de AST]
)

= Ejecución de programas

== El modelo de ejecución

Un programa de Komodo está hecho de módulos de código. Un modulo de código es creado cada vez que:

- se ejecuta un archivo con código,
- se inicia una sesión del REPL.

Un archivo con código es ejecutado cuando el usuario lo solicita usando la interfaz de línea de comandos, o cuando es importado desde otro módulo.

Toda ejecución de un módulo de código tiene su propio entorno.

#figure(
  grid(
    columns: 1,
    gutter: 4mm,
    grid(
      columns: 2,
      gutter: 25mm,
      {
        ```
        # foo.komodo
        let foo(x) := x * x


        ```
      },
      {
        ```
        # bar.komodo
        let bar() := 5


        ```
      },
    ),
    sym.arrow.b,
    {
      ```
      from "./foo.komodo" import foo
      from "./bar.komodo" import bar
      let x := bar()
      println(foo(x))


      ```
    },
  ),
  caption: "Ejemplo de un programa de Komodo. Hay 3 módulos y 3 entornos.",
)

El que haya una correspondencia exacta entre entornos y módulos de código es conveniente para razonar fácilmente sobre los módulos: son unidades aisladas que se comunican entre si mediante la importación de variables.

=== Entornos

Un entorno está compuesto de una pila de _scopes_. Un _scope_ es una tabla que hace corresponder nombres con objetos.

El estado inicial de todo entorno tiene un _scope_, y siempre va a tener al menos un _scope_.

Se añade un nuevo _scope_ al entorno cada que:

- Se ejecuta una función,
- Se ejecuta un ciclo, 
- Se ejecuta un bloque de código indentado.

Después de ejecutar el código en cada uno de estos casos, el _scope_ es eliminado del entorno.

Cuando se ejecutan archivos con código, los entornos también guardan la ruta del archivo en el sistema de archivos local, y la ruta de la terminal donde fue ejecutado el intérprete.

=== Evaluador

Todos los módulos de código son ejecutados por separado. Para ejecutar un módulo de código, se ejecuta cada uno de los nodos del AST que lo componen, en orden, con el mismo entorno que va siendo potencialmente modificado tras cada ejecución. El estado inicial del entorno es el descrito previamente.

Esto hace que un entorno sea el único lugar donde se conserva el estado de ejecución de un módulo.

== Variables

Komodo permite la declaración de variables inmutables usando la palabra clave `let`. También se permite la creación de variables mutables con la palabra clave `var`.

#figure(
  ```
  var x := 5
  let y := a -> a + a * 2
  
  
  ```,
  caption: "Ejemplos de declaraciones en Komodo."
)

=== Resolución de nombres

Cuando un nombre es referenciado en el código, se busca en el entorno de la siguiente forma:

- Si se referencia para ser mutado (por ejemplo, al escribir `x := 5`), se comienza buscando desde el _scope_ al tope de la pila hasta el de más abajo. Se retorna la primera coincidencia encontrada. Si la variable no se encuentra en el _scope_ al tope, se interrumpe la búsqueda cuando se pasa por el scope generado por la ejecución de una función. Si la variable no es encontrada, también se interrumpe la ejecución con un error.

  Por ejemplo, en el siguiente ejemplo de código, la variable `x` es encontrada y modificada:

  #figure(
    ```
    var res := 0
    for i in 0..3 do
        res := res + i
    
    assert(res = 3)


    ```,
    caption: "Ejemplo de uso ordinario de una variable mutable."
  )

  Sin embargo, en este ejemplo el intérprete retorna un error:

  #figure(
    ```
    var res := 0
    let f() :=
        for i in 0..3 do
            res := res + i
    
    f()
    assert(res = 3)


    ```,
    caption: "Ejemplo de uso no permitido de una variable mutable."
  )

  En la implementación actual, el intérprete de Komodo comunica que la variable existe, pero que no puede ser mutada.

- Si se referencia una variable para obtener su valor (por ejemplo, al escribir `cur + 10`), se comienza buscando desde el _scope_ al tope de la pila hasta el de más abajo. Se retorna la primera coincidencia encontrada. Si la variable no es encontrada, se interrumpe la ejecución del programa con un error.

=== Copiado de valores

Komodo no tiene una noción de referencia. En el contexto de la dicotomía valor-referencia, en Komodo solo se manipulan valores. Esto hace que los detalles internos sobre referencias a valores y el copiado de valores sean invisibles al usuario.

El intérprete usa referencias siempre que es posible. Cuando una variable inmutable es asignada como el valor de otra variable inmutable, lo que se obtiene es una referencia a la variable original.

Sin embargo, siempre que esta variable haga parte de un cálculo o un procedimiento, se va a hacer una copia.

Cuando el valor de una variable es asignado a otra variable mutable, siempre se hace una copia.

Salvo por los tipos `Char` y `Bool`, la inicialización de todos los tipos de Komodo requieren la solicitud de memoria en tiempo de ejecución. Por esta razón, se prefiere la creación de referencias en lugar de crear copias.

=== Variables y tipos

En Komodo, los tipos están asociados a valores, y no a variables. Esto permite que una variable pueda ser declarada con un valor con cierto tipo, y luego se le pueda asignar otro valor, con otro tipo. Esto es conocido como tipado latente.

#figure(
  ```
  var x := 2
  x := "2"

  
  ```,
  caption: "Ejemplo de una variable mutada con distintos tipos."
)

=== Ocultamiento o _shadowing_ <shadowing>

Una variable puede ser declarada varias veces con el mismo nombre, incluso en el mismo _scope_. Esto se conoce como _shadowing_. Es una característica conveniente dada la tendencia del intérprete a funcionar con referencias, y es un medio para reciclar nombres en rutinas donde esto es útil.

Cabe destacar que el ocultar una variable con un nuevo valor no afecta los usos previos al ocultamiento. Un ejemplo de esto se muestra en el siguiente programa:

#figure(
  ```
  let f() := 1
  let g() := f()

  let f() := 2
  let h() := f()
  let f() := 3

  assert(g() = 1)
  assert(h() = 2)


  ```,
  caption: [Ejemplo de _shadowing_.],
)

Aquí, la función `g` retorna 1, que es el valor de la función `f` cuando `g` fue definida. Lo mismo sucede con la función `h`, que retorna 2. El hecho de que luego `f` retorne 3 no afecta a ningún uso previo.

=== Mutabilidad restringida

La mutabilidad de variables está restringida por dos reglas:

- Las entradas de las funciones son siempre inmutables. Por ejemplo, la asignación dentro de esta función no está permitida, pues hay una asignación a uno de los argumentos:

  #figure(
    ```
    let f(x) :=
        println(x)
        x := x - 1
    
    
    ```,
    caption: "Ejemplo de asignación ilegal a un argumento (siempre son inmutables)."
  )

- Dentro de una función, no se puede modificar el valor de una variable definida fuera de la función. Este es un ejemplo mínimo:

  #figure(
    ```
    var x := 0
    let f() :=
        x := 1
    

    ```,
    caption: "Ejemplo de asignación ilegal a una variable.",
  )

El propósito de estas reglas es restringir los casos de uso de un estado mutable.

== Importación de código

Komodo tiene sintaxis para importar módulos de código externos, ya sea de la librería estándar o de archivos con código del sistema de archivos local.

Por ejemplo, para importar funciones del módulo `utils` de la librería estándar, basta escribir

#figure(
  ```
  from utils import (map, reduce)
  (0..5)
      .map(a -> a*a)
      .reduce((acc, cur) -> acc + cur, 0)


  ```,
  caption: "Ejemplo de uso de la librería estándar."
)

Por otro lado, para importar código de un archivo local, hay que pasar una cadena con la ruta correspondiente, en lugar del nombre del módulo. Este es un ejemplo:

#figure(
  ```
  from "/tmp/foo.komodo" import VALUE
  println(VALUE)


  ```,
  caption: "Ejemplo de importación de código externo."
)

Komodo permite la importación de cualquier valor, no sólo funciones.

=== Comportamiento de las sentencias `import`

Las sentencias `from <module> import <values>` pueden ponerse en cualquier parte de un programa. Siempre retornan la tupla vacía `()`.

Este es el procedimiento que realizan:

- Se obtiene un entorno derivado del módulo solicitado.
  - Si el módulo solicitado corresponde a código de Komodo, todo este código es ejecutado, obteniendo un entorno.
  - Si el módulo solicitado no corresponde a código de Komodo, entonces debe ser un módulo de la librería estándar para el que se creó un entorno correspondiente previamente, que es retornado.

- Ya con el entorno, los nombres solicitados son obtenidos del mismo e introducidos en el scope al tope del entorno del módulo que se está ejecutando actualmente (en el que fue escrita la sentencia).

Nótese que una sentencia `import` puede ponerse en cualquier punto de un programa, por lo que puede afectar un _scope_ específico. En el siguiente ejemplo, se realiza una importación y solo se añaden los nombres importados al _scope_ donde se realizó la importación:

#figure(
  ```
  let f(x) :=
      from math import sqrt
      x + sqrt(x)
  
  sqrt(5) # error!


  ```,
  caption: [Importación en un _scope_ específico.]
)

En este caso se importó la función `sqrt` dentro del _scope_ de la función `f`, por lo que no afecta a los _scopes_ anteriores, y la función no será encontrada en estos.

Otra característica a destacar de la importación de módulos es que al importar elementos de un archivo con código, todo el archivo es ejecutado; independientemente de cuantos nombres se soliciten.

== Búsqueda de patrones o _Pattern matching_

La búsqueda de patrones es una forma de verificar propiedades en valores de Komodo. Por ejemplo, el siguiente programa busca patrones en una lista:

#figure(
  ```
  let len(list: List) :=
      case list do
        [] => 0
        [_|tail] => 1 + len(tail)
  

  ```,
  caption: "Ejemplo de búsqueda de patrones en Komodo.",
)

En este ejemplo hay una lista de parejas, que hacen corresponder patrones y resultados.

El primer patrón expresa que si la lista referenciada por `list` esta vacía, su longitud es 0.

El segundo dice que si la lista está compuesta de un elemento al principio y otra lista con los demás, referenciada con `tail`, su longitud es de 1 más la longitud de `tail`.

La búsqueda de patrones permite describir procedimientos como listas de reglas. También permite usar la estructura de un valor para obtener otros valores de su interior. Vamos a describir estos casos de uso.

=== Descripción de procedimientos

Como se mostró en el ejemplo anterior, se pueden describir procedimientos con patrones. Komodo tiene dos mecanismos para hacer esto.

==== Patrones en funciones

Las funciones en Komodo pueden definirse en varias declaraciones separadas, donde se pueden poner patrones diferentes. Por ejemplo, esta es una forma de definir la función de Fibonacci:

#figure(
  ```
  let fib(0) := 0
  let fib(1) := 1
  let fib(n) := fib(n - 1) + fib(n - 2)


  ```,
  caption: "Función de Fibonacci en Komodo."
)

Los parámetros de la función pueden escribirse como patrones, que cuando la función sea llamada, serán comparados con los argumentos en orden. El resultado asociado a la primera lista de patrones que sea compatible con los argumentos será el resultado de la llamada. Sin ningún patrón es compatible, el programa se detiene con un error.

==== Expresiones `case`

No es necesario usar funciones para escribir procedimientos con patrones. Se puede usar una expresión `case`:

#figure(
  ```
  case x % 2 do
      0 => "x es par"
      1 => "x es impar"


  ```,
  caption: [Ejemplo de uso de una expresión `case`.],
)

El comportamiento es el mismo: los patrones son comparados con la expresión en orden, y el primer patrón compatible determina el resultado. Si ningún patrón es compatible, el programa se detiene con un error.

=== Desestructuración

Se pueden usar patrones en definiciones para extraer valores del interior de otros valores:

#figure(
  ```
  let coordinates(n) := (n + 1, n * 2)
  let (x, y) := coordinates(0)
  assert(x = 1)
  assert(y = 0)


  ```,
  caption: "Ejemplo de desestructuración en Komodo.",
)

En este ejemplo, se compara el patrón a la izquierda de la asignación con el valor de `coordinates(0)`. En este caso el patrón es compatible: el valor es `(1, 0)`. Así, se asigna a `x` el valor `1` y a `y` el valor `0`.

Cuando el patrón no es compatible, el programa se detiene con un error.

También se pueden desestructurar valores en ciclos `for`:

#figure(
  ```
  from utils import map
  let coordinates(n) := (n + 1, n * 2)
  for (x, y) in (0..5).map(coordinates) do println(x + y)


  ```,
  caption: [Ejemplo de desestructuración en un ciclo `for`.],
)

El comportamiento es el mismo que se da cuando se hace una declaración, solo que se repite al principio de cada iteración.

== Tipos

Komodo es un lenguaje con tipado latente, gradual y dinámico. Esto hace considerar fácilmente a Komodo como un lenguaje de tipado débil. Describamos estas características.

=== Latente

Los tipos de Komodo no están asociados a símbolos, sino a valores. La principal consecuencia de esto es que un símbolo puede tener valores de distintos tipos en momentos diferentes. Esto es útil para la reutilización de nombres, por ejemplo.

=== Gradual

Se pueden añadir chequeos de tipos a un programa de Komodo de manera opcional, y a conveniencia. Estos chequeos se realizan en tiempo de compilación. Por ejemplo, en este programa hay un chequeo de tipos:

#figure(
  ```
  let first(list: List) := list[0]


  ```,
  caption: "Chequeo de tipos en Komodo."
)

Nótese que hay patrones que realizan chequeos de tipos implicitos:

#figure(
  ```
  let some({res|_}: Set) := res


  ```,
  caption: "Chequeo de tipos redundante."
)

En este ejemplo, verificar que la entrada es de tipo `Set` es redundante, pues el patrón `{res|_}` solo es compatible con valores de tipo `Set`. Bastaría con escribir la función así:

#figure(
  ```
  let some({res|_}) := res


  ```,
  caption: "Chequeo de tipos implícito."
)

=== Dinámico

Todos los chequeos de tipos en programas de Komodo se realizan en tiempo de ejecución. Esto hace que la implementación de reglas de tipado débil sea más sencilla, con la consecuencia de que deben realizarse más chequeos en tiempo de ejecución.

=== Los tipos incorporados

Komodo viene con tipos incorporados que facilitan la creación de procedimientos básicos, y son herramientas que se esperan en cualquier lenguaje de programación de propósito general. Sin embargo, la elección de los tipos incorporados de Komodo refleja sus preferencias de uso.

==== La tupla vacía <empty_tuple>

Está representada por `()`. Es en la práctica el tipo nulo de Komodo.

Es importante recalcar que la tupla vacía no es un tipo separado (como sucede con el tipo unitario en lenguajes como Haskell o Rust), sino que realmente el intérprete lo considera una tupla sin valores. Este es simplemente un atajo para que `()` no tenga un tipo exclusivo. Esta característica hace a `()` más cercano a un tipo nulo, típico de los lenguajes de programación imperativos; que a un tipo unitario, típico de los lenguajes de programación funcionales.

La tupla vacía es un patrón que se puede rastrear:

#figure(
  ```
  let isNull(()) := true
  let isNull(_) := false
  

  ```,
  caption: [_Pattern matching_ con la tupla vacía.]
)

También puede usarse el operador de igualdad:

#figure(
  ```
  let isNull(val) := val = ()


  ```,
  caption: [Comparación con la tupla vacía.]
)

Este ejemplo muestra como realmente `()` es representado como una tupla:

#figure(
  ```
  let isTuple(_: Tuple) := true
  let isTuple(_) := false

  assert(().isTuple())


  ```,
  caption: [Tipo de la tupla vacía.]
)

==== Números

Komodo tiene tres representaciones para números: Enteros, flotantes y fracciones. Todos tienen tamaño arbitrario, que crece bajo demanda. El intérprete usa las librerías GMP y MPFR, que hacen parte del proyecto GNU y están diseñadas para funcionar juntas.

===== Enteros

Los enteros tienen signo y tienen las operaciones de suma, resta, multiplicación, división, residuo, exponenciación y desplazamiento de bits, tanto a la izquierda como a la derecha. Los bits más significativos están a la izquierda. Son representados en tiempo de ejecución como arreglos dinámicos de enteros de longitud de la palabra de máquina de ejecución. El signo va por separado.

La generación de enteros de Komodo requiere, en general, de solicitar memoria en tiempo de ejecución. Este es un proceso costoso en términos de tiempo.

Se pueden escribir constantes en base 2, 8, 10 y 16. No hay diferencia entre dos enteros que representan la misma magnitud, independientemente de la base en que fueron escritos.

#figure(
  ```
  let eights := {
      0b1000,
      0o10,
      8,
      0x8,
  }

  assert(eights = {8})


  ```,
  caption: "Enteros de Komodo."
)

La implementación de los enteros es traída de la librería GMP. @gmp

===== Números de punto flotante

Los números de punto flotante de Komodo son una extensión de los descritos por el estándar IEEE 754, con las siguientes diferencias:

- El tamaño de la mantisa puede ser mayor que 53 bits.
- El tamaño de la mantisa puede variar entre diferentes instancias de los números.
- El tamaño de la mantisa se decide en el momento que un número es instanciado.

Puesto que la representación de estos números es binaria, viene con las características típicas de los números de punto flotante de máquina. En particular, no todos los números decimales son representables por estos números. Este es un ejemplo común:

#figure(
  ```
  let x := 0.1


  ```,
  caption: "Números de punto flotante en Komodo."
)

En este caso, `x` tiene un redondeo muy cercano a 0.1, pero no es exactamente 0.1, por el hecho de que 0.1 no puede ser representado con un mantisa y un exponente binarios.

La generación de flotantes requiere de la solicitud de memoria en tiempo de ejecución.

La implementación de los números de punto flotante es traída de la librería MPFR @mpfr, que es una extensión de la librería GMP.

===== Fracciones

Las fracciones tienen signo y tienen las operaciones de suma, multiplicación, división y exponenciación. Son representados como un par de enteros de longitud arbitraria, por lo que se pueden realizar operaciones con números arbitrariamente grandes o pequeños.

La utilidad de las fracciones viene cuando es necesario hacer operaciones sin redondeos, con el costo de menor velocidad. Las fracciones pueden representar todos los números que los enteros y los flotantes pueden representar, y más.

Se escriben con dos barras inclinadas:

#figure(
  ```
  let a := 5
  let b := 1 // 5

  assert(a * b = 1)
  ```,
  caption: "Fracciones de Komodo."
)

La generación de fracciones también requiere de la solicitud de memoria en tiempo de ejecución.

La implementación de las fracciones es traída de la librería GMP.

==== Funciones

Las funciones de Komodo pueden escribirse de dos formas:

- De forma anónima, como una lista de parámetros y un bloque de código:
  
  #figure(
    ```
    (a, b) -> a + b - 5
    
    
    ```,
    caption: "Función anónima de Komodo."
  )

  Estas funciones son expresiones, así que pueden ser puestas dentro de contenedores o ser guardadas como variables.

- con nombre, como una lista de parejas patrón-código:

  #figure(
    ```
    let f(0, _) := 0
    let f(_, 0) := 0
    let f(a, b) := a + b - 5


    ```,
    caption: "Función nombrada de Komodo."
  )

  Las funciones nombradas son siempre inmutables, así que no pueden crearse con la palabra clave `var`. Ejecutar esta pieza de código retorna un error:

  #figure(
    ```
    var f(x) := 2*x
    
    
    ```,
    caption: "Declaración ilegal de una función."
  )

Todas las funciones de Komodo pueden ser pasadas como argumentos de otras funciones.

Los _scopes_ de Komodo son creados de forma léxica, lo que significa que los nombres referenciados en la función son los que se obtienen en el contexto de la definición de la función, y no en el contexto de sus ejecuciones. Usemos como ejemplo el siguiente fragmento de código:

#figure(
  ```
  let a := 2
  let func := () -> a

  for i in 0..5 do
      let a := i
      assert(func() = 2)
  

  ```,
  caption: [_Scope_ léxico en Komodo.],
)

En este caso, a pesar de que la `func` es ejecutada en un _scope_ donde el valor de `a` varía, siempre usa el valor que `a` tenía cuando fue definida. Esta regla limita la forma en que se puede interpretar una llamada a una función, lo que puede ser conveniente al analizarla.

==== Caracteres y cadenas

Komodo, a diferencia de muchos lenguajes de _scripting_, tiene tipos separados para representar caracteres y cadenas. Esto puede ser útil a la hora de iterar sobre cadenas y de hacer tratamiento minucioso de cadenas. Los dos tipos operan juntos, y se realizan conversiones implícitas entre ellos cuando es conveniente.

===== Caracteres

Los caracteres de Komodo son valores escalares de Unicode, por lo que pueden representar cualquier símbolo Unicode. Tienen una longitud fija de 32 bits.

Su sintaxis es muy similar a la de las cadenas, sólo que usa comillas simples:

#figure(
  ```
  let a := 'a'

  ```,
  caption: "Declaración de un caracter en Komodo."
)

Todos los caracteres son patrones que pueden ser rastreados, y también se puede restringir la entrada de una función por su tipo:

#figure(
  ```
  let isAnA('a' || 'A') := true
  let isAnA(_) := false

  let isChar(_: Char) := true
  let isChar(_) := false

  assert(isAnA('a'))
  assert(isChar('b'))


  ```,
  caption: [_Patttern mathing_ de caracteres.]
)

Los caracteres pueden ser sumados entre si para sumar cadenas, y pueden ser sumados con cadenas para producir otras cadenas. También pueden ser multiplicados por un entero para concatenarse a si mismas varias veces:

#figure(
  ```
  assert('a'+'b'="ab") # Char + Char
  assert('a'+"abc"="abc") # Char + String
  assert('z'*3="zzz") # Char "multiplicado"


  ```,
  caption: "Operaciones con caracteres en Komodo."
)

===== Cadenas

Las cadenas de Komodo están representadas como arreglos inmutables de bytes, que están codificados con UTF-8.

Se puede iterar de izquierda a derecha sobre las cadenas de Komodo de la misma forma que se hace con las listas. Este es un detalle importante y que puede ser confuso. El patrón `[first|tail]` (o patrón _cons_) es compatible con listas y cadenas. Veamos un ejemplo:

#figure(
  ```
  let length([] || "") := 0
  let length([_|tail]) := 1 + len(tail)

  assert(length([1, 2]) = length("ab"))

  ```,
  caption: [Patrón _cons_ para listas y cadenas.]
)

En este ejemplo, se muestra que para que la función `length` funcione para listas y cadenas, el patrón `[] || ""` debe usarse, y así tener en cuenta ambos casos. Sin embargo, el patrón `[_|tail]` funciona para cadenas y listas por igual. Esto hace que la compatibilidad con el patrón _cons_ no garantice que el argumento pasado sea una lista. En efecto, podría ser una lista o una cadena de caracteres.

Además, nótese que una lista de caracteres es diferente a una cadena:

#figure(
  ```
  assert(['a', 'b'] /= "ab")


  ```,
  caption: "Diferencia entre cadenas y listas de caracteres."
)

La diferencia entre cadenas y listas de caracteres es una característica traída de otros lenguajes como un detalle de implementación, pero conflictúa con la preferencia de Komodo de entender a los datos con la menor cantidad de detalles de implementación posible.

==== Contenedores

Los contenedores almacenan otros valores, incluyendo los de su mismo tipo. Todos los contenedores de Komodo permiten almacenar valores de diferente tipo en el mismo contenedor simultáneamente.

===== Tuplas

Las tuplas son colecciones ordenadas de valores, que no crecen. Su propósito es juntar valores. Pueden escribirse como valores separados por comas, rodeados por paréntesis.

#figure(
  ```
  (5, "cinco", (a) -> a + 5)


  ```,
  caption: "Tuplas de Komodo.",
)

Sin las tuplas, quedarían dos soluciones para tener valores compuestos:

- Usar un diccionario: Esta solución está bien, pero puede ser demasiado complicada para algunos problemas. Además, puede operarse con otros diccionarios, lo cual puede ser indeseable.

- Usar una lista: Es una solución muy similar, pero sigue estando el problema de que pueden ser operadas con otras listas, lo cual puede ser indeseable.

Estas dos soluciones usan tipos con un propósito muy claro, y estarían siendo usadas de manera ligeramente distinta. La mayor utilidad de las tuplas es declarar la intención de que los datos en ellas deberían estar juntos.

La tupla vacía, mencionada al principio de esta sección, es una tupla y no un tipo por separado. (véase @empty_tuple)

===== Listas

Las listas de Komodo son de longitud arbitraria.

Se puede acceder a sus elementos de tres formas:

- Con índices enteros indexados desde cero, usando la notación `list[index]`. Esto es útil para escribir procedimientos iterativos que involucran el orden en que se encuentran los elementos, y se accede a múltiples partes de la lista en un mismo paso:

  #figure(
    ```
    let reverse(l: List) :=
        var res := l
        for i in 0..(len(l)/2) do
            res[i] := l[len(l)-i-1]
            res[len(l)-i-1] := l[i]
        
        res

    ```,
    caption: "Reverso de una lista en Komodo."
  )

  El acceso por índice a un índice ilegal (negativo o, mayor o igual que la longitud de la lista) hace que el programa sea interrumpido con un error.


- Iterando sobre la lista de izquierda a derecha, con la notación `[first|tail]`. Esto funciona bien para la mayoría de casos de uso, y permite la escritura sencilla de procedimientos recursivos:

  #figure(
    ```
    let max(a, b) := if a > b then a else b
    let max([val]) := val
    let max([first|tail]: List) := max(first, max(tail))


    ```,
    caption: "Máximo de una lista en Komodo"
  )

- Iterando sobre la lista con expresiones por comprensión o en ciclos:

  #figure(
    ```
    let list := [1, 2, 1, 2]

    let set := {val for val in list}
    assert(set = {1, 2})

    var acc := 0
    for val in list do
        acc := acc + val
    assert(acc = 6)


    ```,
    caption: "Iteración sobre listas.",
  )

El intérprete las almacena como arreglos dinámicos. Esta es una representación conveniente para minimizar la solicitud de memoria en tiempo de ejecución y para la velocidad del acceso por índice, pero no tanto para la creación de sublistas obtenidas de la lista donde se itera.

===== Conjuntos

Los conjuntos de Komodo son de longitud arbitraria. Se puede iterar sobre ellos y verificar la pertenencia de elementos.

Están representados como árboles binarios de búsqueda.

Los conjuntos tienen su propia sintaxis, y pueden ser escritos por extensión o por comprensión:

#figure(
  ```
  let A := {1, 2, 4, 8, 16} # por extensión
  let B := {2**k for k in 0..5} # por comprensión

  assert(A = B)


  ```,
  caption: "Conjuntos de Komodo.",
)

Se puede iterar sobre sus elementos de varias maneras:

- Usando la notación _cons_ para conjuntos:

  #figure(
    ```
    let prod({}) := 1
    let prod({some|rest}) := some * prod(rest)


    ```,
    caption: [Notación _cons_ para conjuntos.]
  )

  Esta notación funciona de la misma forma que la notación _cons_ de listas.

  La implementación actual garantiza que los elementos son recorridos en orden, pero esta característica podría cambiar.

- Usándolo como iterador en contenedores por comprensión y ciclos:

  #figure(
    ```
    let set := {1, 2, 2}

    var list := []
    for val in set do
        list := [val|list]
    assert(list = [1, 2] || list = [2, 1])

    let list := [val + 1 for val in set]
    assert(list = [2, 3] || list = [3, 2])


    ```,
    caption: "Iteración sobre conjuntos.",
  )

Para verificar que un elemento pertenece a un conjunto, puede usarse el operador `in`:

#figure(
  ```
  let A := {1, 2}
  assert(1 in A)


  ```,
  caption: "Pertenencia de conjuntos.",
)

Los conjuntos también pueden verificar contenencia e igualdad entre ellos, y se tienen las operaciones de unión y diferencia:

#figure(
  ```
  let A := {1, 2}
  let B := {2, 3}

  assert(A + B = {1, 2, 3})
  assert(A - B = {1})
  assert(A - B < A) # contenencia estricta
  assert(A <= A) # contenencia o igualdad


  ```,
  caption: "Operaciones entre conjuntos.",
)

Los conjuntos pueden ser desestructurados y rastreados con patrones:

#figure(
  ```
  let {a, b} := {1, 2}
  assert(a + b = 3)


  ```,
  caption: "Desestructuración de conjuntos.",
)

La razón de que los conjuntos sean estructuras de primera clase es evitar que el usuario los implemente incidentalmente como parte de la implementación de ciertas rutinas. Esta situación es muy común en el tipo de problemas a los que Komodo apela.

===== Diccionarios - Objetos

Los diccionarios de Komodo son de longitud arbitraria. Son colecciones de parejas clave-valor, donde el tipo de ambos es arbitrario.

Los diccionarios deben ser inicializados con al menos un elemento, pues la expresión `{}` genera un conjunto:

#figure(
  ```
  let set := {} # conjunto
  let dict := { () => () } # diccionario


  ```,
  caption: "Construcción de diccionarios.",
)

Se puede acceder a sus elementos de dos formas:

- Notación de objeto: `objeto.clave`, donde `objeto` es un diccionario y `clave` es interpretado como una cadena, que es buscada en el diccionario.

  Esto es equivalente a escribir `objeto["clave"]`. Aunque confusa, esta notación es una facilidad para usar los diccionarios de una forma muy particular cuando la situación lo amerita.

  Este es un ejemplo:

  #figure(
    ```
    var data := {
        "values" => [1, 2, 3],
        "length" => 3,
    }

    assert(data.length = 3)
    assert(data.values = [1, 2, 3])

    data.values := [val + 1 for val in data.values]
    assert(data.values = [2, 3, 4])


    ```,
    caption: "Diccionarios como estructuras.",
  )


- Notación usual: `dic[clave]` donde `dic` es un diccionario y `clave` es un valor arbitrario.

  Esta notación permite usar cualquier valor de Komodo como una clave. Por ejemplo, aquí usamos listas y conjuntos como claves:

  #figure(
    ```
    let dict := {
      [[1], [2]] => 3,
      {2, 3, 4} => 9,
    }

    assert(dict[[[1], [2]]] = 3)
    assert(dict[{2, 3, 4}] = 9)


    ```,
    caption: "Diccionarios con claves arbitrarias.",
  )

La búsqueda de una clave que no se encuentra en un diccionario interrumpe el programa con un error.

En la implementación actual, no se puede iterar sobre diccionarios. Sin embargo, si pueden ser buscados con patrones:

#figure(
  ```
  let dict := {
      [[1], [2]] => 3,
      {2, 3, 4} => 9,
  }

  let f({{2, 3, 4} => x, ..}) := x
  assert(f(dict) = 9)
  

  ```,
  caption: "Patrones con diccionarios.",
)

Los diccionarios están representados como árboles binarios de búsqueda, igual que los conjuntos. Esto podría cambiar en el futuro.

== El intérprete

El intérprete de Komodo es un binario compilado estáticamente. Además de la interfaz del sistema operativo, el intérprete no tiene dependencias en tiempo de ejecución.

El intérprete está escrito en el lenguaje de programación Rust. @rust El ecosistema de Rust, de manera similar a lenguajes como OCaml, @ocaml es favorable para construir herramientas para lenguajes de programación. El modelo de memoria de Rust no incluye manejo de memoria automático, sino un sistema que permite verificar reglas que garantizan seguridad de memoria en tiempo de compilación.

== Gestión de memoria

El intérprete gestiona la memoria automáticamente con un algoritmo de _mark-and-sweep_ sin adiciones. El espacio de memoria crece a demanda en tiempo de ejecución.

== Interacción con el sistema

Hasta ahora, Komodo sólo interactúa directamente con la entrada y salida estándar, e indirectamente con la importación de código.

= Aspectos periféricos

Hay software adicional al intérprete que lo asiste o extiende su alcance.

== Editor web

Una compilación del interprete a _WebAssembly_ @wasm es usada para interactuar poder usar el intérprete en navegadores de Internet. Es una versión sin la librería estándar y con una interfaz simulada de la entrada y salida estándar.

== Resaltado de sintaxis

Se escribió una gramática de _TextMate_ @textmate para los _tokens_ de Komodo, y así obtener resaltado de sintaxis en los editores de texto compatibles.

#bibliography("ref.bib", title: "Referencias")
